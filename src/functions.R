train_model <- function(df, features, response, seed = 1987, scale_responses=F){
  library(brms)
  #scale data so that the bayesinf model can use data centered in zero
  scaling_xs = scale(df[,colnames(df) %in% features])
  df[,colnames(df) %in% features] = scaling_xs
  
  if(scale_responses == T){
    scaling_ys = scale(df[,colnames(df) %in% response])
    df[,colnames(df) %in% response] = scaling_ys
  }
  set.seed(seed)
  train = df %>% ungroup %>% sample_frac(0.8)
  test = df %>% filter(!(PlotCode %in% train$PlotCode))
  if(length(response) > 1){
    #train model using bayesian lasso (horseshoe priors) to identify which indexes and bands can be reduced
    fit = brm(paste("mvbind(", paste(paste("log10(", response, ")", sep=""), collapse = ", "),
                    ") ~ ", paste(paste("(", features_name, ")"), collapse = " + ")),
              # define data and grouping correlation structures
              data = train, family = student(),
              set_prior(horseshoe()),
              #other parameters
              chains=2, cores =2, iter = 2000)#, backend = "cmdstanr", threads = threading(2))
  }else{
    #train model using bayesian lasso (horseshoe priors) to identify which indexes and bands can be reduced
    fit = brm(paste(paste(paste("log10(", response, ")", sep=""), collapse = ", "),
                    " ~ ", paste(paste("s(", features_name, ")"), collapse = " + ")),
              # define data and grouping correlation structures
              data = train, family = student(),
              #set_prior(horseshoe()),
              #other parameters
              chains=2, cores =2, iter = 2000)#, backend = "cmdstanr", threads = threading(2))
  }
  #calculate the waic of the model
  waic_fit = waic(fit)
  #calculate the bayesian R2 for the model on the test set
  bR2_fit = bayes_R2(fit, newdata=test)
  #perform cross validation, predict and calculate rmse on the original scale of y
  kcv <- kfold(fit, K = 3, seed=seed, save_fits = TRUE)
  predicts <- predict(kcv, seed=seed, newdata = test)
  y = test %>% select(response)
  y[[response]] = y[[response]] * attr(scaling_ys,"scaled:scale")[[response]]+
    attr(scaling_ys,"scaled:center")[[response]]
  predicts = predicts * attr(scaling_ys,"scaled:scale")[[response]]+
    attr(scaling_ys,"scaled:center")[[response]]
  
  # define a loss function
  rmse <- function(y, yrep) {
    yrep_mean <- mean(yrep)
    sqrt(mean((yrep_mean - y)^2))
  }
  predicts = predicts %>% data.frame
  rmse(y[[response]] , (predicts[["Estimate"]]))
  #rmse(y, colMeans(predicts))
  #return a list with model, metrics, and scaling factors
  return(list(bR2 = bR2_fit, WAIC = waic_fit, RMSE = rmse, 
              scaling = list(responses= scaling_ys, features = scaling_xs)))
  
}

path_predictors = "~/Dropbox (UFL)/RMBL_2018/Data_for_Sergio/NEON_refl_VI_plot_cl_brnor_500_2400_adaptive.csv"
path_responses = "~/Dropbox (UFL)/RMBL_2018/Data_for_Sergio/Abiotic_var_fluxes_traits_field.csv"
generate_augmented_matrix <- function(path_responses, path_predictors){
  library(data.table)
  library(tidyverse)
  # want to generate on the go the dataset to feed the models into
  X = fread(path_predictors)
  #get the average predictors in plot
  X = X %>% group_by(PlotCode, Site, Spec_type) %>% summarize_if(is.numeric, mean)
  # read responses
  Y = fread(path_responses)
  #drop empty rows
  Y = Y[complete.cases(Y),]
  dataset = inner_join(X, Y, by="PlotCode")
  return(dataset)
}
