train_model <- function(df, features, response, seed = 33, scale_responses=F){
  library(brms)
  #scale data so that the bayesinf model can use data centered in zero
  scaling_xs = scale(df[,colnames(df) %in% features])
  df[,colnames(df) %in% colnames(scaling_xs)] = scaling_xs
  
  # if(scale_responses == T){
  #   scaling_ys = scale(df[,colnames(df) %in% response])
  #   df[,colnames(df) %in% response] = scaling_ys
  # }
  set.seed(seed)
  train = df %>% ungroup %>% group_by(Site) %>% sample_frac(0.7)
  test = df %>% filter(!(PlotCode %in% train$PlotCode))
  if(length(response) > 1){
    #train model using bayesian lasso (horseshoe priors) to identify which indexes and bands can be reduced
    fit = brm(paste("mvbind(", paste(paste("log10(", response, ")", sep=""), collapse = ", "),
                    ") ~ ", paste(paste("(", features, ")"), collapse = " + ")
                    , 
                    #add site effect
                    #"+ (1|Site)"
    ),
    # define data and grouping correlation structures
    data = train, family = student(),
    set_prior(horseshoe()),
    #other parameters
    chains=2, cores =2, iter = 2000)#, backend = "cmdstanr", threads = threading(2))
  }else{
    if(features[1] %like% "nm_"){
      #train model using bayesian lasso (horseshoe priors) to identify which indexes and bands can be reduced
      fit = brm(paste(paste(paste("log10(", response, ")", sep=""), collapse = ", "),
                      " ~ ", paste(paste("(", features, ")"), collapse = " + ") 
                      #add site effect
                      #,"+ (1|Site)"
      ),
      # define data and grouping correlation structures
      data = train, family = student(),
      set_prior(horseshoe()),
      #other parameters
      chains=2, cores =2, iter = 2000 
      #,backend = "cmdstanr", threads = threading(1)
      )
    }else{
      fit = brm(paste(paste(paste("log10(", response, ")", sep=""), collapse = ", "),
                      " ~ ", paste(paste("(", features, ")"), collapse = " + ") 
                      #add site effect
                      #,"+ (1|Site)"
      ),
      # define data and grouping correlation structures
      data = train, family = student(),
      #other parameters
      chains=2, cores =2, iter = 2000 
      )
    }
  }
  #calculate the waic of the model
  #loo_fit = loo(fit)
  #calculate the bayesian R2 for the model on the test set
  bR2_fit = bayes_R2(fit, newdata=test)
  #perform cross validation, predict and calculate rmse on the original scale of y
  #kcv <- kfold(fit, K = 3, seed=seed, save_fits = TRUE)
  predicts <- predict(fit,  newdata = test)
  y = test %>% select(response)
  # if(scale_responses == T){
  #   y[[response]] = y[[response]] * attr(scaling_ys,"scaled:scale")[[response]]+
  #     attr(scaling_ys,"scaled:center")[[response]]
  #   predicts = predicts * attr(scaling_ys,"scaled:scale")[[response]]+
  #     attr(scaling_ys,"scaled:center")[[response]]
  # }
  # define a loss function
  rmse <- function(y, yrep) {
    yrep_mean <- mean(yrep)
    sqrt(mean((yrep_mean - y)^2))
  }
  predicts = 10**predicts %>% data.frame
  rmse_ft = rmse(y[[response]] , (predicts[["Estimate"]]))
  #rmse(y, colMeans(predicts))
  #return a list with model, metrics, and scaling factors
  return(list(model = fit, bR2 = bR2_fit, #LOO = loo_fit, RMSE = rmse_ft, 
              y_hat_test = predicts, y_test = y,
              scaling = list(features = scaling_xs)))
  
}

#path_predictors = "~/Dropbox (UFL)/RMBL_2018/Data_for_Sergio/NEON_refl_VI_plot_cl_brnor_500_2400_adaptive.csv"
#path_responses = "~/Dropbox (UFL)/RMBL_2018/Data_for_Sergio/Abiotic_var_fluxes_traits_field.csv"
generate_augmented_matrix <- function(path_responses, path_predictors, dim_redution =T){
  library(data.table)
  library(tidyverse)
  # want to generate on the go the dataset to feed the models into
  X = fread(path_predictors, header=T)
  if(dim_redution ==T & path_predictors %like% "reflectance"){
    if(path_predictors %like% "canopy"){
      X_KD = fread("./kld_canopy.csv", header=T, drop = 1)
    }else if(path_predictors %like% "NEON"){
      X_KD = fread("./kld_NEON.csv", header=T, drop = 1)
    }else if(path_predictors %like% "leaf"){
      X_KD = fread("./kld_leaf.csv", header=T, drop = 1)
    }
    X = cbind.data.frame(X[["Site"]], X_KD)
    colnames(X)[1] = "Site"
  }
  colnames(X)
  
  #get the average predictors in plot
  X = X %>% group_by(PlotCode, Site) %>% summarize_if(is.numeric, mean)
  X = X %>% ungroup %>%select(-one_of(c("Site", "Year")))
  if(path_predictors %like% "reflectance"){
    colnames(X)[-1]=paste("nm", colnames(X)[-1], sep="_")
    
  }
  indexes = sapply(1:ncol(X), function(x) is.numeric(data.frame(X)[,x]))
  indexes = colnames(X)[indexes]
  # read responses
  Y = fread(path_responses)
  #drop empty rows
  Y = Y[complete.cases(Y),]
  
  #remove site from X if present
  dataset = inner_join(X, Y, by=c("PlotCode"))
  return(list(dataset = dataset, numeric_predictors = indexes))
}
