#read dataset
library(tidyverse)
library(data.table)
library(GGally)
#load data for NEON spectra and indexes
path_predictors = "/Volumes/Data/Data_for_Sergio/"
predictors = list.files(path_predictors) 
predictors = predictors[!predictors %like% "ecosystem_fluxes"]
path_responses = "/Volumes/Data/Data_for_Sergio/ecosystem_fluxes.csv"
br2_overview=list()
for(prdct in predictors){
  df = generate_augmented_matrix(path_predictors=paste(path_predictors, prdct,sep=""),
                                 path_responses=path_responses)
  
  ecosystem_fluxes = c("NEE","GPP","Reco","Rsoil","SOC","ET","WUE")
  features = c(df$numeric_predictors)
  #plot responses correlation and distribution
  # df$dataset %>% ungroup %>% select(ecosystem_fluxes) %>%
  #   mutate_all(log10)%>%
  #   ggpairs(aes(alpha = 0.4))
  
  #it seems that responses have a lognormal distribution. will use that for the formula
  #joint model
  #joint_model = train_model(df, response = ecosystem_fluxes, features = indexes)
  
  #individual models
  df = df$dataset
  all_models = list()
  for(fl in ecosystem_fluxes){
    all_models[[fl]]=train_model(df, features, fl, seed = 33, scale_responses=F)
  }
  br2_ = list()
  for(fl in ecosystem_fluxes){
    br2_[[fl]] = (all_models[[fl]]$bR2)
  }
  br2_overview[[prdct]] = do.call(rbind.data.frame, br2_)
  saveRDS(all_models,paste("./out/", substr(prdct, 1, nchar(prdct)-4), ".rds", sep=""))
}
