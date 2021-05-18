#read dataset
library(tidyverse)
library(data.table)
library(GGally)
#load data for NEON spectra and indexes
path_predictors = "/Volumes/Data/Data_for_Sergio/"
predictors = list.files(path_predictors) 
predictors = predictors[!predictors %like% "ecosystem_fluxes"]
path_responses = "/Volumes/Data/Data_for_Sergio/ecosystem_fluxes.csv"
#create an empty list to store results for each model
br2_overview=list()
for(prdct in predictors[-1]){
  #retrieve specific data for specific model
  df = generate_augmented_matrix(path_predictors=paste(path_predictors, prdct,sep=""),
                                 path_responses=path_responses)
  #fixed responses
  ecosystem_fluxes = c("NEE","GPP","Reco","Rsoil","SOC","ET","WUE")
  # separate Rao and CV is the model is spectral diversity
  features = c(df$numeric_predictors)
  if(prdct %like% "spectra_diversity"){
    for(prdct in c("CV", "Rao")){
      features = features[!features %like% "sd"]
      features = prdct
      #individual models
      dataset = df$dataset
      all_models = list()
      for(fl in ecosystem_fluxes){
        all_models[[fl]]=train_model(dataset, features, fl, seed = 33, scale_responses=F)
      }
      br2_ = list()
      for(fl in ecosystem_fluxes){
        br2_[[fl]] = (all_models[[fl]]$bR2)
      }
      br2_overview[[prdct]] = do.call(rbind.data.frame, br2_)
      saveRDS(all_models,paste("./out/loo_mixed_eff", substr(prdct, 1, nchar(prdct)), ".rds", sep=""))
      #for anything else than spectral diversity add all features that are in the file and apply to the specific model
    }else{
      features = features[!features %like% "sd"]
      #features = prdct
      #individual models
      dataset = df$dataset
      all_models = list()
      for(fl in ecosystem_fluxes){
        all_models[[fl]]=train_model(dataset, features, fl, seed = 33, scale_responses=F)
      }
      br2_ = list()
      for(fl in ecosystem_fluxes){
        br2_[[fl]] = (all_models[[fl]]$bR2)
      }
      br2_overview[[prdct]] = do.call(rbind.data.frame, br2_)
      saveRDS(all_models,paste("./out/NEON_for_maps_", substr(prdct, 1, nchar(prdct)), ".rds", sep=""))
    }
  }
  
}
saveRDS(br2_overview,paste("./out/loo_overview_table_site.rds", sep=""))
