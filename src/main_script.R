#read dataset
library(tidyverse)
library(data.table)
library(GGally)
#load data for NEON spectra and indexes
path_predictors = "~/Dropbox (UFL)/RMBL_2018/Data_for_Sergio/NEON_refl_VI_plot_cl_brnor_500_2400_adaptive.csv"
path_responses = "~/Dropbox (UFL)/RMBL_2018/Data_for_Sergio/Abiotic_var_fluxes_traits_field.csv"
df = generate_augmented_matrix(path_predictors=path_predictors,
                               path_responses=path_responses)

ecosystem_fluxes = c("NEE","GPP","Reco","Rsoil","SOC","ET","WUE")
indexes = c("NDVI","RENDVI", "VREI1","VREI2","MCARI2","NDNI",
            "NDLI","PRI","PRI2","NDWI")
#plot responses correlation and distribution
df %>% ungroup %>% select(ecosystem_fluxes) %>%
  mutate_all(log10)%>%
  ggpairs(aes(alpha = 0.4))

#it seems that responses have a lognormal distribution. will use that for the formula
#joint model
joint_model = train_model(df, response = ecosystem_fluxes, features = indexes)

#individual models
all_models = list()
for(fl in ecosystem_fluxes){
  all_models[[fl]]=train_model(df, features, response, seed = 1987, scale_responses=T)
}

# numeric_cols = colnames(dataset)[6:306]
# dataset[numeric_cols] = scale(dataset[numeric_cols])
# dataset = data.frame(dataset)
# train = dataset %>% sample_frac(0.8)
# test = dataset %>% filter(!(PlotCode %in%train$PlotCode))
# spectra = train
# library(brms)
# #train model using bayesian lasso (horseshoe priors) to identify which indexes and bands can be reduced
# reflectance.nee = brm(paste("log10(NEE) ~ ", paste(paste("(", features_name, ")"), collapse = " + ")),
#                       # define data and grouping correlation structures
#                       data = train, family = student(),
#                       set_prior(horseshoe()),
#                       #other parameters
#                       chains=4, cores =2, iter = 6000)#, backend = "cmdstanr", threads = threading(2))
# 
# bayes_R2(reflectance.nee)
# bayes_R2(reflectance.nee, newdata=test)
# 
# reflectance.nee.hs = brm(paste("NEE ~ ", paste(paste("(", colnames(train)[6:306], ")"), collapse = " + ")),
#                       # define data and grouping correlation structures
#                       data = train, family = student(),
#                       set_prior(horseshoe()),
#                       #other parameters
#                       chains=4, cores =2, iter = 6000)#, backend = "cmdstanr", threads = threading(2))
# 
# bayes_R2(reflectance.nee.hs, newdata=test)
# test_predict = predict(reflectance.nee.hs, newdata = test, robust = T)
# 
# 
# library(pls)
# pls_train = train %>% select(c("NEE", colnames(train)[6:306]))
# plsglm.nee = plsr(NEE ~ ., ncomp = 10, data = pls_train, validation = "LOO")
# 
# #plsglm.nee = plsRglm(NEE~.,data=pls_train,nt=10)
# 
# pls_test = test %>% select(c("NEE", colnames(train)[6:306]))
# NEE.estimate = predict(plsglm.nee, pls_test, comps = 5)
# 
# compare_tests = cbind.data.frame(test_predict, NEE.estimate, test$NEE)
# colnames(compare_tests)[5:6] = c("PLS.estimate", "obs_NEE")
# ggplot(data = compare_tests)+
#   geom_point(aes(x = Estimate, y = obs_NEE, color = "Bayes")) +
#   geom_point(aes(x = PLS.estimate, y = obs_NEE, color = "PLS")) + 
#   theme_bw() + geom_abline(slope = 1)
