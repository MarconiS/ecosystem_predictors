library(data.table)
library(tidyverse)
ecosystem_fluxes = c("NEE","GPP","Reco","Rsoil","SOC","ET","WUE")
ls_files = list.files("./out_mixed_eff/")
ls_files = ls_files[ls_files %like% "loo"]
ls_files = ls_files[!ls_files %like% "overview_table"]
statistic = "RMSE"
waic_overview = list()
for(prdct in ls_files){
  br2_ = list()
  all_models = read_rds(paste("./out_mixed_eff/", prdct, sep=""))
  for(fl in ecosystem_fluxes){
    br2_[[fl]] = (loo_mixed_effCV[[fl]][[statistic]])
  }
  waic_overview[[prdct]] = do.call(rbind.data.frame, br2_)
}
results = data.frame(waic_overview)
colnames(results) = names(waic_overview)
results = cbind.data.frame(ecosystem_fluxes, results)
write_csv(results, "/Volumes/Data/results_RMSE.csv")
saveRDS(br2_overview,paste("./out_mixed_eff/overview_table.rds", sep=""))

Abiotic_var$NEE$WAIC
