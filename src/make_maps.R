# make predictions
#load maps and turn them into KL 
library(sf)
library(rgeos)
# 
# kld_grps = read_csv("../NeonSpeciesClassification/data/kld_grps_2103_0308.csv", col_names = F)
# cbt = raster::brick("/Users/sergiomarconi/Downloads/drive-download-20210428T221522Z-001/NEON_CBT.tif")
# pbm = raster::brick("/Users/sergiomarconi/Downloads/drive-download-20210428T221522Z-001/NEON_PBM.tif")
# pf_en = raster::brick("/Users/sergiomarconi/Downloads/drive-download-20210428T221522Z-001/NEON_Pfeiler_Enquist.tif")
# pf_doe = raster::brick("/Users/sergiomarconi/Downloads/drive-download-20210428T221522Z-001/NEON_PfeilerDOE.tif")
# pump = raster::brick("/Users/sergiomarconi/Downloads/drive-download-20210428T221522Z-001/NEON_Pump.tif")
# road = raster::brick("/Users/sergiomarconi/Downloads/drive-download-20210428T221522Z-001/NEON_Road.tif")
# upperMT = raster::brick("/Users/sergiomarconi/Downloads/drive-download-20210428T221522Z-001/NEON_upperMT.tif")


site = "CBT"
path_to_tiff = "/Users/sergiomarconi/Downloads/drive-download-20210428T221522Z-001/"
path_to_models = "./out/NEON_for_maps_reflectance_NEON.csv.rds"
for(site in list_sites){
  
  rbrick = raster::brick(paste(path_to_tiff, "NEON_",site , ".tif", sep=""))
  for(fl in ecosystem_fluxes){
    
    rbrick_df = raster::as.data.frame(rbrick) 
    colnames(rbrick_df)
    #remove unused bands
    # rgb = np.delete(rgb, np.r_[419:425])
    # rgb = np.delete(rgb, np.r_[281:313])
    # rgb = np.delete(rgb, np.r_[191:211])
    rbrick_df = rbrick_df[,(colnames(rbrick_df) %like% "B")]
    rbrick_df = rbrick_df[,-c(192:212, 282:314, 420:425)]
    rbrick_df = rbrick_df[,c(11:357)]
    norm_vec <- function(x) x/sqrt(sum(x^2))
    noisy_pixels =  apply((rbrick_df), 1, function(x)any(is.nan(x)))
    rbrick_df = apply((rbrick_df), 2, norm_vec)
    
    kl_refl = rbrick_df %>% t
    kl_refl = cbind.data.frame(kl_refl, kld_grps)
    colnames(kl_refl)[ncol(kl_refl)] = "kl_grp"
    kl_mean = kl_refl %>% group_by(kl_grp) %>%
      summarize_each(mean) %>% ungroup %>%
      select(-one_of("kl_grp")) %>% t %>% data.frame
    colnames(kl_mean) = paste("mean", colnames(kl_mean), sep="_")
    
    kl_max = kl_refl %>% group_by(kl_grp) %>%
      summarize_each(max) %>% ungroup %>%
      select(-one_of("kl_grp")) %>% t %>% data.frame
    colnames(kl_max) = paste("max", colnames(kl_max), sep="_")
    
    kl_min = kl_refl %>% group_by(kl_grp) %>%
      summarize_each(mean) %>% ungroup %>%
      select(-one_of("kl_grp")) %>% t %>% data.frame
    colnames(kl_min) = paste("min", colnames(kl_min), sep="_")
    
    kl_brick = cbind.data.frame(kl_mean, kl_max, kl_min)
    colnames(kl_brick) = paste("nm", 1:45, sep="_")
    #here is where 
    all_models = readRDS(path_to_models)
    fit_tmp = all_models[[fl]]$model
    feat_scale = all_models[[fl]]$scaling
    
    kl_brick = scale(kl_brick, center = attr(feat_scale$features, "scaled:center"),
                     scale = attr(feat_scale$features, "scaled:scale"))
    
    predictions = predict(fit_tmp, newdata = kl_brick)
    
    # create raster stack with predictions
    dim(predictions) = c(dim(cbt)[1], dim(cbt)[2], 4)
    rast = rbrick
    raster::values(rast) = predictions
    names(rast) = c("Estimate", "SE", "Q2_5", "Q97_5")
    raster::writeRaster(rast, paste("./out/maps/", fl, "_", site,".tiff", sep=""), writeFormats = "GTiff", overwrite=T)
  }
}