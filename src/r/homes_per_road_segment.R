# playing with the homes per road segment thing

source("src/r/a_prep_environment.R")
source("src/r/b_import_clean_data.R")
library(stars)
library(fasterize)
library(sf)
library(foreach)
library(doParallel)
homes_per_road <- raster("data/homes_per_rd_seg/homes_per_rdseg.tif")

registerDoParallel(detectCores()-1)
res<-foreach(i = 1:nrow(fishnet_50k), .combine=rbind)%dopar%{
  x <- raster::extract(homes_per_road, fishnet_50k[i,], fun = mean, na.rm=TRUE)
  return(fishnet_50k[i,] %>% mutate(hpr = x))
}


res <- res%>%
  mutate(log_hpr = log(hpr))
st_write(res, "data/hpr_fishnet.gpkg", delete_dsn = TRUE)

plot(res["log_hpr"])
