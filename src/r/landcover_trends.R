## looking at landcover trends in modis

library(sf)
library(raster)
library(doParallel)
library(foreach)
library(tidyverse)
options(stringsAsFactors = FALSE)

# these mosaics were created in a different project
mosaic_files <- list.files("~/data/MCD12Q1_mosaics/", 
                           pattern = ".tif",
                           full.names = TRUE)

classes <- read_csv("data/usa_landcover_t1_classes.csv")

corz <- detectCores()-1
registerDoParallel(corz)
lc_by_year<-foreach(i = 1:length(mosaic_files), .combine = rbind)%dopar%{
  year <- str_sub(mosaic_files[i],
                  length(mosaic_files[i]) - 9,
                  length(mosaic_files[i]) - 6) %>%
    as.numeric()
  
  results <- raster(mosaic_files[i]) %>%
    freq() %>%
    as.data.frame() %>%
    mutate(year = year) %>%
    left_join(classes) %>%
    na.omit()
  
  return(results)
  system(paste("echo", i))
}


ggplot(lc_by_year %>% filter(name !="Barren", year <2017),
       aes(x=year, y=count, color = name)) +
  geom_line() +
  theme(axis.text = element_blank())+
  facet_wrap(~name
             , scales = "free"
             )
  
