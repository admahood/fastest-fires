# playing with the homes per road segment thing

source("src/r/a_prep_environment.R")
source("src/r/b_import_clean_data.R")
library(stars)
library(fasterize)
library(sf)
library(scales)
library(foreach)
library(doParallel)
homes_per_road <- raster("data/homes_per_rd_seg/homes_per_rdseg.tif")
home_density <- raster("/home/a/data/ztrax/ztrax_cumsum_2016.tif")
road_density <- raster("data/road_denisity_km_km2.tif")

# writeRaster(home_density/road_density,"homes_per_km_road.tif")

registerDoParallel(detectCores()-1)
res<-foreach(i = 1:nrow(fishnet_50k), .combine=rbind)%dopar%{
  h <- raster::extract(home_density, fishnet_50k[i,], fun = sum, na.rm=TRUE)
  r <- raster::extract(road_density, fishnet_50k[i,], fun = sum, na.rm=TRUE)
  return(fishnet_50k[i,] %>% mutate(homes_n = h, roads_km = r))
}


res <- res%>%
  mutate(homes_per_km_road = homes_n/roads_km)
st_write(res, "data/hpr_fishnet.gpkg", delete_dsn = TRUE)

modis_fish <- fishnet_50k %>%
  st_intersection(., st_centroid(
    st_transform(modis_events, 
                 crs=st_crs(fishnet_50k)))) %>%
  group_by(fishid50k) %>%
  summarise(freq = n(),
            fsr_ha = mean(fsr_ha_per_day, na.rm=TRUE),
            mean_max_growth = mean(max_growth_ha),
            max_max_growth =  max(max_growth_ha)) %>%
  st_centroid() %>%
  mutate(class_fsr = classify_fsr(fsr_ha),
         class_freq = classify_freq(freq),
         class_max_fsr = classify_max_fsr(mean_max_growth),
         class_max_max_fsr = classify_max_max_fsr(max_max_growth))

res_p <- res %>%
  st_centroid() %>%
  left_join(st_set_geometry(modis_fish, NULL), by = "fishid50k") %>%
  filter(max_max_growth >9999)

p1 <- res_p %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long,y = lat, group=group), 
               color='black', fill = "transparent", size = .50)+
  geom_sf(aes(color = (homes_per_km_road)),
             size = 2.3) +
  # scale_color_viridis_c(direction=-1, option="B")+
  scale_color_gradient2(low = "grey80", mid =("red"), high = "black",
                        midpoint = 17)+
  theme_nothing(legend = TRUE) +
  labs(color = "Homes per km road")+
  theme(legend.position = c(0.1,0),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = "transparent"))+
  annotate("text", 
           x= st_bbox(res)$xmin + ((st_bbox(res)$xmax- st_bbox(res)$xmin )/2),
           y=st_bbox(res)$ymax - (st_bbox(res)$ymax/10) ,
           label="Fast Fires and Traffic",
           hjust = "center",
           size=7) +
  ggsave(file = file.path(draft_figs_dir, "fire_traffic.png"), dpi = 300);p1
