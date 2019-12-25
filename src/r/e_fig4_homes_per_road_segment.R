# playing with the homes per road segment thing

source("src/r/a_prep_environment.R")
source("src/r/b_import_clean_data.R")
library(stars)
library(fasterize)
library(sf)
library(scales)
library(foreach)
library(ggpubr)
library(doParallel)

# 2016 zillow data
home_density <- raster("data/ztrax_cumsum_2016.tif")
# 2019 county-level data from us census derived inthe road densit script
road_density <- raster("data/road_denisity_km_km2.tif")
# one divided by the other
homes_per_road <-home_density/road_density


# writeRaster(home_density,"data/homes_per_km_road.tif")

hpr_fish_file <- "data/hpr_fishnet.gpkg"
if(!file.exists(hpr_fish_file)){
  registerDoParallel(detectCores()-1)
  res<-foreach(i = 1:nrow(fishnet_50k), .combine=rbind)%dopar%{
    h <- raster::extract(home_density, fishnet_50k[i,], fun = sum, na.rm=TRUE)
    r <- raster::extract(road_density, fishnet_50k[i,], fun = sum, na.rm=TRUE)
    return(fishnet_50k[i,] %>% mutate(homes_n = h, roads_km = r))
  }
  
  
  res <- res%>%
    mutate(homes_per_km_road = homes_n/roads_km)%>%
    mutate(classes = cut(homes_per_km_road, 
                         breaks=c(0,5,10,20,50,100,100000000),
                         labels = c("0-5", "5-10","10-20", "20-50", "50-100",
                                    "100+")))
  st_write(res, hpr_fish_file, delete_dsn = TRUE)
}else{res <- st_read(hpr_fish_file)}

# classification functions -----------------------------------------------------
classify_fsr <-  function(x) {
  ifelse(x < 15, "< 15",
         ifelse(x >= 15 & x < 40, "15 - 40",
                ifelse(x >= 40 & x < 100, "40 - 100",
                       ifelse(x >= 100& x <400, "100 - 400",
                              "> 400"))))
}

classify_freq <-  function(x) {
  ifelse(x >= 1 & x <= 25, "1 - 25",
         ifelse(x >= 25 & x < 100, "25 - 100",
                ifelse(x >= 100 & x < 250, "100 - 250",
                       ifelse(x >= 250 & x < 500, "250 - 500",
                              "> 500"))))
}

classify_max_fsr <-  function(x) {
  ifelse(x < 100, "< 100",
         ifelse(x >= 100 & x < 200, "100 - 200",
                ifelse(x >= 200 & x < 500, "200 - 500",
                       ifelse(x >= 500& x <2000, "500 - 2000",
                              "> 2000"))))
}
classify_max_max_fsr <-  function(x) {
  ifelse(x < 200, "< 200",
         ifelse(x >= 200 & x < 500, "200 - 500",
                ifelse(x >= 500 & x < 2000, "500 - 2,000",
                       ifelse(x >= 2000 & x <10000, "2,000 - 10,000",
                              "> 10,000"))))
}

# wrangling the data -----------------------------------------------------------
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
  na.omit() %>%
  ggplot() +
  geom_sf(data=states, fill = "transparent")+
  geom_sf(aes(color = classes),
             size = 2.3, show.legend = "point") +
  scale_color_viridis_d(direction=-1)+
  #scale_color_gradient2(low = "grey80", mid =("red"), high = "black",
  #                      midpoint = 17)+
  theme_nothing(legend = TRUE) +
  labs(color = "Homes per km road")+
  theme(legend.position = c(0.1,0),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = "transparent"))+
  guides(col = guide_legend(ncol=2,
                            override.aes = list(shape = 15, 
                                                size = 10)))+
  annotate("text", 
           x= st_bbox(res)$xmin + ((st_bbox(res)$xmax- st_bbox(res)$xmin )/2),
           y=st_bbox(res)$ymax - (st_bbox(res)$ymax/10) ,
           label="Fast Fires and Traffic",
           hjust = "center",
           size=7) +
  ggsave(file = file.path(draft_figs_dir, "fire_traffic.png"), dpi = 300);p1


p2 <- res %>%
  na.omit() %>%
  st_centroid %>%
  ggplot() +
  geom_sf(data=states, fill = "transparent")+
  geom_sf(aes(color = classes),
          size = 2.3, show.legend = "point") +
  scale_color_viridis_d(direction=-1)+
  theme_nothing(legend = TRUE) +
  labs(color = "Homes per km road")+
  theme(legend.position = c(0.05,0),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank())+
  guides(col = guide_legend(ncol=2,
                            override.aes = list(shape = 15, 
                                                size = 10)))+
  annotate("text", 
           x= st_bbox(res)$xmin + ((st_bbox(res)$xmax- st_bbox(res)$xmin )/2),
           y=st_bbox(res)$ymax - (st_bbox(res)$ymax/10) ,
           label="Homes per km of road",
           hjust = "center",
           size=7) +
  ggsave(file = file.path(draft_figs_dir, "homes_per_km_road_50k.png"), dpi = 300);p2

# homes per km road ------------------------------------------------------------
hpr<- raster("data/homes_per_km_road.tif")
hpr[0]<-NA
writeRaster(hpr,"data/homes_per_km_road_0_na.tif")

hpr<- read_stars("data/homes_per_km_road_0_na.tif")

mid<-exp(log(max(getValues(raster("data/homes_per_km_road_0_na.tif")), na.rm=T))/2)

classInt::classIntervals(na.omit(getValues(raster("data/homes_per_km_road.tif"))), 
                         fixedBreaks=c(0,10,20,50,100,500,1000, 100000),style ="fixed")

for_plotting<-raster("data/homes_per_km_road.tif") %>%
  as.data.frame(xy=TRUE) %>%
  na.omit()%>%
  filter(homes_per_km_road>0) %>%
  mutate(classes = cut(homes_per_km_road, 
                       breaks=c(0,5,10,20,50,100,100000000),
                       labels = c("0-5", "5-10","10-20", "20-50", "50-100",
                                  "100+")))

library(RColorBrewer)
p3 <- ggplot() +
  geom_tile(data = for_plotting, aes(fill = classes, x=x,y=y)) +
  theme_void() +
  # scale_fill_manual(#values = RColorBrewer::brewer.pal(6,"Spectral"),
  #                   values = viridis(6,direction = -1),
  #                   na.value ="white")+
  scale_fill_viridis_d(direction=-1)+
  geom_sf(data = states, fill = "transparent")+
  annotate("text", 
           x= st_bbox(hpr)$xmin + ((st_bbox(hpr)$xmax- st_bbox(hpr)$xmin )/2),
           y=st_bbox(hpr)$ymax - (st_bbox(hpr)$ymax/10) ,
           label="Homes per km of road",
           hjust = "center",
           size=7)+
  labs(fill="Homes per km road") +
  theme(legend.position=c(0.05,0.05),
        legend.justification=c(0,0),
        legend.title = element_blank()) +
  ggsave("results/draft_figures/homes_per_km_road.png")

# 2 panel option 1
dev.off()
ggarrange(p1,p2, nrow = 1, ncol = 2, labels = "auto") +
  ggsave("results/draft_figures/2_panel_option1.png", dpi = 300, width = 18, height = 6)

ggarrange(p1,p3, nrow =1, ncol = 2, labels = "auto") +
  ggsave("results/draft_figures/2_panel_option2.png", dpi = 300, width = 18, height = 6)
