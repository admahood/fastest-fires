# map of temporal trends script
source("src/r/a_prep_environment.R")
source("src/r/b_import_clean_data.R")
library(foreach)
library(doParallel)
library(ggpubr)
library(mblm)
modis_points<- modis_events %>%
  st_centroid() %>%
  st_transform(crs=st_crs(fishnet_50k)) %>%
  mutate(ignition_year = as.numeric(ignition_year))

# by 50km fishnet --------------------------------------------------------------

registerDoParallel(detectCores()-1)
trends_df<-foreach(i = 1:nrow(fishnet_50k),.combine=rbind)%dopar%{
  
  system(paste("echo",i))  
  
  fishrow <- fishnet_50k[i,]%>%
    st_intersection(modis_points) # %>%
    # filter(duration>2)
  #errors
  #100, 199, 298,397,496,595
  if(nrow(fishrow) >10){
    
    mod_fsr <- mblm(fsr_ha_per_day ~ ignition_year, fishrow, 
                    repeated = FALSE) %>% summary
    mod_growth <- mblm(max_growth_ha~ignition_year, fishrow, 
                       repeated = FALSE) %>% summary
    mod_dur <- mblm(duration~ignition_year, fishrow, 
                    repeated = FALSE) %>% summary
    
    xx <-data.frame(
      mean_fsr_trend_ha_day = mod_fsr$coefficients[2,1],
      mean_fsr_p = mod_fsr$coefficients[2,4],
      max_growth_trend_ha = mod_growth$coefficients[2,1],
      max_growth_p = mod_fsr$coefficients[2,4],
      duration_trend_days = mod_dur$coefficients[2,1],
      duration_p = mod_dur$coefficients[2,4],
      fishid50k = fishnet_50k[i,]$fishid50k %>% as.numeric(),
      n = nrow(fishrow))
    
  }else{
    xx <-data.frame(
      mean_fsr_trend_ha_day = NA,
      mean_fsr_p = NA,
      max_growth_trend_ha = NA,
      max_growth_p = NA,
      duration_trend_days = NA,
      duration_p = NA,
      fishid50k = fishnet_50k[i,]$fishid50k %>% as.numeric(),
      n = nrow(fishrow))
  }
  return(xx)
}

fishnet_trends <- left_join(fishnet_50k, trends_df, by="fishid50k") %>%
  st_centroid()

mean_fsr <- fishnet_trends %>%
  filter(n>=20) %>%
  mutate(class = ifelse(mean_fsr_trend_ha_day >0, "Positve", "Negative"),
         sig = ifelse(mean_fsr_p < 0.05, "Significant", "Not Significant"),
         ss = ifelse(sig == "Significant", class,"Not Significant")
         )

max_g <- fishnet_trends %>%
  filter(n>=20) %>%
  mutate(class = ifelse(max_growth_trend_ha >0, "Positve", "Negative"),
         sig = ifelse(max_growth_p < 0.05, "Significant", "Not Significant"),
         ss = ifelse(sig == "Significant", class,"Not Significant")
  )

duration <- fishnet_trends %>%
  filter(n>=20) %>%
  mutate(class = ifelse(duration_trend_days >0, "Positve", "Negative"),
         sig = ifelse(duration_p < 0.05, "Significant", "Not Significant"),
         ss = ifelse(sig == "Significant", class,"Not Significant")
  )

ggarrange(ggplot() +
            geom_sf(data=states, fill="white")+
            geom_sf(data = mean_fsr, alpha = 0.9,
                    aes(color = ss, size=n), show.legend = "point") +
            scale_color_manual(values=c("skyblue","grey", "red"))+
            labs(size = "# Events", color= 'Direction of\nTrend')+
            theme_void() +
            theme(legend.box = "horizontal",
                  legend.position = c(0.05,0.04),
                  legend.justification = c(0,0)) +
            annotate("text", label ="Temporal Trends in Mean Fire Spread Rate",
                     x=((st_bbox(states)$xmax - st_bbox(states)$xmin)/2) +st_bbox(states)$xmin,
                     y=st_bbox(states)$ymax - (st_bbox(states)$ymax/10),
                     hjust = "center", size=7)

          ,ggplot() +
            geom_sf(data=states, fill="white")+
            geom_sf(data = duration, alpha = 0.9,
                    aes(color = ss, size=n), show.legend = "point") +
            scale_color_manual(values=c("skyblue","grey", "red"))+
            labs(size = "# Events", color= 'Direction of\nTrend')+
            theme_void() +
            theme(legend.box = "horizontal",
                  legend.position = c(0.05,0.04),
                  legend.justification = c(0,0)) +
            annotate("text", label ="Significant Trends in Fire Duration",
                     x=((st_bbox(states)$xmax - st_bbox(states)$xmin)/2) +st_bbox(states)$xmin,
                     y=st_bbox(states)$ymax - (st_bbox(states)$ymax/10),
                     hjust = "center", size=7) 
           
          ,nrow=2, ncol=1) +
  ggsave("results/draft_figures/spatiotemporal_trends.png", dpi=600, width =10, height = 14) +
  ggsave("results/draft_figures/spatiotemporal_trends_small.png", dpi=200, width =10, height = 14)
  

ggplot() +
  geom_sf(data=states, fill="white")+
  geom_sf(data = max_g, 
          aes(color = ss, size=n), show.legend = "point") +
  scale_color_manual(values=c("skyblue", "grey","red"))+
  labs(size = "# Events", color= 'Direction of\nTrend')+
  theme_void() +
  theme(legend.box = "horizontal",
        legend.position = c(0.05,0.04),
        legend.justification = c(0,0)) +  
  ggtitle("Significant Trends in Maximum Single-Day Fire Growth",
          "Fire events longer than 5 days")

# ecoregion --------------------------------------------------------------------
# note: this takes a very long time on 8 cores, uses about 17gb of ram

registerDoParallel(detectCores()-1)
eco_trends_df<-foreach(i = 1:nrow(ecoregions_l321),.combine=rbind)%dopar%{
  
  system(paste("echo",i, ecoregions_l321[i,]$us_l3name %>% as.character()))  
  
  fishrow <- ecoregions_l321[i,]%>%
    st_intersection(modis_points)  %>%
    filter(duration>5)

  if(nrow(fishrow) >10){
    
    mod_fsr <- mblm(fsr_ha_per_day ~ ignition_year, fishrow, 
                    repeated = FALSE) %>% summary
    mod_growth <- mblm(max_growth_ha~ignition_year, fishrow, 
                       repeated = FALSE) %>% summary
    mod_dur <- mblm(duration~ignition_year, fishrow, 
                    repeated = FALSE) %>% summary
    
    xx <-data.frame(
      mean_fsr_trend_ha_day = mod_fsr$coefficients[2,1],
      mean_fsr_p = mod_fsr$coefficients[2,4],
      max_growth_trend_ha = mod_growth$coefficients[2,1],
      max_growth_p = mod_fsr$coefficients[2,4],
      duration_trend_days = mod_dur$coefficients[2,1],
      duration_p = mod_dur$coefficients[2,4],
      us_l3name = ecoregions_l321[i,]$us_l3name %>% as.character(),
      n = nrow(fishrow))
    
  }else{
    xx <-data.frame(
      mean_fsr_trend_ha_day = NA,
      mean_fsr_p = NA,
      max_growth_trend_ha = NA,
      max_growth_p = NA,
      duration_trend_days = NA,
      duration_p = NA,
      us_l3name = ecoregions_l321[i,]$us_l3name %>% as.character(),
      n = nrow(fishrow))
  }
  return(xx)
}

ecoregion_trends <- left_join(ecoregions_l321, eco_trends_df, by="us_l3name")# %>%
  # st_centroid()


ecoregion_trends <- ecoregion_trends %>%
  mutate(sign_fsr = ifelse(mean_fsr_trend_ha_day >0, "Positive", "Negative"),
         sig_fsr = ifelse(mean_fsr_p < 0.05, "Significant", "Not Significant"),
         class_fsr = ifelse(sig_fsr == "Significant", sign_fsr,"Not Significant"),
         sign_mg = ifelse(max_growth_trend_ha >0, "Positive", "Negative"),
         sig_mg = ifelse(max_growth_p < 0.05, "Significant", "Not Significant"),
         class_mg = ifelse(sig_mg == "Significant", sign_mg,"Not Significant"),
         sign_d = ifelse(duration_trend_days >0, "Positive", "Negative"),
         sig_d = ifelse(duration_p < 0.05, "Significant", "Not Significant"),
         class_d = ifelse(sig_d == "Significant", sign_d,"Not Significant"),
         class_d = ifelse(is.na(class_d), "Insufficient data", class_d) %>%
           factor(levels = c("Positive", "Not Significant", "Negative",
                             "Insufficient data")),
         class_mg = ifelse(is.na(class_mg), "Insufficient data", class_mg) %>%
           factor(levels = c("Positive", "Not Significant", "Negative",
                             "Insufficient data")),
         class_fsr = ifelse(is.na(class_fsr), "Insufficient data", class_fsr) %>%
           factor(levels = c("Positive", "Not Significant", "Negative",
                             "Insufficient data"))
  )


st_write(ecoregion_trends,"ecoregion_trends_duration_5_or_more.gpkg", delete_dsn = TRUE)
system(paste0("aws s3 cp ","ecoregion_trends_duration_5_or_more.gpkg ",
              s3_base,
              "/ecoregion_trends_duration_5_or_more.gpkg"))

# messing with doing a cartogram
# isn't that informative but maybe try with burned area instead of n, and also
# maybe try aggregating by state

library(cartogram)
carto <- as(ecoregion_trends, "Spatial") %>%
  cartogram_cont("n") %>%
  as("sf")

# plotting

pmg<-ggplot() +
  geom_sf(data = ecoregion_trends, color = "black",
          aes(fill = class_mg)) +
  scale_fill_manual(values=c("red", "grey","skyblue","white"))+
  labs(size = "# Events", fill= 'Direction of\nTrend')+
  theme_void() +
  theme(legend.box = "horizontal",
        legend.position = c(0.05,0.04),
        legend.justification = c(0,0)) +  
  ggtitle("Trends in Maximum Single-Day Fire Growth",
          "Fire events longer than 5 days")

pfsr<- ggplot() +
  geom_sf(data = ecoregion_trends, color = "black",
          aes(fill = class_fsr)) +
  scale_fill_manual(values=c("red", "grey","skyblue","white"))+
  labs(fill= 'Direction of\nTrend')+
  theme_void() +
  theme(legend.box = "horizontal",
        legend.position = c(0.05,0.04),
        legend.justification = c(0,0)) +  
  ggtitle("Trends in Mean Fire Spread Rate",
          "Fire events longer than 5 days")

pd<-ggplot() +
  geom_sf(data = ecoregion_trends, color = "black",
          aes(fill = class_d)) +
  scale_fill_manual(values=c("red", "grey","skyblue","white"))+
  labs(fill= 'Direction of\nTrend')+
  theme_void() +
  theme(legend.box = "horizontal",
        legend.position = c(0.05,0.04),
        legend.justification = c(0,0)) +  
  ggtitle("Trends in Fire Duration",
          "Fire events longer than 5 days")

ggarrange(pmg, pfsr, pd, ncol = 1, nrow=3) +
  ggsave("egregions_3pan.png", height = 18, width = 8.5)
