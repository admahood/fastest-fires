# spatiotemporal trends by landcover


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
