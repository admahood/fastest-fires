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

registerDoParallel(detectCores()-1)
trends_df<-foreach(i = 1:nrow(fishnet_50k),.combine=rbind)%dopar%{

  fishrow <- fishnet_50k[i,]%>%
    st_intersection(modis_points) %>%
    filter(duration>5)
  
  if(nrow(fishrow) >10){
    
    mod_fsr <- mblm(fsr_ha_per_day ~ ignition_year, fishrow) %>% summary
    mod_growth <- mblm(max_growth_ha~ignition_year, fishrow) %>% summary
    
    xx <-data.frame(
      mean_fsr_trend_ha_day = mod_fsr$coefficients[2,1],
      mean_fsr_p = mod_fsr$coefficients[2,4],
      max_growth_trend_ha = mod_growth$coefficients[2,1],
      max_growth_p = mod_fsr$coefficients[2,4],
      fishid50k = fishnet_50k[i,]$fishid50k %>% as.numeric(),
      n = nrow(fishrow))
    
  }else{
    xx <-data.frame(
      mean_fsr_trend_ha_day = NA,
      mean_fsr_p = NA,
      max_growth_trend_ha = NA,
      max_growth_p = NA,
      fishid50k = fishnet_50k[i,]$fishid50k %>% as.numeric(),
      n = nrow(fishrow))
  }
  return(xx)
}

fishnet_trends <- left_join(fishnet_50k, trends_df, by="fishid50k") %>%
  st_centroid()

mean_fsr <- fishnet_trends %>%
  filter(mean_fsr_p < 0.05) %>%
  mutate(class = ifelse(mean_fsr_trend_ha_day >0, "Positve", "Negative"))

max_g <- fishnet_trends %>%
  filter(max_growth_p < 0.05) %>%
  mutate(class = ifelse(max_growth_trend_ha >0, "Positve", "Negative"))

ggarrange(
ggplot() +
  geom_sf(data=states, fill="white")+
  geom_sf(data = mean_fsr, 
          aes(color = class, size=n), show.legend = "point") +
  scale_color_manual(values=c("skyblue", "red"))+
  labs(size = "# Events", color= 'Direction of\nTrend')+
  theme_void() +
  theme(legend.box = "horizontal",
        legend.position = c(0.05,0.04),
        legend.justification = c(0,0)) +
  ggtitle("Significant Trends in Mean Fire Spread Rate", 
          "Fire events longer than 5 days")
,
ggplot() +
  geom_sf(data=states, fill="white")+
  geom_sf(data = max_g, 
          aes(color = class, size=n), show.legend = "point") +
  scale_color_manual(values=c("skyblue", "red"))+
  labs(size = "# Events", color= 'Direction of\nTrend')+
  theme_void() +
  theme(legend.box = "horizontal",
        legend.position = c(0.05,0.04),
        legend.justification = c(0,0)) +  
  ggtitle("Significant Trends in Maximum Single-Day Fire Growth",
          "Fire events longer than 5 days"),nrow=2, ncol=1) +
  ggsave("results/draft_figures/spatiotemporal_trends.png", dpi=600, width =10, height = 14) +
  ggsave("results/draft_figures/spatiotemporal_trends_small.png", dpi=200, width =10, height = 14)
  

