source("src/r/a_prep_environment.R")
library(ggpubr)
if(!file.exists(file.path(modis_event_dir, "modis_event_polygons_cus.gpkg"))){
  system(paste("aws s3 cp",
               file.path(s3_events_path, "modis_event_polygons_cus.gpkg"),
               file.path(modis_event_dir, "modis_event_polygons_cus.gpkg")))
}
modis_events <- st_read(file.path(modis_event_dir, 
                                  "modis_event_polygons_cus.gpkg")) %>%
  filter(as.numeric(as.character(ignition_year)) >= '2001') 

ggplot(modis_events %>% filter(total_area_ha >1000, !is.na(l1_ecoregion)), aes(x=max_growth_ha)) +
  geom_histogram() +
  facet_wrap(~l1_ecoregion, scales = "free")+
  coord_flip() +
  theme_pubr() +
  ylab("Maximum Single Day Fire Growth (ha)") +
  xlab("Count")+
  ggsave("results/draft_figures/max_growth_hist_by_l1eco.png", height = 7, width = 12)

ggplot(modis_events %>% filter(total_area_ha >1000, 
                               !is.na(lc_name),
                               lc_name != "Barren", 
                               lc_name != "Urban and Built-up Lands"), 
       aes(x=max_growth_ha)) +
  geom_histogram() +
  facet_wrap(~lc_name, scales = "free") +
  coord_flip() +
  theme_pubr() +
  ylab("Maximum Single Day Fire Growth (ha)") +
  xlab("Count")+
  ggsave("results/draft_figures/max_growth_hist_by_lc.png", height = 8, width =12)
