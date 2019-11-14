source("src/r/a_prep_environment.R")
source("src/r/b_import_clean_data.R")

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
# need to turn modis_events into centroids
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

fishnet_50k_sp <- as(st_centroid(fishnet_50k), "Spatial")
fs50_df <- SpatialPointsDataFrame(fishnet_50k_sp, fishnet_50k_sp@data)
fs50_df$id <- row.names(fs50_df)
fs50_df <- data.frame(fs50_df)

usa <- as(states, "Spatial")
usa$id <- row.names(usa)
st_df <- fortify(usa, region = 'id')
st_df <- left_join(st_df, usa@data, by = 'id')
names(st_df) <- tolower(names(st_df))

modis_fish_ff <- left_join(fs50_df, modis_fish, by = "fishid50k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) 


p1 <- modis_fish_ff %>%
  na.omit() %>%
  filter(freq != 0) %>%
  # transform(class_freq = factor(class_freq, levels=c("1 - 25", "25 - 100","100 - 250", "250 - 500", "> 500"))) %>%
  transform(class_fsr = factor(class_fsr, levels=c("< 15", "15 - 40", "40 - 100","100 - 400","> 400"))) %>%
  ggplot() +
    geom_polygon(data = st_df, aes(x = long,y = lat, group=group), 
                 color='black', fill = "white", size = .50)+
    geom_point(aes(x = long, y = lat, color = class_fsr),
               size = 2.3) +
    coord_equal() +
    #scale_color_viridis_d(option = "A", direction = -1) +
    scale_color_manual(values = rev(brewer.pal(5,"Spectral")),
                       name = "Fire Spread Rate (ha/day)") +
    #scale_size_discrete(range = c(.25, 2)) +
    theme_nothing(legend = TRUE) +
    ggtitle("Mean Fire Spread Rate") +
    theme(plot.title = element_text(hjust = 0.1, size = 20),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          legend.key = element_rect(fill = "white"),
          legend.position = c(.04,0),
          legend.justification = c(0,0),
          legend.background = element_rect(fill ="transparent"),
          legend.direction = "vertical",
          legend.box = "horizontal",
          legend.box.just = "bottom")+
    guides(col = guide_legend(override.aes = list(shape = 15, size = 7))) +
    ggsave(file = file.path(draft_figs_dir, "mean_fsr_50k.png"), dpi = 300);p1
# legend.key = element_rect(fill = "white"))

p2 <- modis_fish_ff %>%
  na.omit() %>%
  filter(freq != 0) %>%
  # transform(class_max_fsr = factor(class_freq, levels=c("1 - 25", "25 - 100","100 - 250", "250 - 500", "> 500"))) %>%
  transform(class_max_max_fsr = factor(class_max_max_fsr, 
                                   levels=c("< 200", "200 - 500", "500 - 2,000",
                                            "2,000 - 10,000","> 10,000"))) %>%
  ggplot() +
    geom_polygon(data = st_df, aes(x = long,y = lat, group=group), 
                 color='black', fill = "white", size = .50)+
    geom_point(aes(x = long, y = lat, color = class_max_max_fsr),
               size = 2.3) +
    coord_equal() +
    #scale_color_viridis_d(option = "A", direction = -1) +
    scale_color_manual(values = rev(brewer.pal(5,"Spectral")),
                       name = "Fire Spread Rate (ha/day)") +
    #scale_size_discrete(range = c(.25, 2)) +
    theme_nothing(legend = TRUE) +
    ggtitle("Maximum Single Day Fire Growth") +
    theme(plot.title = element_text(hjust = 0.1, size = 20),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          legend.key = element_rect(fill = "white"),
          legend.position = c(.04,0),
          legend.justification = c(0,0),
          legend.background = element_rect(fill ="transparent"),
          legend.direction = "vertical",
          legend.box = "horizontal",
          legend.box.just = "bottom")+
    guides(col = guide_legend(override.aes = list(shape = 15, size = 7))) +
    ggsave(file = file.path(draft_figs_dir, "mean_max_growth_50k.png"), dpi = 300);p2

# stats for proposal

n_reds <- modis_fish_ff %>%
  filter(class_max_max_fsr == "> 10,000") %>%
  nrow()

n_reds*50*50

n_reds/nrow(modis_fish_ff)*100

proposal_fig <- modis_fish_ff %>%
  na.omit() %>%
  filter(freq != 0) %>%
  # transform(class_max_fsr = factor(class_freq, levels=c("1 - 25", "25 - 100","100 - 250", "250 - 500", "> 500"))) %>%
  transform(class_max_max_fsr = factor(class_max_max_fsr, 
                                       levels=c("< 200", "200 - 500", "500 - 2,000",
                                                "2,000 - 10,000","> 10,000"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long,y = lat, group=group), 
               color='black', fill = "white", size = .50)+
  geom_point(aes(x = long, y = lat, color = class_max_max_fsr),
             size = 2.8) +
  coord_equal() +
  #scale_color_viridis_d(option = "A", direction = -1) +
  scale_color_manual(values = rev(brewer.pal(5,"Spectral")),
                     name = "Fire Spread Rate (ha/day)") +
  #scale_size_discrete(range = c(.25, 2)) +
  theme_nothing(legend = TRUE) +
  ggtitle("Maximum Single Day Fire Growth") +
  theme(plot.title = element_text(hjust = 0.1, size = 30, face = "plain"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.text = element_text(size = 20, face = "plain"),
        legend.title = element_text(size = 22, face= "plain"),
        legend.key = element_rect(fill = "white"),
        legend.position = c(.04,0),
        legend.justification = c(0,0),
        legend.background = element_rect(fill ="transparent"),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.box.just = "bottom")+
  guides(col = guide_legend(ncol=2,
                            override.aes = list(shape = 15, 
                                                size = 10))) +
  # guides(col=guide_legend())+
  ggsave(file = file.path(draft_figs_dir, "proposal_fig.png"), dpi = 300);proposal_fig
