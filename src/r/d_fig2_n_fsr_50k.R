source("src/r/a_prep_environment.R")
source("src/r/b_import_clean_data.R")

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


helper_function<- function(x) {
  # thanks http://rpubs.com/sogletr/sf-ops
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# converting modis_events into centroids ---------------------------------------
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

# stats on high fire spread rates

n_reds <- modis_fish_ff %>%
  filter(class_max_max_fsr == "> 10,000") %>%
  nrow()

n_reds*50*50

n_reds/nrow(modis_fish_ff)*100

fig_vals <- rev(brewer.pal(5,"Spectral"))
fig_vals[1] <- "grey"

proposal_fig <- modis_fish_ff %>%
  na.omit() %>%
  filter(freq != 0) %>%
  # transform(class_max_fsr = factor(class_freq, levels=c("1 - 25", "25 - 100","100 - 250", "250 - 500", "> 500"))) %>%
  transform(class_max_max_fsr = factor(class_max_max_fsr, 
                                       levels=c("< 200", "200 - 500", "500 - 2,000",
                                                "2,000 - 10,000","> 10,000"))) %>%
  ggplot() +
  geom_sf(data = states, fill = "white")+
  geom_point(aes(x = long, y = lat, color = class_max_max_fsr),
             size = 2.8) +
  # coord_equal() +
  #scale_color_viridis_d(option = "A", direction = -1) +
  scale_color_manual(values = fig_vals,
                     name = "Hectares") +
  #scale_size_discrete(range = c(.25, 2)) +
  theme_nothing(legend = TRUE) +
  #ggtitle("Maximum Single Day Fire Growth") +
  annotate("text", label = "Maximum Single Day Fire Growth", x=400000, y=700000,
           size = 12) +
  theme(#plot.title = element_text(hjust = 0.1, size = 30, face = "plain"
                              #    ,margin=margin(0,0,0,0)
                               #   ),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.text = element_text(size = 18, face = "plain"),
        legend.title = element_text(size = 20, face= "plain"),
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
  ggsave(file = file.path(draft_figs_dir, "proposal_fig.png"),
         width = 12, height = 7,dpi = 300);proposal_fig


# which ecoregions have red dots? ==============================================
ecos3 <- st_read("data/bounds/ecoregions/us_eco_l3/us_eco_l321.gpkg") %>%
  st_transform(crs = st_crs(modis_fish_ff))
red_dots <- modis_fish_ff %>%
  st_as_sf(coords = c("long", "lat")) %>%
  filter(max_max_growth > 10000) %>%
  st_transform(crs=st_crs(ecos3)) 

x1<-st_intersects(ecos3, red_dots,sparse = F) %>%
  rowSums()

ecos3 <- ecos3 %>%
  mutate(n_fast_fires = x1)

ecos_w_red_dots <- ecos3 %>%
  filter(n_fast_fires >0)


ggplot() +
  geom_sf(data = ecos_w_red_dots) +
  geom_sf(data = red_dots, col = "red") +
  theme_void()

ecos_w_red_dots %>%
  dplyr::select(us_l3name, n_fast_fires)%>%
  st_set_geometry(NULL) %>%
  arrange(desc(n_fast_fires)) %>%
  write_csv("results/ecos_w_red_dots.csv")
