# calculating ecoregion fsr stuff, making figure

source("src/r/a_prep_environment.R")
source("src/r/b_import_clean_data.R")
library(tidyverse)
library(nngeo) # for st_remove_holes
library(mblm)

# level 1 ======================================================================

ecoregion_summary <- modis_events %>%
  #removing geometry speeds up the process a lot
  st_set_geometry(NULL) %>% 
  group_by(l1_ecoregion) %>%
  summarise(avg_fsr = fsr_ha_per_day %>% mean(na.rm=TRUE),
            avg_max_growth = max_growth_ha %>% mean(na.rm = TRUE)) %>%
  ungroup() %>%
  rename(na_l1name = l1_ecoregion)

ecoregions_l1 <- ecoregions_l321 %>%
  mutate(na_l1name = stringr::str_to_title(na_l1name)) %>%
  group_by(na_l1name) %>%
  summarise() %>%
  ungroup() %>%
  nngeo::st_remove_holes() %>%
  na.omit() %>%
  left_join(ecoregion_summary)

ggplot() +
  geom_sf(data = ecoregions_l1, aes(fill = avg_fsr), color = "transparent") +
  scale_fill_viridis(name = "Mean FSR (ha/day)") +
  theme_void() +
  theme(legend.position = "bottom")

ggplot() +
  geom_sf(data = ecoregions_l1, aes(fill = avg_max_growth), color = "transparent") +
  scale_fill_viridis(name = "Avg Maximum Daily Growth (ha)") +
  theme_void() +
  theme(legend.position = "bottom")

# level 3 ======================================================================

fired_l3_summary <- fired_ecoreg %>%
  st_set_geometry(NULL) %>%
 # mutate(us_l3name = stringr::str_to_title(us_l3name)) %>%
  group_by(us_l3name) %>%
  summarise(avg_fsr = fsr_ha_per_day %>% mean(na.rm=TRUE),
            avg_max_growth = max_growth_ha %>% mean(na.rm = TRUE)) %>%
  ungroup()

ecoregions_l3 <- ecoregions_l321 %>%
  #mutate(us_l3name = stringr::str_to_title(us_l3name)) %>%
  group_by(us_l3name) %>%
  summarise() %>%
  ungroup() %>%
  nngeo::st_remove_holes() %>%
  na.omit() %>%
  left_join(fired_l3_summary)

ggplot() +
  geom_sf(data = ecoregions_l3, aes(fill = avg_fsr), color = "transparent") +
 # scale_fill_viridis(option = "B",name = "Mean FSR (ha/day)") +
  scale_fill_gradient2_tableau(palette =  "Red-Blue Diverging",
                               trans = "reverse",
                               name = "Mean FSR (ha/day)") +
  theme_void() +
  theme(legend.position = c(0.2,0.1),
        legend.direction = "horizontal") +
  ggsave("results/draft_figures/mean_fsr_l3.png")

ggplot() +
  geom_sf(data = ecoregions_l3, aes(fill = avg_max_growth), color = "transparent") +
  #scale_fill_viridis(option = "B",name = "Avg Maximum Daily Growth (ha)") +
  scale_fill_gradient2_tableau(palette =  "Red-Blue Diverging",
                               trans = "reverse",
                               name = "Avg Maximum Daily Growth (ha)") +
  theme_void() +
  theme(legend.position = c(0.2,0.1),
        legend.direction = "horizontal")

# temporal trends ==============================================================

ecoregion_temporal <- modis_events %>%
  st_set_geometry(NULL) %>% 
  mutate(ignition_year = as.numeric(as.character(ignition_year)))%>%
  filter(is.na(l1_ecoregion) == F &
           is.na(lc_name) == F) %>%
  group_by(l1_ecoregion, ignition_year, lc_name) %>%
  summarise(avg_fsr = fsr_ha_per_day %>% mean(na.rm=TRUE),
            avg_max_growth = max_growth_ha %>% mean(na.rm = TRUE)) %>%
  ungroup() %>%
  rename(na_l1name = l1_ecoregion) 
mods <- ecoregion_temporal %>%
  nest(-na_l1name) %>%
  mutate(fit = map(data, ~ lm(avg_fsr ~ ignition_year*lc_name,
                              data = .x)),
         results = map(fit, glance)) %>%
  unnest(results)

dplyr::arrange(mods, desc(r.squared))

# divide between winter / summer months
ggplot(data = ecoregion_temporal %>% filter(na_l1name == "North American Deserts"),
       aes(x = ignition_year, y = avg_fsr, color = lc_name)) +
  facet_wrap(~lc_name, scales = "free")+
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")+
  geom_smooth(method = "lm")
