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
  geom_sf(data = ecoregions_l1, aes(fill = avg_fsr)) +
  scale_fill_viridis()

ggplot() +
  geom_sf(data = ecoregions_l1, aes(fill = avg_max_growth)) +
  scale_fill_viridis()

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
