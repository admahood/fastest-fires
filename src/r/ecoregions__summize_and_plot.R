# calculating ecoregion fsr stuff, making figure

source("src/r/a_prep_environment.R")
source("src/r/b_import_clean_data.R")
library(tidyverse)
library(ggpubr)
library(nngeo) # for st_remove_holes
library(mblm)

# level 1 ======================================================================
options(stringsAsFactors = FALSE)
ecoregion_summary <- modis_events %>%
  #removing geometry speeds up the process a lot
  st_set_geometry(NULL) %>% 
  group_by(l1_ecoregion) %>%
  summarise(avg_fsr = fsr_ha_per_day %>% mean(na.rm=TRUE),
            avg_max_growth = max_growth_ha %>% mean(na.rm = TRUE)) %>%
  ungroup() %>%
  rename(na_l1name = l1_ecoregion) %>%
  filter(!is.na(na_l1name))

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
        legend.direction = "horizontal") +
  ggsave("results/draft_figures/avg_max_fsr_l3.png")

# temporal trends ==============================================================

ecoregion_temporal <- modis_events %>%
  st_set_geometry(NULL) %>% 
  mutate(ignition_year = as.numeric(as.character(ignition_year)))%>%
  filter(!is.na(l1_ecoregion) &
           !is.na(lc_name)) %>%
  group_by(l1_ecoregion, ignition_year, lc_name) %>%
  summarise(n = n(),
            peak_season = mean(as.numeric(ignition_month)),
            season_length = sd(ignition_date) *2,
            avg_fsr = fsr_ha_per_day %>% median(na.rm=TRUE),
            avg_max_growth = max_growth_ha %>% median(na.rm = TRUE),
            max_growth = max_growth_ha %>% max(na.rm = TRUE)) %>%
  ungroup() %>%
  rename(na_l1name = l1_ecoregion) %>%
  filter(lc_name != "Barren",
         lc_name != "Water Bodies",
         lc_name != "Permanent Wetlands",
         lc_name != "Permanent Snow and Ice",
         na_l1name != "Northern Forests");ecoregion_temporal

mods <- ecoregion_temporal %>%
  nest(-na_l1name) %>%
  mutate(fit = map(data, ~ lm(avg_fsr ~ ignition_year*lc_name,
                              data = .x)),
         results = map(fit, glance)) %>%
  unnest(results) %>%
  left_join(label_positions)

# just look at it
ggplot(filter(ecoregion_temporal, n>5)) +
  geom_point(aes(x=ignition_year, y=avg_fsr, color = lc_name)) +
  geom_smooth(aes(x=ignition_year, y=avg_fsr, color = lc_name),
              method="lm", se=FALSE) +
  
  facet_wrap(~na_l1name, scales = "free_y") +
  geom_label(data = mods, x=-Inf, y = Inf, 
            aes(group = na_l1name,
                label = paste('R2:', round(r.squared, 3), "\n",
                              "p:", round(p.value, 4))),
            hjust = 0,
            vjust = 1,
            alpha=0.75) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggsave("results/draft_figures/eco_lc_avg_fsr_trends.png")

mods1 <- ecoregion_temporal %>%
  nest(-na_l1name) %>%
  mutate(fit = map(data, ~ lm(log(max_growth) ~ ignition_year*lc_name,
                              data = .x)),
         results = map(fit, glance)) %>%
  unnest(results)

ggplot(filter(ecoregion_temporal, n>5)) +
  geom_point(aes(x=ignition_year, y=log(max_growth), color = lc_name)) +
  geom_smooth(aes(x=ignition_year, y=log(max_growth), color = lc_name),
              method="lm", se=FALSE) +
  facet_wrap(~na_l1name, scales = "free_y") +
  geom_label(data = mods1, x=-Inf, y = Inf, 
             aes(group = na_l1name,
                 label = paste('R2:', round(r.squared, 3), "\n",
                               "p:", round(p.value, 4))),
             hjust = 0,
             vjust = 1,
             alpha=0.75) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  ggsave("results/draft_figures/eco_lc_max_growth_trends.png")

# landcover --------------------------------------------------------------------

lc_temporal <- modis_events %>%
  mutate(ignition_year = as.numeric(as.character(ignition_year)),
         lc_name = as.character(lc_name)) %>%
  filter(ignition_year < 2019) %>%
  filter(lc_name == "Grasslands" |
           lc_name == "Woody Savannas" |
           lc_name == "Evergreen Needleleaf Forests") %>%
  st_set_geometry(NULL) %>% 
  mutate(ignition_year = as.numeric(as.character(ignition_year)))%>%
  filter(!is.na(l1_ecoregion) &
           !is.na(lc_name)) %>%
  group_by(ignition_year, lc_name) %>%
  summarise(n = n(),
            peak_season = mean(as.numeric(ignition_month)),
            season_length = sd(ignition_date) *2,
            avg_fsr = fsr_ha_per_day %>% median(na.rm=TRUE),
            avg_max_growth = max_growth_ha %>% median(na.rm = TRUE),
            max_growth = max_growth_ha %>% max(na.rm = TRUE)) %>%
  ungroup() #%>%
  # rename(na_l1name = l1_ecoregion) %>%
  # filter(lc_name != "Barren",
  #        lc_name != "Water Bodies",
  #        lc_name != "Permanent Wetlands",
  #        lc_name != "Permanent Snow and Ice",
  #        na_l1name != "Northern Forests");lc_temporal

mods_lc <- lc_temporal %>%
  nest(-lc_name) %>%
  mutate(fit = map(data, ~ lm(log(max_growth) ~ ignition_year,
                              data = .x)),
         results = map(fit, glance)) %>%
  unnest(results)

# for the grant proposal
ggplot(lc_temporal, aes(x=ignition_year, y = log(max_growth))) +
  geom_point() +
  facet_wrap(~lc_name, scales = "free_y", nrow = 3, ncol=1) +
  geom_smooth(method = "lm") +
  geom_label(data = mods_lc, x=-Inf, y = Inf, 
             aes(group = lc_name,
                 label = paste('R2:', round(r.squared, 3), "\n",
                               "p:", round(p.value, 4))),
             hjust = 0,
             vjust = 1,
             alpha=0.75) +
  theme_pubr() +
  xlab("Ignition Year") +
  ylab("Max Growth (Natural Log)") +
  ggsave("images/fig_proposal_temp_trends.png",
         width = 4, height = 9)

# sesonality ===================================================================

ggplot(filter(ecoregion_temporal, n>5)) +
  geom_point(aes(x=ignition_year, y=peak_season, color = lc_name)) +
  geom_smooth(aes(x=ignition_year, y=peak_season, color = lc_name),
              method="lm", se=FALSE) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  facet_wrap(~na_l1name, scales = "free_y") +
  ggsave("results/draft_figures/peak_season_trend.png")

ggplot(filter(ecoregion_temporal, n>5)) +
  geom_point(aes(x=ignition_year, y=season_length, color = lc_name)) +
  geom_smooth(aes(x=ignition_year, y=season_length, color = lc_name),
              method="lm", se=FALSE) +
  theme_pubr() +
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  facet_wrap(~na_l1name, scales = "free_y") +
  ggsave("results/draft_figures/season_length_trend.png")

ggplot(modis_events %>% filter(ignition_year != "2019",
                               is.na(l1_ecoregion) == FALSE), 
       aes(x = as.numeric(as.character(ignition_doy)), 
                        color = ignition_year)) +
  geom_density() +
  theme_pubr() +
  scale_color_viridis_d()+
  facet_wrap(~l1_ecoregion, scales="free_y", nrow = 2)

modis_events <- modis_events %>%
  mutate(temp_bin = ifelse(as.numeric(as.character(ignition_year)) <2010,
                           "2000-2009","2010-2019"))

mods2 <- modis_events %>%
  filter(!is.na(l1_ecoregion)) %>%
  mutate(ignition_doy = as.numeric(as.character(ignition_doy))) %>%
  nest(-l1_ecoregion) %>%
  mutate(fit = map(data, ~ kruskal.test(.x$ignition_doy ~ .x$temp_bin,
                              data = .x)),
         results = map(fit, glance)) %>%
  unnest(results)

ggplot(modis_events %>% filter(ignition_year != "2019",
                               is.na(l1_ecoregion) == FALSE)) +
  geom_density(aes(x = as.numeric(as.character(ignition_doy)), 
                   color = temp_bin)) +
  theme_pubr() +
  geom_label(data = mods2, x=-Inf, y = Inf, 
             aes(group = l1_ecoregion,
                 label = paste("Kruskal-Wallis: p =", round(p.value, 4))),
             hjust = 0,
             vjust = 1,
             alpha=0.75) +
  facet_wrap(~l1_ecoregion, scales="free_y", nrow = 2) +
  ggsave("results/draft_figures/densityplots_seasonality.png")

# divide between winter / summer months
ggplot(data = ecoregion_temporal %>% filter(na_l1name == "North American Deserts"),
       aes(x = ignition_year, y = avg_fsr, color = lc_name)) +
  facet_wrap(~lc_name, scales = "free")+
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")+
  geom_smooth(method = "lm")


# size vs speed ================================================================
library(lme4)
library(lmerTest)
lmer(fsr_km2_per_day ~ total_area_km2 + (1|lc_name), 
     data = modis_events) %>% summary
lm(fsr_km2_per_day ~ total_area_km2*lc_name, data = modis_events) %>% summary

ggplot(filter(modis_events, !is.na(l1_ecoregion)), 
       aes(x=total_area_ha, y=fsr_ha_per_day, color = lc_name)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_pubr()
  
ggplot(filter(modis_events, !is.na(l1_ecoregion), lc_name != "Water Bodies",
              lc_name != "Permanent Snow and Ice", lc_name != "Barren"), 
       aes(x=total_area_km2, y=fsr_km2_per_day, color = lc_name)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_pubr() +
  theme(legend.title = element_blank(),
        legend.position = c(1,0),
        legend.justification = c(1,0))+
  guides(color=guide_legend(ncol=2)) +
  facet_wrap(~l1_ecoregion, scales = "free")+
  ggtitle("Fire Spread Rate ~ Fire Size")+
  ggsave("results/draft_figures/fsr-total_area.png")

ggplot(filter(modis_events, !is.na(l1_ecoregion), lc_name != "Water Bodies",
              lc_name != "Permanent Snow and Ice", lc_name != "Barren"), 
       aes(x=total_area_km2, y=max_growth_km2,
                         color = lc_name)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~l1_ecoregion, scales = "free") +
  theme_pubr() +
  theme(legend.title = element_blank(),
        legend.position = c(1,0),
        legend.justification = c(1,0))+
  guides(color=guide_legend(ncol=2)) +
  ggtitle("Max Single Day Growth ~ Fire Size")+
  ggsave("results/draft_figures/max_growth-total_area.png")

ggplot(filter(modis_events, !is.na(l1_ecoregion), lc_name != "Water Bodies",
              lc_name != "Permanent Snow and Ice", lc_name != "Barren"), 
       aes(x=total_area_km2, y=max_growth_km2,
           color = lc_name)) +
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.5) +
  facet_wrap(~lc_name) +
  theme_pubr() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.justification = c(1,0))+
  #guides(color=guide_legend(ncol=4)) +
  ggtitle("Max Single Day Growth ~ Fire Size")+
  ggsave("results/draft_figures/max_growth-total_area_lc_only.png")
