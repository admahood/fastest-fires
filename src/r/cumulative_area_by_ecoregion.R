# to do:
# get axis labels on x and y axes
# convert to acres or ha

# setup ------------------------------------------------------------------------
libs <- c("tidyverse", "sf", "ggpubr", "cowplot")
lapply(libs, library, character.only = TRUE)
options(stringsAsFactors = FALSE)
daily_file <- Sys.glob("data/daily_stats*")
ecoregion_file <- "data/bounds/ecoregions/us_eco_l3/us_eco_l321.gpkg"

# data processing --------------------------------------------------------------

# this takes a few minutes
ecoregions <- st_read(ecoregion_file) %>% 
  group_by(na_l1name) %>%
  summarise() %>%
  ungroup() %>%
  nngeo::st_remove_holes() %>%
  mutate(na_l1name = str_to_title(na_l1name),
         na_l1name = replace(na_l1name,
                             na_l1name == "Northwestern Forested Mountains",
                             "Northwestern Forested Mtns"))%>%
  na.omit()
  

elabs <- ecoregions$na_l1name

#merging some classes together
#table(daily$lc_name)
daily <- read_csv(daily_file)%>%
  filter( lc_name != "Permanent Snow and Ice",
                lc_name != "Water Bodies",
                lc_name != "Urban and Built-up Lands",
                lc_name != "Barren",
                lc_name != "Permanent Wetlands") %>%
  dplyr::mutate(lc_name = replace(lc_name, 
                                  lc_name == "Cropland/Natural  Vegetation  Mosaics",
                                  "Crops/Natural Veg"),
                lc_name = replace(lc_name, lc_name == "Woody Savannas", "Savannas"),
                lc_name = replace(lc_name, lc_name == "Closed Shrublands", "Shrublands"),
                lc_name = replace(lc_name, lc_name == "Open Shrublands", "Shrublands"),
                lc_name = replace(lc_name, lc_name == "Deciduous Broadleaf Forests", 
                                  "Broadleaf Forests"),
                lc_name = replace(lc_name, lc_name == "Evergreen Broadleaf Forests", 
                                  "Broadleaf Forests"),
                lc_name = replace(lc_name, lc_name == "Evergreen Needleleaf Forests", 
                                  "Conifer Forests"),
                l1_ecoregion = replace(l1_ecoregion, 
                                       l1_ecoregion == "Northwestern Forested Mountains",
                                       "Northwestern Forested Mtns"))

# plotting ---------------------------------------------------------------------

# this makes the legend colors consistent
values <- RColorBrewer::brewer.pal(length(unique(daily$lc_name)), "Set1")
values[6] <- "gold"
names(values) <- unique(daily$lc_name)

er <- list()
da <- list()
cp <- list()
labs <- vector()
for(i in 1:length(ecoregions$na_l1name)){
  er[[i]] <- ggplot(ecoregions) + 
    geom_sf(data = ecoregions,fill = "transparent", color = "grey", size=0.1) +
    geom_sf(data = filter(ecoregions, na_l1name == ecoregions$na_l1name[i]), 
            color = "black", size=0.3)+
    theme_void()+
    theme(panel.grid.major = element_line(color = "transparent"))
  
  da[[i]]<- ggplot(data = filter(daily, l1_ecoregion == ecoregions$na_l1name[i]), 
                   aes(x=event_day, cum_area_km2, group = id)) +
    geom_line(alpha=0.75, aes(color = lc_name)) +
    scale_color_manual(values = values)+
    theme_pubr() +
    xlab("") +
    ylab("") +
    theme(legend.position = "none") +
    ylim(c(0,2620)) +
    xlim(c(0,213)) +
    ggtitle(ecoregions$na_l1name[i]) +
    theme(plot.title = element_text(size = 10),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  cp[[i]] <- ggdraw() +
    draw_plot(da[[i]]) +
    draw_plot(er[[i]], x = 0.55, y=0.5, width = 0.45, height=0.45)
  
  labs[i] <- ecoregions$na_l1name[i]
}

#grabbing the legend as an independent object
leg <- get_legend(ggplot(daily, aes(x=event_day, cum_area_km2, 
                                    group = id)) +
                    geom_point(aes(color = lc_name), shape = 15, size=5) +
                    theme_pubr()+
                    scale_color_manual(values=values,
                                       name = "")+
                    guides(color=guide_legend(ncol=2)))

# multistage, super advanced plotting process
x <- ggarrange(plotlist=cp, nrow=3, ncol=4) %>%
  annotate_figure(left = text_grob(expression(Cumulative~Area~Burned), rot = 90, size = 20),
                  bottom = text_grob("Days From Ignition", size = 20)) 

y <- ggdraw() +
  draw_plot(x) +
  draw_plot(leg, x=0.6, y=0.1, width = 0.4, height = .25)+
  ggsave("images/ecoregions_cum_area.pdf", 
         dpi=600, width = 10, height = 7.5)

y <- ggdraw() +
  draw_plot(x) +
  draw_plot(leg, x=0.6, y=0.1, width = 0.4, height = .25)+
  ggsave("images/ecoregions_cum_area.png", 
         dpi=300, width = 10, height = 7.5)
