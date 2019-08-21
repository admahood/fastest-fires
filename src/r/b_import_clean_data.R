# Import and prep the USA shapefile ============================================
if (!exists("states")){
  file_download(shp_path_name = file.path(us_raw_dir, 'cb_2016_us_state_20m.shp'),
                          shp_dir = us_raw_dir,
                          url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip")
    
  states <- st_read(file.path(us_raw_dir, 'cb_2016_us_state_20m.shp')) %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR")) %>%
    dplyr::select(STATEFP, STUSPS) %>%
    rename_all(tolower)
}

# 50k Fishnet ==================================================================
if (!exists("fishnet_50k")) {
  if (!file.exists(file.path(fishnet_dir, "fishnet_50k.gpkg"))) {
    fishnet_50k <- sf::st_make_grid(states, cellsize = 50000, what = 'polygons') %>%
      sf::st_sf('geometry' = ., data.frame('fishid50k' = 1:length(.))) %>%
      sf::st_intersection(., st_union(states))
    
    sf::st_write(fishnet_50k,
                 file.path(fishnet_dir, "fishnet_50k.gpkg"),
                 driver = "GPKG")

  } else {
    fishnet_50k <- sf::st_read(file.path(fishnet_dir, "fishnet_50k.gpkg"))
  }
}

# Download and import the Level 3 Ecoregions data, which has Levels 3, 2, 1 ====
# Download will only happen once as long as the file exists
if (!exists("ecoregions_l321")) {
  
  if(!file.exists(file.path(ecoregionl3_raw_dir, 'us_eco_l3.shp'))) {
    file_download(shp_path_name = file.path(ecoregionl3_raw_dir, 'us_eco_l3.shp'),
                  shp_dir = ecoregionl3_raw_dir,
                  url = "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip")
  }
  
  ecoregions_l321 <- st_read(file.path(ecoregionl3_raw_dir, 'us_eco_l3.shp')) %>%
    sf::st_transform(st_crs(states)) %>%
    st_make_valid() %>%
    mutate(NA_L2NAME = case_when(
      NA_L2NAME == 'UPPER GILA MOUNTAINS (?)' ~ 'UPPER GILA MOUNTAINS',
      TRUE ~ as.character(NA_L2NAME)),
      NA_L2NAME = as.factor(NA_L2NAME)) %>%
    group_by(US_L3NAME, NA_L2NAME, NA_L1NAME) %>%
    summarise() %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 100) %>%
    mutate(region = if_else(NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS",
                                                       "TROPICAL WET FORESTS",
                                                       "NORTHERN FORESTS"), "East",
                                      if_else(NA_L1NAME %in% c("NORTH AMERICAN DESERTS",
                                                               "SOUTHERN SEMI-ARID HIGHLANDS",
                                                               "TEMPERATE SIERRAS",
                                                               "MEDITERRANEAN CALIFORNIA",
                                                               "NORTHWESTERN FORESTED MOUNTAINS",
                                                               "MARINE WEST COAST FOREST"), "West", "Central"))) %>%
    rename_all(tolower)
  
  ecoregions_l321 %>%
    st_write(., file.path(ecoregionl3_dir, 'us_eco_l321.gpkg'), driver = 'GPKG', delete_layer = TRUE)
  } else {
    ecoregions_l321 <- st_read(file.path(ecoregionl3_dir, 'us_eco_l321.gpkg'))
  }

# Import and clean the MTBS polygons ===========================================
if (!exists('mtbs_fire')) {
  
  mtbs_shp <- file.path(mtbs_raw, 'mtbs_perimeter_data')
  if (!file.exists(mtbs_shp)) {
    file_download(file.path(mtbs_raw, 'mtbs_perimeter_data','mtbs_perims_DD.shp'),
                  mtbs_raw, "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip")
    
  }
  
  mtbs_fire <- st_read(dsn = file.path(mtbs_raw),
                       layer = 'mtbs_perims_DD', quiet= TRUE) %>%
    filter(Year >= '2001') %>%
    st_transform(st_crs(states)) %>%
    mutate(discovery_date = ymd(paste(Year, StartMonth, StartDay, sep="-")),
           discovery_year = year(discovery_date),
           discovery_day = day(discovery_date),
           discovery_month = month(discovery_date),
           discovery_doy = yday(discovery_date)) %>%
    st_intersection(., st_union(states)) %>%
    rename_all(tolower) %>%
    dplyr::select(fire_id, fire_name, discovery_date, discovery_year, discovery_day, discovery_month, discovery_doy, acres) %>%
    # Below we are categorizing the fires as in the East or West based on the -97th parallel - which is what MTBS uses
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sfc_as_cols(., st_centroid(geometry)) %>%
    mutate(mtbs_region = ifelse(x < -97, 'West', 'East')) %>%
    dplyr::select(-x, -y) %>%
    st_transform(st_crs(states))
}

if(!file.exists(file.path(fire_dir, 'mtbs_ecoreg.gpkg'))) {
  mtbs_ecoreg <- mtbs_fire %>%
    st_intersection(., ecoregions_l321) %>%
    mutate(mtbs_ba_ha = acres*0.404686,
           mtbs_ba_ecoreg_ha = as.numeric(st_area(geometry))*0.0001) 
  
  mtbs_ecoreg %>%
    st_write(., file.path(fire_dir, 'mtbs_ecoreg.gpkg'), delete_layer = TRUE)
  
  system(paste0('aws s3 sync data' , ' ', s3_base))
  
} else {
  mtbs_ecoreg <- st_read(file.path(fire_dir, 'mtbs_ecoreg.gpkg'))
}

# get modis event polygons =====================================================
if(!file.exists(file.path(modis_event_dir, "modis_event_polygons_cus.gpkg"))){
  system(paste("aws s3 cp",
               file.path(s3_events_path, "modis_event_polygons_cus.gpkg"),
               file.path(modis_event_dir, "modis_event_polygons_cus.gpkg")))
}
modis_events <- st_read(file.path(modis_event_dir, "modis_event_polygons_cus.gpkg"))

