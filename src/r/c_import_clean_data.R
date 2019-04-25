# Import and prep the USA shapefile
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

# Download and import the Level 3 Ecoregions data, which has Levels 3, 2, 1
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

# Import and clean the MTBS polygons
if (!exists('mtbs_fire')) {
  
  mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perimeter_data_v2','dissolve_mtbs_perims_1984-2015_DD_20170501.shp')
  if (!file.exists(mtbs_shp)) {
    file.download(file.path(mtbs_prefix, 'mtbs_perimeter_data_v2','dissolve_mtbs_perims_1984-2015_DD_20170501.shp'),
                  mtbs_prefix, "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip")
    
  }
  
  mtbs_fire <- st_read(dsn = file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'),
                       layer = 'dissolve_mtbs_perims_1984-2015_DD_20170501', quiet= TRUE) %>%
    filter(Year >= '2001') %>%
    st_transform(p4string_ea) %>%
    mutate(discovery_date = ymd(paste(Year, StartMonth, StartDay, sep="-")),
           discovery_year = year(discovery_date),
           discovery_day = day(discovery_date),
           discovery_month = month(discovery_date),
           discovery_doy = yday(discovery_date)) %>%
    st_intersection(., st_union(usa)) %>%
    rename_all(tolower) %>%
    dplyr::select(fire_id, fire_name, discovery_date, discovery_year, discovery_day, discovery_month, discovery_doy, acres) %>%
    # Below we are categorizing the fires as in the East or West based on the -97th parallel - which is what MTBS uses
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sfc_as_cols(., st_centroid(geometry)) %>%
    mutate(mtbs_region = ifelse(x < -97, 'West', 'East')) %>%
    dplyr::select(-x, -y) %>%
    st_transform(p4string_ea)
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
  
# Import and polygonize the modis event data
if(!file.exists(file.path(modis_event_dir, 'usa_modis_events_2001_2017.gpkg'))) {
  yr_list <- rep(2001:2017)
  
  library(foreach)
  library(doParallel)
  
  cl <- parallel::makeCluster(parallel::detectCores()/2)
  doParallel::registerDoParallel(cl)
  
  full_poly <- foreach(y = unique(yr_list), .combine = rbind, .packages = c('foreach')) %dopar% {
    rst_list <- list.files(modis_event_dir, pattern = paste0(y), full.names = TRUE)
    
    full_df <- foreach(x = rst_list, .combine = cbind, .packages = c('foreach', 'tidyverse', 'raster', 'sf', 'velox', 'stars')) %do% {
      
      file_basename <- x %>%
        basename %>%
        strsplit(split = "\\.") %>%
        unlist
      
      file_split <- file_basename[1] %>%
        strsplit(split = "_") %>%
        unlist
      
      yr <- file_split[3]
      var <- file_split[4]
      
      rst <- raster(x) 
      rst[rst == 0] <- NA
      
      poly_out <- rst %>%
        st_as_stars() %>%
        st_as_sf() %>%
        mutate(year = yr) 
      
      if(var == 'events') {
        poly_out <- poly_out %>%
          mutate(events = paste0(layer, '_', year)) %>%
          dplyr::select(events, year)
      } else if('layer' %in% colnames(poly_out)) {
        poly_out <- poly_out %>%
          dplyr::select(!!ensym(var) := layer, year)
      } else {
        poly_out <- poly_out %>%
          dplyr::select(!!ensym(var) := file_basename[1], year)
      }
      return(poly_out)
    }
    return(full_df)
  }
  parallel::stopCluster(cl)
  
  usa_modis_events <- full_poly %>%
    dplyr::select(-contains('.')) %>%
    group_by(events) %>%
    summarise_all(mode)
  st_write(usa_modis_events, file.path(modis_event_dir, 'usa_modis_events_2001_2017.gpkg'))
} else {
  usa_modis_events <- st_read(file.path(modis_event_dir, 'usa_modis_events_2001_2017.gpkg'))
  }

