# missing the object s3_base which is used in script c_import_clean_data.R

packages <- c("tidyverse", "magrittr", "raster", "RCurl", "sf", "assertthat", 'lubridate', 
              'viridis', 'lwgeom', 'scales', 'velox', 'ggmap','RColorBrewer', 'gridExtra', 
              'ggthemes', 'broom', 'stars')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
  lapply(packages, library, character.only = TRUE, verbose = FALSE) 
} else {
  lapply(packages, library, character.only = TRUE, verbose = FALSE) 
}

# load all functions
file_sources <- list.files(file.path('src', 'functions'), pattern="*.R", 
                           full.names=TRUE, ignore.case=TRUE)
invisible(sapply(file_sources, source, .GlobalEnv))

p4string_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

# Raw data folders
data_dir <- "data"
raw_dir <- file.path(data_dir, "raw")
mtbs_raw <- file.path(raw_dir, "mtbs")
us_raw_dir <- file.path(raw_dir, "cb_2016_us_state_20m")
ecoregion_raw_dir <- file.path(raw_dir, "ecoregions")
ecoregionl3_raw_dir <- file.path(raw_dir, "us_eco_l3")

var_dir <- list(data_dir, raw_dir, us_raw_dir, mtbs_raw, ecoregion_raw_dir, ecoregionl3_raw_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# Output folders
fire_dir <- file.path(data_dir, 'fire')
modis_event_dir <- file.path(fire_dir, "modis_events")
stat_out <- file.path(modis_event_dir, 'lvl1_eco_stats')

bounds_dir <- file.path(data_dir, "bounds")
fishnet_dir <- file.path(bounds_dir, 'fishnet')
ecoreg_dir <- file.path(bounds_dir, "ecoregions")
ecoregionl3_dir <- file.path(ecoreg_dir, "us_eco_l3")
anthro_dir <- file.path(data_dir, 'anthro')
ztrax_dir <- file.path(anthro_dir, 'ztrax')
transportation_dir <- file.path(anthro_dir, 'transportation')

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(fire_dir, modis_event_dir, stat_out, raw_dir,
                bounds_dir, fishnet_dir, ecoreg_dir, ecoregionl3_dir,
                anthro_dir, ztrax_dir, transportation_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# Figures and Tables
results_dir <- 'results'
draft_figs_dir <- file.path(results_dir, 'draft_figures')

var_dir <- list(results_dir, draft_figs_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

#s3 paths (for modis events)
s3_base <- "s3://earthlab-amahood/fastest-fires/data"
s3_events_path <- "s3://earthlab-natem/modis-burned-area/delineated_events"

system(paste("aws s3 sync", s3_base, "data"))
