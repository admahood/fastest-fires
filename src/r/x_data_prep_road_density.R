#Road Density script
# this downloads the road files for each county in the 
# continental US and calculates road density for 1km pixels
library(tidyverse)
library(sf)
library(doParallel)
library(raster)
library(foreach)
library(spex)
library(fasterize)

system(paste("aws s3 sync s3://earthlab-amahood/fastest-fires/data/counties data/background/counties --only-show-errors"))
system(paste("aws s3 cp s3://earthlab-amahood/fastest-fires/data/home_density.tif data/home_density.tif --only-show-errors"))


source("src/r/a_prep_environment.R")

counties <- st_read("data/background/counties/")
homes_per_road <- raster("data/home_density.tif")
blank_raster <- homes_per_road
blank_raster[] <- 0

if (!exists("states")){
  shp_path_name <- file.path(us_raw_dir, 'cb_2016_us_state_20m.shp')
  if(!file.exists(shp_path_name)){
    file_download(shp_path_name = shp_path_name,
                  shp_dir = us_raw_dir,
                  url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip")
  }
  states <- st_read(file.path(us_raw_dir, 'cb_2016_us_state_20m.shp')) %>%
    sf::st_transform(crs=st_crs(counties)) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR")) %>%
    dplyr::select(STATEFP, STUSPS) %>%
    rename_all(tolower)
}


# A few counties didn't have road layers. They're not in CUS so no worries
# counties[counties$GEOID == 69085,]
# counties[counties$GEOID == 60030,]
bb<- st_bbox(states) %>%
  st_as_sfc

counties <- counties %>%
  filter(GEOID != 69085, GEOID != 60030) %>%
  mutate(STATEFP = as.numeric(STATEFP)) %>%
  filter(STATEFP < 57)  %>%
  st_intersection(bb)
geoids<-counties$GEOID %>% as.character

basep <- "ftp://ftp2.census.gov/geo/tiger/TIGER2019/ROADS/tl_2019_"
endp <- "_roads.zip"

#dir.create("data/background/roads/counties", recursive=T)

# first, downloading the roads for each county
dir.create("data/background/roads/counties", recursive =TRUE)
registerDoParallel(detectCores()-1)
foreach(i = geoids)%dopar%{
  outfile<- paste0("data/background/roads/counties/roads_",i, ".zip" )
  if(!file.exists(outfile)){
    download.file(url = paste0(basep, i, endp),
                  destfile = outfile,quiet = TRUE)
  }
}
list.files("data/background/roads/counties/") %>% length
geoids%>% length

registerDoParallel(detectCores()-1)
foreach(i = geoids)%dopar%{
  exdir<- paste0("data/background/roads/counties/",i, "/" )
  outfile<- paste0("data/background/roads/counties/roads_",i, ".zip" )
  if(!dir.exists(exdir)){
    dir.create(exdir)
    unzip(outfile, exdir = exdir)
    unlink(outfile)
  }
}


road_dirs <- list.files("data/background/roads/counties", full.names = TRUE)

# making sure everything's there
for(i in road_dirs){
  if(str_extract(i, "\\d{5}")%>%str_sub( 1,2) %>% as.numeric() < 57){
   x<-list.files(i)
    if(length(x)<1){
   print(i)}
  }
}

dir.create("data/background/roads/rd_tifs")
for(i in road_dirs){
  t0<- Sys.time()
  county<-str_extract(i, "\\d{5}")
  if(county %in% geoids){
  outfile <- paste0("data/background/roads/rd_tifs/",
                    "road_density_km_km2_",county,".tif")
  if(!file.exists(outfile)){
    sl<- st_read(i, quiet=T) %>%
      st_transform(crs=crs(blank_raster, asText = TRUE))
    br<-crop(blank_raster, as(sl, "Spatial"))
    x<-br %>%
      spex::polygonize() %>%
      dplyr::select(-home_density) %>%
      mutate(road_density_km_km2 = 0,
             FID = 1:nrow(.))
    
    registerDoParallel(detectCores()-1)
    z<-foreach(c = 1:nrow(x), .combine = rbind) %dopar%{
      y<-x[c,]
      lc <- st_intersection(sl, y) %>%
        st_length() %>% 
        sum() %>% 
        as.numeric()
      y[1,"road_density_km_km2"] <- lc/1000
      return(y %>% st_set_geometry(NULL) )
    }
    zz<- z %>%
      left_join(x %>% dplyr::select(-road_density_km_km2), by="FID") %>%
      dplyr::select(-FID) %>%
      st_as_sf()
    fasterize(zz, br, field = "road_density_km_km2") %>%
      writeRaster(filename=outfile)
    print(paste(which(road_dirs == i), "out of",length(road_dirs)))
    print(Sys.time() - t0)
    
    system(paste("aws s3 cp", outfile, 
                 file.path("s3://earthlab-amahood/fastest-fires",outfile),
                 "--only-show-errors"))
  }
  }
}

road_tifs <- list.files("data/background/roads/rd_tifs", full.names =T)
final_raster <- blank_raster

for(i in 1:length(road_tifs)){
  r<- raster(road_tifs[i]) %>% 
    extend(blank_raster) 
  r[is.na(r[])] <- 0 
  final_raster <- final_raster + r
}

writeRaster(final_raster, "data/road_denisity_km_km2.tif")

system(paste("aws s3 cp", "data/road_denisity_km_km2.tif", 
             file.path("s3://earthlab-amahood/fastest-fires","data/road_denisity_km_km2.tif"),
             "--only-show-errors"))

homes_per_road/final_raster %>%
  writeRaster("data/home_density_per_km_km2_road.tif")

system(paste("aws s3 cp", "data/home_density_per_km_km2_road.tif", 
             file.path("s3://earthlab-amahood/fastest-fires","data/home_density_per_km_km2_road.tif"),
             "--only-show-errors"))
