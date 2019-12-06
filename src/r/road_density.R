#Road Density script
library(tidyverse)
library(sf)
library(doParallel)
library(raster)
library(foreach)
library(spex)
library(fasterize)

system(paste("aws s3 sync s3://earthlab-amahood/fastest-fires/data/counties data/background/counties --only-show-errors"))
system(paste("aws s3 cp s3://earthlab-amahood/fastest-fires/data/home_density.tif data/home_density.tif --only-show-errors"))

counties <- st_read("data/background/counties/")
homes_per_road <- raster("data/home_density.tif")
blank_raster <- homes_per_road
blank_raster[] <- 0

# A few counties didn't have road layers. They're not in CUS so no worries
# counties[counties$GEOID == 69085,]
# counties[counties$GEOID == 60030,]

counties <- counties %>%
  filter(GEOID != 69085, GEOID != 60030)
geoids<-counties$GEOID

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
  outfile <- paste0("data/background/roads/rd_tifs/",
                    "road_density_km_km2_",county,".tif")
  if(!file.exists(outfile)){
    sl<- st_read(i) %>%
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
