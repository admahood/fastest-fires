FROM rocker/geospatial:latest
MAINTAINER "Adam Mahood" adam.mahood@colorado.edu

RUN install2.r --error \
  lwgeom \ 
  velox \
  ggmap \ 
  ggthemes \
  stars