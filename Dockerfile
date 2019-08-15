FROM rocker/geospatial:latest
MAINTAINER "Adam Mahood" adam.mahood@colorado.edu

RUN install2.r --error \
  doParallel \
  lme4 \
  lmerTest \
  lwgeom \ 
  ggmap \
  ggpubr \
  ggthemes \
  nngeo
  mblm \
  stars \
  strucchange \
  velox \
