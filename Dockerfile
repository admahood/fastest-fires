FROM rocker/geospatial:latest
MAINTAINER "Adam Mahood" adam.mahood@colorado.edu

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    awscli \
    htop 

RUN install2.r --error \
  broom \
  doParallel \
  lme4 \
  lmerTest \
  lwgeom \ 
  ggmap \
  ggpubr \
  ggthemes \
  nngeo \
  mblm \
  scales \
  stars \
  strucchange \
  tidyverse \
  sf \
  velox 
