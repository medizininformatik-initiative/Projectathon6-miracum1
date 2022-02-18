FROM rocker/r-ver:latest

# MAINTAINER Nandhini Santhanam <nandhini.santhanam@medma.uni-heidelberg.de>
LABEL Description="PJT#6 miracum1"
LABEL Maintainer="nandhini.santhanam@medma.uni-heidelberg.de"

## build ARGs
ARG NCPUS=${NCPUS:--1}

RUN mkdir -p /Ergebnisse && \
  mkdir -p /errors && \
  mkdir -p /Bundles

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  libxml2-dev \
  libssl-dev curl && \
  ## clear caches:
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /tmp/* && \
  apt-get clean && apt-get autoclean && apt-get autoremove -y

RUN install2.r --error --skipinstalled -n $NCPUS \
  fhircrackr \
  config \
  dplyr \
  zoo \
  stringr \
  tidyr \
  data.table \
  openxlsx && \
  ## clear caches:
  rm -rf /tmp/downloaded_packages && \
  rm -rf /var/lib/apt/lists/*

COPY config_default.yml config_default.yml
COPY miracum_select.R miracum_select.R
COPY install_R_packages.R install_R_packages.R

CMD ["Rscript", "miracum_select.R"]
