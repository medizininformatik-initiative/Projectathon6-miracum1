FROM rocker/r-ver:latest

# MAINTAINER Nandhini Santhanam <nandhini.santhanam@medma.uni-heidelberg.de>
LABEL Description="PJT#6 miracum1 step2"
LABEL Maintainer="nandhini.santhanam@medma.uni-heidelberg.de"

## build ARGs
ARG NCPUS=${NCPUS:--1}

ENV http_proxy http://proxy.klima.ads.local:2080/
ENV https_proxy http://proxy.klima.ads.local:2080/

RUN mkdir -p /Ergebnisse && \
  mkdir -p /errors && \
  mkdir -p /Bundles

RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  curl \
  ## `libpq-dev` for RPostgres (one of the deps/suggests of the packages):
  libpq-dev \
  libssl-dev \
  libxml2-dev \
  libgdal-dev \
  libgeos-dev \ 
  libproj-dev \
  uuid-dev && \
  ## clear caches:
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /tmp/* && \
  apt-get clean && apt-get autoclean && apt-get autoremove -y

RUN install2.r --error --deps TRUE --skipinstalled -n $NCPUS \
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

# COPY config_default.yml config_default.yml
COPY 1_install_r_packages.R 1_install_r_packages.R
COPY 2_data_selection.R 2_data_selection.R
COPY 3_feature_extraction.R 3_feature_extraction.R
COPY 4_modeling1_fixedwindow_false.R 4_modeling1_fixedwindow_false.R
COPY 5_modeling2_fixedwindow_true.R 5_modeling2_fixedwindow_true.R
COPY 6_modeling3_gamboost.R 6_modeling3_gamboost.R
COPY execution.R execution.R


CMD ["Rscript", "execution.R"]
