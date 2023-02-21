FROM rocker/r-ver:latest


# MAINTAINER Nandhini Santhanam <nandhini.santhanam@medma.uni-heidelberg.de>
LABEL Description="PJT#6 miracum1 step2"
LABEL Maintainer="nandhini.santhanam@medma.uni-heidelberg.de"

## build ARGs
ARG NCPUS=${NCPUS:--1}


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
  default-jdk \
  r-cran-rjava \
  uuid-dev && \
  ## clear caches:
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /tmp/* && \
  apt-get clean && apt-get autoclean && apt-get autoremove -y
  
  
RUN R -e "install.packages(c('fhircrackr', 'config', 'dplyr', 'zoo', 'stringr', 'tidyr', 'data.table', 'openxlsx', 'rdwd', 'lubridate', 'geosphere', 'stringi', 'caret', 'ranger', 'mboost', 'xgboost' ,'tidyverse', 'corrplot', 'earth', 'Metrics', 'ggplot2', 'RANN', 'pacman'), dependencies = TRUE, repos='https://cloud.r-project.org/')"






# COPY config_default.yml config_default.yml
COPY 1_install_r_packages.R 1_install_r_packages.R
COPY 2_data_selection.R 2_data_selection.R
COPY 3_feature_extraction.R 3_feature_extraction.R
COPY 4_modeling1_fixedwindow_false.R 4_modeling1_fixedwindow_false.R
COPY 5_modeling2_fixedwindow_true.R 5_modeling2_fixedwindow_true.R
COPY 6_modeling3_gamboost.R 6_modeling3_gamboost.R
COPY customRF.R customRF.R
COPY execution.R execution.R


CMD ["Rscript", "execution.R"]
