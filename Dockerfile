FROM rocker/r-ver:latest

# MAINTAINER Nandhini Santhanam <nandhini.santhanam@medma.uni-heidelberg.de>
LABEL Description="PJT#6 miracum1"
LABEL Maintainer="nandhini.santhanam@medma.uni-heidelberg.de"

RUN mkdir -p /Ergebnisse
RUN mkdir -p /errors
RUN mkdir -p /Bundles

RUN apt-get update -qq && apt-get install -yqq \
  libxml2-dev \
  libssl-dev curl

RUN install2.r --error --deps TRUE \
  fhircrackr

COPY config_default.yml config_default.yml
COPY miracum_select.R miracum_select.R
COPY install_R_packages.R install_R_packages.R

CMD ["Rscript", "miracum_select.R"]
