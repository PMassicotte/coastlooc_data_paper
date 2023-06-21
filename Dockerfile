FROM rocker/r-ver:4.3.0

RUN apt-get update \
   && apt-get install -y --no-install-recommends \
   libcairo2-dev \
   libpng-dev \
   libtiff5-dev \
   libjpeg-dev \
   default-jre \
   default-jdk \
   gdal-bin \
   libgdal-dev \
   libpoppler-dev  \
   libpoppler-cpp-dev \
   libudunits2-dev \
   libglpk-dev \
   libxt6 \
   texlive-extra-utils \
   ghostscript

WORKDIR /root/projects/

COPY renv.lock renv.lock

RUN mkdir -p renv

# COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# Not copying my local .Rprofile since it contains configuration details not needed for the current project.
RUN echo 'source("renv/activate.R")' >> /root/.Rprofile

# Make sure that we use the packages at the specified date
RUN echo 'options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/cran/__linux__/jammy/2023-06-01"))' >> /root/.Rprofile

RUN R -e "renv::restore()"
