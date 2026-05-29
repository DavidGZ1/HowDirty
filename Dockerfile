FROM rocker/r-ver:4.2.0

# System libraries required by R packages (ggplot2, curl, openssl, xml2, fonts)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    cmake \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install package dependencies via renv (exact versions from lockfile)
# RENV_PATHS_LIBRARY redirects renv to the system library so all R processes
# inside the container see the packages without renv project infrastructure.
ENV RENV_PATHS_LIBRARY=/usr/local/lib/R/site-library
WORKDIR /build
COPY renv.lock .
RUN Rscript -e " \
    install.packages('renv', repos = 'https://cloud.r-project.org'); \
    renv::restore(prompt = FALSE) \
    "

# Install HowDirty from source, then clean up build artefacts
COPY . /build/pkg
RUN R CMD INSTALL /build/pkg && rm -rf /build

# CLI helpers
COPY docker/run_report.R  /usr/local/lib/howdirty_run.R
COPY docker/entrypoint.sh /usr/local/bin/howdirty
RUN sed -i 's/\r//' /usr/local/bin/howdirty && chmod +x /usr/local/bin/howdirty

VOLUME ["/data"]
WORKDIR /data
ENTRYPOINT ["/usr/local/bin/howdirty"]
