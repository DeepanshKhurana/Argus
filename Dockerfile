FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-gnutls-dev libssl-dev libxml2-dev libpq-dev \
    pkg-config cmake-data git libv8-dev libsodium-dev tzdata \
    zlib1g-dev libgit2-dev ca-certificates && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p /root/Argus
COPY . /root/Argus

WORKDIR /root/Argus

RUN R -e "renv::restore()"

EXPOSE 3838
CMD ["R","-e","shiny::runApp('/root/Argus', host='0.0.0.0', port=3838)"]
