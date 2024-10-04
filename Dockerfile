FROM rocker/r-ver

RUN R -e "install.packages('renv')"

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    libssl-dev \
    pkg-config \
    cmake-data

RUN mkdir /root/Argus
COPY . /root/Argus

WORKDIR /root/Argus/

RUN R -e "renv::restore()"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/Argus', host = '0.0.0.0', port = 3838)"]
