FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    libssl-dev \
    pkg-config \
    cmake-data \
    git

RUN mkdir /root/Argus
COPY . /root/Argus

RUN mkdir -p -m 0600 ~/.ssh && \
    ssh-keyscan -H github.com >> ~/.ssh/known_hosts

RUN --mount=type=ssh git clone git@github.com:DeepanshKhurana/Hrafnagud-Creds /tmp/Hrafnagud-Creds --depth 1 && \
    cp /tmp/Hrafnagud-Creds/creds.txt /root/Argus/.Renviron && \
    rm -rf /tmp/Hrafnagud-Creds

WORKDIR /root/Argus/

RUN R -e "renv::restore()"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/Argus', host = '0.0.0.0', port = 3838)"]
