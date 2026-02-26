# syntax=docker/dockerfile:1.7

FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    pkg-config \
    cmake-data \
    git \
    openssh-client \
    libv8-dev \
    libsodium-dev \
    tzdata \
    zlib1g-dev \
    ca-certificates \
    libgit2-dev \
 && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /root/.ssh && \
    ssh-keyscan github.com >> /root/.ssh/known_hosts

RUN mkdir -p /root/Argus
COPY . /root/Argus

WORKDIR /root/Argus

RUN --mount=type=ssh R -e "renv::restore()"

EXPOSE 3838

CMD ["R","-e","shiny::runApp('/root/Argus', host='0.0.0.0', port=3838)"]
