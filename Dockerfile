FROM rocker/r-ver:4.4.3

ENV RENV_CONFIG_REPOS_OVERRIDE=https://cloud.r-project.org \
    TZ=America/Sao_Paulo

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        libcurl4-openssl-dev \
        libicu-dev \
        libsodium-dev \
        libssl-dev \
        libxml2-dev \
        pandoc \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny','shinythemes','DT','readr','dplyr','stringr','stringdist','stringi','purrr','glue','htmltools','fs','blastula','httr2','jsonlite','tibble'), repos='https://cloud.r-project.org')"

WORKDIR /srv/app

COPY app.R ./app.R
COPY R ./R
COPY contexto.md ./contexto.md
COPY awe ./awe
COPY tecnoteam ./tecnoteam
COPY wr-tecnologia ./wr-tecnologia
COPY _config ./_config

RUN mkdir -p logs backups \
    && chmod -R 0775 /srv/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/app', host = '0.0.0.0', port = 3838)"]
