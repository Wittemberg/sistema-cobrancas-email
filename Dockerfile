FROM rocker/shiny:4.4.3

ARG APP_VERSION=dev

ENV RENV_CONFIG_REPOS_OVERRIDE=https://cloud.r-project.org \
    TZ=America/Sao_Paulo \
    APP_VERSION=${APP_VERSION}

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
        zip \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shinythemes','DT','readr','dplyr','stringr','stringdist','stringi','purrr','glue','htmltools','fs','blastula','httr2','jsonlite','tibble','curl','later'), repos='https://cloud.r-project.org')" \
    && R -e "pkgs <- c('shiny','shinythemes','DT','readr','dplyr','stringr','stringdist','stringi','purrr','glue','htmltools','fs','blastula','httr2','jsonlite','tibble','curl','later'); stopifnot(all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)))"

WORKDIR /srv/shiny-server

RUN rm -rf /srv/shiny-server/*

COPY app.R ./app.R
COPY run-app.R ./run-app.R
COPY R ./R
COPY contexto.md ./contexto.md
COPY awe ./awe
COPY tecnoteam ./tecnoteam
COPY wr-tecnologia ./wr-tecnologia

RUN mkdir -p _config logs backups \
    && chmod -R 0775 /srv/shiny-server

EXPOSE 3838

CMD ["sh", "-c", "echo APP_VERSION=${APP_VERSION:-dev}; echo EMAIL_WORKER=$(test -f /srv/shiny-server/R/08-email-worker.R && echo TRUE || echo FALSE); exec Rscript /srv/shiny-server/run-app.R"]
