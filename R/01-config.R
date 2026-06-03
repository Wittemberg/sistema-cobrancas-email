# =========================================================
# CONFIGURAÇÕES GERAIS
# =========================================================

library(shiny)
library(shinythemes)
library(DT)

library(readr)
library(dplyr)
library(stringr)
library(stringdist)
library(stringi)
library(purrr)
library(glue)
library(htmltools)

library(fs)

library(blastula)

options(shiny.maxRequestSize = 500 * 1024^2)

pasta_raiz <- getwd()

# =========================================================
# PASTAS
# =========================================================

dir_create(file.path(pasta_raiz, "logs"))
dir_create(file.path(pasta_raiz, "backups"))
dir_create(file.path(pasta_raiz, "_config"))
