library(shiny)

source("R/01-config.R")
source("R/02-dados.R")
source("R/03-email.R")
source("R/07-chatwoot.R")
source("R/06-auth.R")
source("R/04-ui.R")
source("R/05-server.R")

message("APP_VERSION=", Sys.getenv("APP_VERSION", unset = "dev"))
message("EMAIL_WORKER=", file.exists(file.path(pasta_raiz, "R", "08-email-worker.R")))
message("PASTA_RAIZ=", pasta_raiz)

shinyApp(ui = ui, server = server)
