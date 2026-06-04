log_startup <- function(...) {
  message(...)
  flush.console()
}

source_startup <- function(arquivo) {
  log_startup("SOURCE_START=", arquivo)
  source(arquivo)
  log_startup("SOURCE_OK=", arquivo)
}

log_startup("APP_BOOT_VERSION=", Sys.getenv("APP_VERSION", unset = "dev"))
log_startup("APP_BOOT_CWD=", getwd())

library(shiny)

source_startup("R/01-config.R")
source_startup("R/02-dados.R")
source_startup("R/03-email.R")
source_startup("R/07-chatwoot.R")
source_startup("R/06-auth.R")
source_startup("R/04-ui.R")
source_startup("R/05-server.R")

log_startup("EMAIL_WORKER=", file.exists(file.path(pasta_raiz, "R", "08-email-worker.R")))
log_startup("PASTA_RAIZ=", pasta_raiz)
log_startup("SHINY_APP_READY=TRUE")

shinyApp(ui = ui, server = server)
