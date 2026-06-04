log_boot <- function(...) {
  message(...)
  flush.console()
}

tryCatch(
  {
    log_boot("RUNNER_START=TRUE")
    log_boot("RUNNER_VERSION=", Sys.getenv("APP_VERSION", unset = "dev"))
    log_boot("RUNNER_CWD=", getwd())
    log_boot("RUNNER_APP_EXISTS=", file.exists("/srv/shiny-server/app.R"))

    source("/srv/shiny-server/app.R", chdir = TRUE)

    log_boot("RUNNER_SOURCE_DONE=TRUE")
    log_boot("RUNNER_UI_EXISTS=", exists("ui"))
    log_boot("RUNNER_SERVER_EXISTS=", exists("server"))
    log_boot("RUNNER_RUNAPP_START=TRUE")

    shiny::runApp(
      shiny::shinyApp(ui = ui, server = server),
      host = "0.0.0.0",
      port = 3838
    )
  },
  error = function(e) {
    log_boot("APP_FATAL=", conditionMessage(e))
    traceback()
    quit(status = 1)
  }
)
