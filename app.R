library(shiny)

source("R/01-config.R")
source("R/02-dados.R")
source("R/03-email.R")
source("R/06-auth.R")
source("R/04-ui.R")
source("R/05-server.R")

shinyApp(ui = ui, server = server)