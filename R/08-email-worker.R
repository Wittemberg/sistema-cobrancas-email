args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop("Informe o caminho do payload JSON.")
}

source("R/01-config.R")
source("R/02-dados.R")
source("R/03-email.R")

payload <- jsonlite::read_json(
  args[1],
  simplifyVector = TRUE
)

cliente <- tibble::as_tibble(
  lapply(payload$cliente, function(valor) {
    if (is.null(valor)) {
      ""
    } else {
      as.character(valor)
    }
  })
)

enviar_email_cliente(
  empresa = as.character(payload$empresa),
  cliente = cliente,
  competencia = as.character(payload$competencia),
  mes_email = as.character(payload$mes_email),
  ano_email = as.character(payload$ano_email),
  enviar_whatsapp = FALSE,
  usar_subprocesso = FALSE
)

cat("Email enviado com sucesso.\n")
