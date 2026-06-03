# =========================================================
# FUNÇÕES DE EMAIL
# =========================================================

enviar_email_cliente <- function(
    empresa,
    cliente,
    competencia,
    mes_email,
    ano_email,
    enviar_whatsapp = FALSE,
    whatsapp_intervalo_segundos = 0
) {
  substituir_variaveis_email <- function(texto) {
    texto |>
      gsub("{{mes_atual}}", mes_email, x = _, fixed = TRUE) |>
      gsub("{{ano_atual}}", as.character(ano_email), x = _, fixed = TRUE) |>
      gsub("{{mes_referencia}}", mes_email, x = _, fixed = TRUE) |>
      gsub("{{ano_referencia}}", as.character(ano_email), x = _, fixed = TRUE) |>
      gsub("{mes_atual}", mes_email, x = _, fixed = TRUE) |>
      gsub("{ano_atual}", as.character(ano_email), x = _, fixed = TRUE) |>
      gsub("{mes_referencia}", mes_email, x = _, fixed = TRUE) |>
      gsub("{ano_referencia}", as.character(ano_email), x = _, fixed = TRUE)
  }

  remetentes <- readr::read_csv(
    file.path(pasta_raiz, "_config", "remetentes.csv"),
    show_col_types = FALSE
  )

  smtp_lista <- readr::read_csv(
    file.path(pasta_raiz, "_config", "smtp.csv"),
    show_col_types = FALSE
  )

  remetente <- remetentes |>
    dplyr::filter(
      .data$empresa_id == empresa,
      as.logical(.data$ativo) == TRUE
    ) |>
    dplyr::slice(1)

  smtp <- smtp_lista |>
    dplyr::filter(
      .data$smtp_id == remetente$smtp_id
    ) |>
    dplyr::slice(1)

  assunto <- readr::read_file(
    file.path(pasta_raiz, empresa, "modelos", "assunto.txt")
  )

  corpo <- readr::read_file(
    file.path(pasta_raiz, empresa, "modelos", "corpo_email.txt")
  )

  assunto <- substituir_variaveis_email(assunto)
  corpo <- substituir_variaveis_email(corpo)

  template_path <- file.path(
    pasta_raiz,
    empresa,
    "modelos",
    "template_html.html"
  )

  if (file.exists(template_path)) {
    template_html <- readr::read_file(template_path)

    corpo_html <- template_html |>
      gsub("{{corpo_email}}", gsub("\n", "<br>", corpo), x = _, fixed = TRUE) |>
      gsub("{{empresa_nome}}", remetente$empresa_nome, x = _, fixed = TRUE) |>
      gsub("{{cliente_nome}}", cliente$cliente_nome, x = _, fixed = TRUE) |>
      gsub("{{mes_atual}}", mes_email, x = _, fixed = TRUE) |>
      gsub("{{ano_atual}}", as.character(ano_email), x = _, fixed = TRUE) |>
      gsub("{{mes_referencia}}", mes_email, x = _, fixed = TRUE) |>
      gsub("{{ano_referencia}}", as.character(ano_email), x = _, fixed = TRUE) |>
      gsub("{{competencia_pdfs}}", competencia, x = _, fixed = TRUE)

    email <- blastula::compose_email(
      body = htmltools::HTML(corpo_html)
    )
  } else {
    email <- blastula::compose_email(
      body = blastula::md(corpo)
    )
  }

  verificacao <- buscar_pdfs_cliente(
    empresa,
    competencia,
    cliente$cliente_nome
  )

  for (arquivo_pdf in verificacao$arquivos_pdf) {
    email <- blastula::add_attachment(
      email,
      file = arquivo_pdf
    )
  }

  smtp_senha_env <- paste0(
    "SMTP_SENHA_",
    as.character(smtp$smtp_id)
  )

  do.call(
    Sys.setenv,
    stats::setNames(
      as.list(as.character(smtp$senha)),
      smtp_senha_env
    )
  )

  credenciais <- blastula::creds_envvar(
    user = as.character(smtp$usuario),
    pass_envvar = smtp_senha_env,
    host = as.character(smtp$host),
    port = as.numeric(smtp$port),
    use_ssl = as.logical(smtp$use_ssl)
  )

  destinatario <- trimws(as.character(cliente$email_principal))
  copias <- trimws(as.character(cliente$email_copias))

  if (is.na(copias) || copias == "") {
    copias <- NULL
  }

  blastula::smtp_send(
    email = email,
    from = as.character(smtp$usuario),
    to = destinatario,
    cc = copias,
    subject = as.character(assunto),
    credentials = credenciais
  )

  if (isTRUE(enviar_whatsapp) && exists("enviar_whatsapp_cliente")) {
    tryCatch(
      enviar_whatsapp_cliente(
        empresa = empresa,
        cliente = cliente,
        origem = "automatico_email",
        competencia = competencia,
        mes_email = mes_email,
        ano_email = ano_email
      ),
      error = function(e) {
        message("WhatsApp não enviado: ", conditionMessage(e))
      }
    )

    intervalo <- as.numeric(whatsapp_intervalo_segundos)

    if (!is.na(intervalo) && intervalo > 0) {
      Sys.sleep(intervalo)
    }
  }

  TRUE
}
