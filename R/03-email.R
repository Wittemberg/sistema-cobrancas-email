# =========================================================
# FUNÇÕES DE EMAIL
# =========================================================

normalizar_lista_emails <- function(texto) {
  texto <- as.character(texto)

  if (length(texto) == 0 || is.na(texto) || trimws(texto) == "") {
    return(NULL)
  }

  emails <- unlist(strsplit(texto, "[;,[:space:]]+", perl = TRUE))
  emails <- trimws(emails)
  emails <- emails[emails != ""]

  if (length(emails) == 0) {
    return(NULL)
  }

  unique(emails)
}

cliente_tem_email_principal <- function(cliente) {
  if (!"email_principal" %in% names(cliente)) {
    return(FALSE)
  }

  email <- trimws(as.character(cliente$email_principal[1]))

  !is.na(email) && email != "" && grepl("@", email, fixed = TRUE)
}

cliente_tem_whatsapp <- function(cliente) {
  if (!"telefone_whatsapp" %in% names(cliente)) {
    return(FALSE)
  }

  telefone <- normalizar_telefone_br(cliente$telefone_whatsapp[1])

  !is.na(telefone) && nchar(telefone) >= 10
}

enviar_email_cliente_subprocesso <- function(
    empresa,
    cliente,
    competencia,
    mes_email,
    ano_email,
    log_callback = NULL,
    smtp_timeout_segundos = 120
) {
  registrar_etapa <- function(etapa, detalhe = "") {
    if (is.function(log_callback)) {
      tryCatch(
        log_callback(etapa, detalhe),
        error = function(e) {
          message("Log interno de email nao registrado: ", conditionMessage(e))
        }
      )
    }
  }

  timeout <- as.numeric(smtp_timeout_segundos)

  if (is.na(timeout) || timeout <= 0) {
    timeout <- 120
  }

  worker <- file.path(pasta_raiz, "R", "08-email-worker.R")

  if (!file.exists(worker)) {
    stop("Worker de email nao encontrado: ", worker)
  }

  payload <- list(
    empresa = as.character(empresa),
    competencia = as.character(competencia),
    mes_email = as.character(mes_email),
    ano_email = as.character(ano_email),
    cliente = as.list(cliente[1, , drop = FALSE])
  )

  payload_path <- tempfile("email-payload-", fileext = ".json")
  jsonlite::write_json(payload, payload_path, auto_unbox = TRUE, null = "null")
  on.exit(unlink(payload_path), add = TRUE)

  registrar_etapa("smtp_subprocesso_inicio", paste("Timeout:", timeout))

  resultado <- tryCatch(
    system2(
      "Rscript",
      args = c(worker, payload_path),
      stdout = TRUE,
      stderr = TRUE,
      timeout = timeout
    ),
    error = function(e) {
      structure(
        conditionMessage(e),
        status = 1
      )
    }
  )

  status <- attr(resultado, "status")

  if (is.null(status)) {
    status <- 0
  }

  detalhe <- paste(as.character(resultado), collapse = "\n")

  if (status == 0) {
    registrar_etapa("smtp_subprocesso_ok", detalhe)
    return(TRUE)
  }

  if (status == 124) {
    registrar_etapa("smtp_subprocesso_timeout", detalhe)
    stop("Timeout no envio SMTP apos ", timeout, " segundos.")
  }

  registrar_etapa("smtp_subprocesso_erro", detalhe)
  stop("Erro no subprocesso de envio SMTP: ", detalhe)
}

enviar_email_cliente <- function(
    empresa,
    cliente,
    competencia,
    mes_email,
    ano_email,
    enviar_whatsapp = FALSE,
    whatsapp_intervalo_segundos = 0,
    log_callback = NULL,
    smtp_timeout_segundos = 120,
    usar_subprocesso = TRUE
) {
  registrar_etapa_email <- function(etapa, detalhe = "") {
    if (is.function(log_callback)) {
      tryCatch(
        log_callback(etapa, detalhe),
        error = function(e) {
          message("Log interno de email nao registrado: ", conditionMessage(e))
        }
      )
    }
  }

  if (isTRUE(usar_subprocesso)) {
    if (!cliente_tem_email_principal(cliente)) {
      registrar_etapa_email("sem_email_principal")
      stop("Cliente sem e-mail principal.")
    }

    return(enviar_email_cliente_subprocesso(
      empresa = empresa,
      cliente = cliente,
      competencia = competencia,
      mes_email = mes_email,
      ano_email = ano_email,
      log_callback = log_callback,
      smtp_timeout_segundos = smtp_timeout_segundos
    ))
  }

  substituir_variaveis_email <- function(texto) {
    empresa_config <- carregar_dados_empresa_config(empresa)
    cliente_whatsapp <- if ("telefone_whatsapp" %in% names(cliente)) {
      formatar_telefone_br(cliente$telefone_whatsapp)
    } else {
      ""
    }

    texto |>
      gsub("{{cliente_nome}}", as.character(cliente$cliente_nome), x = _, fixed = TRUE) |>
      gsub("{{cliente_email}}", as.character(cliente$email_principal), x = _, fixed = TRUE) |>
      gsub("{{email_principal}}", as.character(cliente$email_principal), x = _, fixed = TRUE) |>
      gsub("{{cliente_whatsapp}}", cliente_whatsapp, x = _, fixed = TRUE) |>
      gsub("{{empresa_whatsapp}}", empresa_config$empresa_whatsapp, x = _, fixed = TRUE) |>
      gsub("{{mes_atual}}", mes_email, x = _, fixed = TRUE) |>
      gsub("{{ano_atual}}", as.character(ano_email), x = _, fixed = TRUE) |>
      gsub("{{mes_referencia}}", mes_email, x = _, fixed = TRUE) |>
      gsub("{{ano_referencia}}", as.character(ano_email), x = _, fixed = TRUE) |>
      gsub("{cliente_nome}", as.character(cliente$cliente_nome), x = _, fixed = TRUE) |>
      gsub("{cliente_email}", as.character(cliente$email_principal), x = _, fixed = TRUE) |>
      gsub("{email_principal}", as.character(cliente$email_principal), x = _, fixed = TRUE) |>
      gsub("{cliente_whatsapp}", cliente_whatsapp, x = _, fixed = TRUE) |>
      gsub("{empresa_whatsapp}", empresa_config$empresa_whatsapp, x = _, fixed = TRUE) |>
      gsub("{mes_atual}", mes_email, x = _, fixed = TRUE) |>
      gsub("{ano_atual}", as.character(ano_email), x = _, fixed = TRUE) |>
      gsub("{mes_referencia}", mes_email, x = _, fixed = TRUE) |>
      gsub("{ano_referencia}", as.character(ano_email), x = _, fixed = TRUE)
  }

  registrar_etapa_email("carregando_configuracoes")

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

  registrar_etapa_email("carregando_modelos")

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
    empresa_config <- carregar_dados_empresa_config(empresa)
    cliente_whatsapp <- if ("telefone_whatsapp" %in% names(cliente)) {
      formatar_telefone_br(cliente$telefone_whatsapp)
    } else {
      ""
    }

    corpo_html <- template_html |>
      gsub("{{corpo_email}}", gsub("\n", "<br>", corpo), x = _, fixed = TRUE) |>
      gsub("{{empresa_nome}}", remetente$empresa_nome, x = _, fixed = TRUE) |>
      gsub("{{empresa_whatsapp}}", empresa_config$empresa_whatsapp, x = _, fixed = TRUE) |>
      gsub("{{cliente_nome}}", cliente$cliente_nome, x = _, fixed = TRUE) |>
      gsub("{{cliente_email}}", as.character(cliente$email_principal), x = _, fixed = TRUE) |>
      gsub("{{email_principal}}", as.character(cliente$email_principal), x = _, fixed = TRUE) |>
      gsub("{{cliente_whatsapp}}", cliente_whatsapp, x = _, fixed = TRUE) |>
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

  registrar_etapa_email("buscando_pdfs")

  verificacao <- buscar_pdfs_cliente(
    empresa,
    competencia,
    cliente$cliente_nome
  )

  registrar_etapa_email("anexando_pdfs", paste("PDFs:", length(verificacao$arquivos_pdf)))

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

  registrar_etapa_email("preparando_credenciais")

  if (!cliente_tem_email_principal(cliente)) {
    registrar_etapa_email("sem_email_principal")
    stop("Cliente sem e-mail principal.")
  }

  credenciais <- blastula::creds_envvar(
    user = as.character(smtp$usuario),
    pass_envvar = smtp_senha_env,
    host = as.character(smtp$host),
    port = as.numeric(smtp$port),
    use_ssl = as.logical(smtp$use_ssl)
  )

  destinatario <- trimws(as.character(cliente$email_principal))
  copias <- normalizar_lista_emails(cliente$email_copias)

  registrar_etapa_email("smtp_send_inicio", paste("Para:", destinatario))

  timeout <- as.numeric(smtp_timeout_segundos)

  if (is.na(timeout) || timeout <= 0) {
    timeout <- 120
  }

  setTimeLimit(elapsed = timeout, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)

  tryCatch(
    blastula::smtp_send(
      email = email,
      from = as.character(smtp$usuario),
      to = destinatario,
      cc = copias,
      subject = as.character(assunto),
      credentials = credenciais
    ),
    error = function(e) {
      registrar_etapa_email("smtp_send_erro", conditionMessage(e))
      stop(e)
    }
  )

  registrar_etapa_email("smtp_send_ok", paste("Para:", destinatario))

  return(TRUE)

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

enviar_whatsapp_apos_tentativa_email <- function(
    empresa,
    cliente,
    competencia,
    mes_email,
    ano_email,
    enviar_whatsapp = FALSE,
    whatsapp_intervalo_segundos = 0,
    email_status = "email_enviado"
) {
  if (isTRUE(enviar_whatsapp) && exists("enviar_whatsapp_cliente")) {
    if (!cliente_tem_whatsapp(cliente)) {
      message("WhatsApp nao enviado: cliente sem telefone WhatsApp.")
      return(FALSE)
    }

    origem <- if (identical(email_status, "email_falha")) {
      "automatico_email_falha"
    } else {
      "automatico_email"
    }

    tryCatch(
      enviar_whatsapp_cliente(
        empresa = empresa,
        cliente = cliente,
        origem = origem,
        competencia = competencia,
        mes_email = mes_email,
        ano_email = ano_email,
        email_status = email_status
      ),
      error = function(e) {
        message("WhatsApp nao enviado: ", conditionMessage(e))
      }
    )

    intervalo <- as.numeric(whatsapp_intervalo_segundos)

    if (!is.na(intervalo) && intervalo > 0) {
      Sys.sleep(intervalo)
    }
  }

  TRUE
}
