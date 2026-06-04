# =========================================================
# FUNCOES CHATWOOT / WHATSAPP
# =========================================================

caminho_log_whatsapp <- function() {
  file.path(pasta_raiz, "logs", "whatsapp.csv")
}

registrar_log_whatsapp <- function(
    empresa,
    cliente_nome,
    telefone,
    mensagem,
    status,
    erro = "",
    origem = "",
    competencia = ""
) {
  caminho <- caminho_log_whatsapp()
  dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)

  novo_log <- tibble::tibble(
    data_hora = as.character(Sys.time()),
    empresa = as.character(empresa),
    cliente_nome = as.character(cliente_nome),
    telefone = as.character(telefone),
    mensagem = as.character(mensagem),
    status = as.character(status),
    erro = as.character(erro),
    origem = as.character(origem),
    competencia = as.character(competencia)
  )

  if (file.exists(caminho)) {
    logs <- readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )

    logs <- dplyr::bind_rows(logs, novo_log)
  } else {
    logs <- novo_log
  }

  readr::write_csv(logs, caminho)
}

carregar_chatwoot_empresa <- function(empresa) {
  caminho <- file.path(pasta_raiz, "_config", "chatwoot.csv")

  if (!file.exists(caminho)) {
    return(tibble::tibble())
  }

  dados <- readr::read_csv(
    caminho,
    show_col_types = FALSE,
    col_types = readr::cols(.default = "c")
  )

  if (!"ativo" %in% names(dados)) {
    dados$ativo <- TRUE
  }

  if (!"enviar_pdfs_whatsapp" %in% names(dados)) {
    dados$enviar_pdfs_whatsapp <- FALSE
  }

  if (!"mensagem_email_enviado" %in% names(dados)) {
    dados$mensagem_email_enviado <- ""
  }

  if (!"mensagem_email_falha" %in% names(dados)) {
    dados$mensagem_email_falha <- ""
  }

  dados |>
    dplyr::mutate(
      ativo = as.logical(.data$ativo),
      enviar_pdfs_whatsapp = as.logical(.data$enviar_pdfs_whatsapp)
    ) |>
    dplyr::filter(
      .data$empresa_id == empresa,
      .data$ativo == TRUE
    ) |>
    dplyr::slice(1)
}

carregar_nome_empresa <- function(empresa) {
  carregar_dados_empresa_config(empresa)$empresa_nome
}

normalizar_telefone_whatsapp <- function(telefone) {
  telefone <- gsub("[^0-9]", "", as.character(telefone))

  if (is.na(telefone) || nchar(telefone) < 10) {
    stop("Telefone WhatsApp inválido. Informe DDD + número.")
  }

  if (!startsWith(telefone, "55")) {
    telefone <- paste0("55", telefone)
  }

  paste0("+", telefone)
}

substituir_variaveis_whatsapp <- function(
    texto,
    empresa,
    cliente,
    competencia = "",
    mes_email = "",
    ano_email = ""
) {
  empresa_config <- carregar_dados_empresa_config(empresa)
  empresa_nome <- empresa_config$empresa_nome
  cliente_email <- if ("email_principal" %in% names(cliente)) {
    as.character(cliente$email_principal)
  } else {
    ""
  }

  texto |>
    gsub("{{cliente_nome}}", as.character(cliente$cliente_nome), x = _, fixed = TRUE) |>
    gsub("{{cliente_email}}", cliente_email, x = _, fixed = TRUE) |>
    gsub("{{email_principal}}", cliente_email, x = _, fixed = TRUE) |>
    gsub("{{empresa_id}}", as.character(empresa), x = _, fixed = TRUE) |>
    gsub("{{empresa_nome}}", empresa_nome, x = _, fixed = TRUE) |>
    gsub("{{empresa_whatsapp}}", empresa_config$empresa_whatsapp, x = _, fixed = TRUE) |>
    gsub("{{competencia_pdfs}}", as.character(competencia), x = _, fixed = TRUE) |>
    gsub("{{mes_referencia}}", as.character(mes_email), x = _, fixed = TRUE) |>
    gsub("{{ano_referencia}}", as.character(ano_email), x = _, fixed = TRUE)
}

mensagem_whatsapp_padrao <- function(
    empresa,
    cliente,
    competencia,
    mes_email,
    ano_email,
    email_status = "email_enviado"
) {
  config <- carregar_chatwoot_empresa(empresa)

  texto_padrao <- if (identical(email_status, "email_falha")) {
    paste(
      "Olá, {{cliente_nome}}.",
      "{{empresa_nome}} está entrando em contato sobre os documentos referentes à competência {{competencia_pdfs}}.",
      "Qualquer dúvida, estamos à disposição."
    )
  } else {
    paste(
      "Olá, {{cliente_nome}}.",
      "{{empresa_nome}} enviou por e-mail os documentos referentes à competência {{competencia_pdfs}}.",
      "Qualquer dúvida, estamos à disposição."
    )
  }

  coluna_mensagem <- if (identical(email_status, "email_falha")) {
    "mensagem_email_falha"
  } else {
    "mensagem_email_enviado"
  }

  texto <- texto_padrao

  if (nrow(config) > 0 && coluna_mensagem %in% names(config)) {
    texto_config <- as.character(config[[coluna_mensagem]][1])

    if (!is.na(texto_config) && trimws(texto_config) != "") {
      texto <- texto_config
    }
  }

  return(substituir_variaveis_whatsapp(
    texto,
    empresa = empresa,
    cliente = cliente,
    competencia = competencia,
    mes_email = mes_email,
    ano_email = ano_email
  ))

  substituir_variaveis_whatsapp(
    paste(
      "Olá, {{cliente_nome}}.",
      "{{empresa_nome}} enviou por e-mail os documentos referentes à competência {{competencia_pdfs}}.",
      "Qualquer dúvida, estamos à disposição."
    ),
    empresa = empresa,
    cliente = cliente,
    competencia = competencia,
    mes_email = mes_email,
    ano_email = ano_email
  )
}

enviar_whatsapp_cliente <- function(
    empresa,
    cliente,
    mensagem = NULL,
    origem = "manual",
    competencia = "",
    mes_email = "",
    ano_email = "",
    email_status = "email_enviado",
    enviar_pdfs = NULL
) {
  cliente_nome <- as.character(cliente$cliente_nome)
  telefone <- as.character(cliente$telefone_whatsapp)
  arquivos_pdf <- character(0)
  mensagem <- if (is.null(mensagem) || trimws(mensagem) == "") {
    mensagem_whatsapp_padrao(
      empresa = empresa,
      cliente = cliente,
      competencia = competencia,
      mes_email = mes_email,
      ano_email = ano_email,
      email_status = email_status
    )
  } else {
    substituir_variaveis_whatsapp(
      mensagem,
      empresa = empresa,
      cliente = cliente,
      competencia = competencia,
      mes_email = mes_email,
      ano_email = ano_email
    )
  }

  tryCatch({
    config <- carregar_chatwoot_empresa(empresa)

    if (nrow(config) == 0) {
      stop("Configuração Chatwoot ativa não encontrada para a empresa.")
    }

    anexar_pdfs <- if (is.null(enviar_pdfs)) {
      isTRUE(as.logical(config$enviar_pdfs_whatsapp[1]))
    } else {
      isTRUE(as.logical(enviar_pdfs))
    }

    if (
      isTRUE(anexar_pdfs) &&
        as.character(competencia) != "" &&
        exists("buscar_pdfs_cliente")
    ) {
      verificacao_pdfs <- buscar_pdfs_cliente(
        empresa = empresa,
        competencia = competencia,
        cliente_nome = cliente_nome
      )

      arquivos_pdf <- as.character(verificacao_pdfs$arquivos_pdf)
    }

    telefone_normalizado <- normalizar_telefone_whatsapp(telefone)
    base_url <- sub("/+$", "", as.character(config$base_url))

    contato_url <- paste0(
      base_url,
      "/public/api/v1/inboxes/",
      as.character(config$inbox_identifier),
      "/contacts"
    )

    contato_resp <- httr2::request(contato_url) |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "api_access_token" = as.character(config$api_access_token)
      ) |>
      httr2::req_body_json(
        list(
          name = cliente_nome,
          phone_number = telefone_normalizado
        )
      ) |>
      httr2::req_perform()

    contato_json <- httr2::resp_body_json(contato_resp)
    contact_identifier <- contato_json$source_id

    if (is.null(contact_identifier) || contact_identifier == "") {
      contact_identifier <- contato_json$id
    }

    conversa_url <- paste0(
      base_url,
      "/public/api/v1/inboxes/",
      as.character(config$inbox_identifier),
      "/contacts/",
      contact_identifier,
      "/conversations"
    )

    conversa_resp <- httr2::request(conversa_url) |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "api_access_token" = as.character(config$api_access_token)
      ) |>
      httr2::req_body_json(
        list(
          custom_attributes = list(
            empresa_id = as.character(empresa),
            competencia = as.character(competencia),
            origem = as.character(origem)
          )
        )
      ) |>
      httr2::req_perform()

    conversa_json <- httr2::resp_body_json(conversa_resp)
    conversation_id <- conversa_json$id

    mensagem_url <- paste0(
      base_url,
      "/public/api/v1/inboxes/",
      as.character(config$inbox_identifier),
      "/contacts/",
      contact_identifier,
      "/conversations/",
      conversation_id,
      "/messages"
    )

    httr2::request(mensagem_url) |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "api_access_token" = as.character(config$api_access_token)
      ) |>
      httr2::req_body_json(
        list(
          content = mensagem
        )
      ) |>
      httr2::req_perform()

    if (length(arquivos_pdf) > 0) {
      for (arquivo_pdf in arquivos_pdf) {
        httr2::request(mensagem_url) |>
          httr2::req_headers(
            "api_access_token" = as.character(config$api_access_token)
          ) |>
          httr2::req_body_multipart(
            content = "",
            file_type = "document",
            `attachments[]` = curl::form_file(arquivo_pdf)
          ) |>
          httr2::req_perform()
      }
    }

    mensagem_log <- if (length(arquivos_pdf) > 0) {
      paste0(mensagem, "\nPDFs anexados: ", length(arquivos_pdf))
    } else {
      mensagem
    }

    registrar_log_whatsapp(
      empresa = empresa,
      cliente_nome = cliente_nome,
      telefone = telefone_normalizado,
      mensagem = mensagem_log,
      status = "enviado",
      origem = origem,
      competencia = competencia
    )

    TRUE
  },
  error = function(e) {
    registrar_log_whatsapp(
      empresa = empresa,
      cliente_nome = cliente_nome,
      telefone = telefone,
      mensagem = mensagem,
      status = "erro",
      erro = conditionMessage(e),
      origem = origem,
      competencia = competencia
    )

    stop(e)
  })
}
