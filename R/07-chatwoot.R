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
    competencia = "",
    detalhe = ""
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
    competencia = as.character(competencia),
    detalhe = as.character(detalhe)
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

chatwoot_timeout_segundos <- function() {
  timeout <- suppressWarnings(as.numeric(Sys.getenv("CHATWOOT_TIMEOUT_SECONDS", "60")))

  if (is.na(timeout) || timeout <= 0) {
    return(60)
  }

  timeout
}

chatwoot_perform <- function(req, etapa) {
  tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop(
        paste0("Chatwoot ", etapa, ": ", conditionMessage(e)),
        call. = FALSE
      )
    }
  )
}

chatwoot_resumo_resposta <- function(resp, metodo, conversation_id = "") {
  status_code <- tryCatch(httr2::resp_status(resp), error = function(e) NA_integer_)
  corpo <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)

  partes <- c(
    paste0("metodo=", metodo),
    paste0("http_status=", status_code)
  )

  if (!is.null(corpo)) {
    if (!is.null(corpo$id)) {
      partes <- c(partes, paste0("message_id=", as.character(corpo$id)))
    }
    if (!is.null(corpo$conversation_id)) {
      partes <- c(partes, paste0("conversation_id=", as.character(corpo$conversation_id)))
    }
    if (!is.null(corpo$status)) {
      partes <- c(partes, paste0("message_status=", as.character(corpo$status)))
    }
    if (!is.null(corpo$source_id)) {
      partes <- c(partes, paste0("source_id=", as.character(corpo$source_id)))
    }
    if (!is.null(corpo$inbox_id)) {
      partes <- c(partes, paste0("inbox_id=", as.character(corpo$inbox_id)))
    }
  }

  if (as.character(conversation_id) != "") {
    partes <- c(partes, paste0("conversation_id_usado=", as.character(conversation_id)))
  }

  paste(partes, collapse = " | ")
}

chatwoot_contact_identifier <- function(empresa, telefone_normalizado) {
  telefone_limpo <- gsub("[^0-9]", "", as.character(telefone_normalizado))
  empresa_limpa <- gsub(" ", "_", normalizar_nome(empresa))
  paste0("wrtec_", empresa_limpa, "_", telefone_limpo)
}

chatwoot_metodo_envio <- function(config) {
  if (!"metodo_envio" %in% names(config)) {
    return("public_api")
  }

  metodo <- trimws(as.character(config$metodo_envio[1]))

  if (metodo %in% c("public_api", "account_api")) {
    return(metodo)
  }

  "public_api"
}

chatwoot_inbox_id <- function(config) {
  inbox_id <- if ("inbox_id" %in% names(config)) {
    trimws(as.character(config$inbox_id[1]))
  } else {
    ""
  }

  if (inbox_id == "" && "inbox_identifier" %in% names(config)) {
    candidato <- trimws(as.character(config$inbox_identifier[1]))

    if (grepl("^[0-9]+$", candidato)) {
      inbox_id <- candidato
    }
  }

  if (inbox_id == "" || !grepl("^[0-9]+$", inbox_id)) {
    stop("Inbox ID numerico obrigatorio para o metodo Account API.")
  }

  as.integer(inbox_id)
}

chatwoot_payload_lista <- function(json) {
  payload <- json$payload

  if (is.null(payload)) {
    return(list())
  }

  if (is.data.frame(payload)) {
    return(split(payload, seq_len(nrow(payload))))
  }

  if (is.list(payload) && "id" %in% names(payload)) {
    return(list(payload))
  }

  if (is.list(payload) && length(payload) > 0 && !is.null(payload[[1]])) {
    return(payload)
  }

  list()
}

chatwoot_primeiro_valor <- function(x, nome) {
  valor <- x[[nome]]

  if (is.null(valor) || length(valor) == 0) {
    return(NULL)
  }

  valor[[1]]
}

chatwoot_contact_source_id <- function(contato, inbox_id, fallback) {
  contact_inboxes <- contato$contact_inboxes

  if (is.null(contact_inboxes)) {
    return(fallback)
  }

  if (is.data.frame(contact_inboxes)) {
    if ("source_id" %in% names(contact_inboxes)) {
      return(as.character(contact_inboxes$source_id[1]))
    }

    return(fallback)
  }

  for (item in contact_inboxes) {
    inbox <- item$inbox
    item_inbox_id <- if (!is.null(inbox$id)) as.integer(inbox$id) else NA_integer_

    if (is.na(item_inbox_id) || item_inbox_id == inbox_id) {
      source_id <- item$source_id

      if (!is.null(source_id) && source_id != "") {
        return(as.character(source_id))
      }
    }
  }

  fallback
}

chatwoot_contact_id <- function(contato_json) {
  if (!is.null(contato_json$id)) {
    return(as.integer(contato_json$id))
  }

  payload <- chatwoot_payload_lista(contato_json)

  if (length(payload) > 0 && !is.null(payload[[1]]$id)) {
    return(as.integer(payload[[1]]$id))
  }

  NA_integer_
}

chatwoot_buscar_ou_criar_contato_account <- function(
    base_url,
    account_id,
    inbox_id,
    token,
    cliente_nome,
    telefone_normalizado,
    identifier,
    timeout
) {
  busca_url <- paste0(
    base_url,
    "/api/v1/accounts/",
    account_id,
    "/contacts/search"
  )

  busca_resp <- httr2::request(busca_url) |>
    httr2::req_headers("api_access_token" = token) |>
    httr2::req_url_query(q = telefone_normalizado) |>
    httr2::req_timeout(timeout) |>
    chatwoot_perform("account_contato_busca")

  contatos <- chatwoot_payload_lista(httr2::resp_body_json(busca_resp))
  telefone_limpo <- gsub("[^0-9]", "", telefone_normalizado)

  for (contato in contatos) {
    contato_tel <- gsub("[^0-9]", "", as.character(contato$phone_number))
    contato_identifier <- as.character(contato$identifier)

    if (
      identical(contato_identifier, identifier) ||
        identical(contato_tel, telefone_limpo)
    ) {
      return(contato)
    }
  }

  criar_url <- paste0(
    base_url,
    "/api/v1/accounts/",
    account_id,
    "/contacts"
  )

  criar_resp <- httr2::request(criar_url) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "api_access_token" = token
    ) |>
    httr2::req_body_json(
      list(
        inbox_id = inbox_id,
        name = cliente_nome,
        phone_number = telefone_normalizado,
        identifier = identifier,
        custom_attributes = list()
      )
    ) |>
    httr2::req_timeout(timeout) |>
    chatwoot_perform("account_contato_criar")

  criar_json <- httr2::resp_body_json(criar_resp)
  payload <- chatwoot_payload_lista(criar_json)

  if (length(payload) > 0) {
    return(payload[[1]])
  }

  criar_json
}

chatwoot_obter_conversa_account <- function(
    base_url,
    account_id,
    inbox_id,
    token,
    contato,
    source_id,
    timeout,
    empresa,
    competencia,
    origem
) {
  contato_id <- chatwoot_contact_id(contato)

  if (is.na(contato_id)) {
    stop("Contato Chatwoot sem ID para criar conversa.")
  }

  conversas_url <- paste0(
    base_url,
    "/api/v1/accounts/",
    account_id,
    "/contacts/",
    contato_id,
    "/conversations"
  )

  conversas_resp <- httr2::request(conversas_url) |>
    httr2::req_headers("api_access_token" = token) |>
    httr2::req_timeout(timeout) |>
    chatwoot_perform("account_conversas_contato")

  conversas <- chatwoot_payload_lista(httr2::resp_body_json(conversas_resp))

  if (length(conversas) > 0) {
    for (conversa in conversas) {
      conversa_inbox_id <- suppressWarnings(as.integer(conversa$inbox_id))

      if (!is.na(conversa_inbox_id) && conversa_inbox_id == inbox_id) {
        return(as.integer(conversa$id))
      }
    }
  }

  criar_url <- paste0(
    base_url,
    "/api/v1/accounts/",
    account_id,
    "/conversations"
  )

  criar_resp <- httr2::request(criar_url) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "api_access_token" = token
    ) |>
    httr2::req_body_json(
      list(
        source_id = source_id,
        inbox_id = inbox_id,
        contact_id = contato_id,
        status = "open",
        custom_attributes = list(
          empresa_id = as.character(empresa),
          competencia = as.character(competencia),
          origem = as.character(origem)
        )
      )
    ) |>
    httr2::req_timeout(timeout) |>
    chatwoot_perform("account_conversa_criar")

  conversa_json <- httr2::resp_body_json(criar_resp)

  as.integer(conversa_json$id)
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
  cliente_whatsapp <- if ("telefone_whatsapp" %in% names(cliente)) {
    formatar_telefone_br(cliente$telefone_whatsapp)
  } else {
    ""
  }

  texto |>
    gsub("{{cliente_nome}}", as.character(cliente$cliente_nome), x = _, fixed = TRUE) |>
    gsub("{{cliente_email}}", cliente_email, x = _, fixed = TRUE) |>
    gsub("{{email_principal}}", cliente_email, x = _, fixed = TRUE) |>
    gsub("{{cliente_whatsapp}}", cliente_whatsapp, x = _, fixed = TRUE) |>
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
    enviar_pdfs = NULL,
    pasta_pdf = NULL
) {
  cliente_nome <- as.character(cliente$cliente_nome)
  telefone <- as.character(cliente$telefone_whatsapp)
  arquivos_pdf <- character(0)
  timeout <- chatwoot_timeout_segundos()
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
        cliente_nome = cliente_nome,
        pasta_pdf = pasta_pdf
      )

      arquivos_pdf <- as.character(verificacao_pdfs$arquivos_pdf)
    }

    telefone_normalizado <- normalizar_telefone_whatsapp(telefone)
    contact_identifier_esperado <- chatwoot_contact_identifier(
      empresa,
      telefone_normalizado
    )
    base_url <- sub("/+$", "", as.character(config$base_url))
    metodo_envio <- chatwoot_metodo_envio(config)

    if (identical(metodo_envio, "account_api")) {
      account_id <- trimws(as.character(config$account_id[1]))

      if (account_id == "") {
        stop("Account ID obrigatorio para o metodo Account API.")
      }

      inbox_id <- chatwoot_inbox_id(config)
      token <- as.character(config$api_access_token)

      contato <- chatwoot_buscar_ou_criar_contato_account(
        base_url = base_url,
        account_id = account_id,
        inbox_id = inbox_id,
        token = token,
        cliente_nome = cliente_nome,
        telefone_normalizado = telefone_normalizado,
        identifier = contact_identifier_esperado,
        timeout = timeout
      )

      source_id <- chatwoot_contact_source_id(
        contato = contato,
        inbox_id = inbox_id,
        fallback = contact_identifier_esperado
      )

      conversation_id <- chatwoot_obter_conversa_account(
        base_url = base_url,
        account_id = account_id,
        inbox_id = inbox_id,
        token = token,
        contato = contato,
        source_id = source_id,
        timeout = timeout,
        empresa = empresa,
        competencia = competencia,
        origem = origem
      )

      mensagem_url <- paste0(
        base_url,
        "/api/v1/accounts/",
        account_id,
        "/conversations/",
        conversation_id,
        "/messages"
      )

      mensagem_resp <- httr2::request(mensagem_url) |>
        httr2::req_headers(
          "Content-Type" = "application/json",
          "api_access_token" = token
        ) |>
        httr2::req_body_json(
          list(
            content = mensagem,
            message_type = "outgoing",
            private = FALSE,
            content_type = "text",
            content_attributes = list()
          )
        ) |>
        httr2::req_timeout(timeout) |>
        chatwoot_perform("account_mensagem")

      detalhe_envio <- chatwoot_resumo_resposta(
        mensagem_resp,
        metodo = "account_api",
        conversation_id = conversation_id
      )

      if (length(arquivos_pdf) > 0) {
        for (arquivo_pdf in arquivos_pdf) {
          httr2::request(mensagem_url) |>
            httr2::req_headers(
              "api_access_token" = token
            ) |>
            httr2::req_body_multipart(
              content = "",
              message_type = "outgoing",
              private = "false",
              `attachments[]` = curl::form_file(arquivo_pdf)
            ) |>
            httr2::req_timeout(timeout) |>
            chatwoot_perform("account_anexo")
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
        origem = paste0(origem, "_account_api"),
        competencia = competencia,
        detalhe = detalhe_envio
      )

      return(TRUE)
    }

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
          identifier = contact_identifier_esperado,
          name = cliente_nome,
          phone_number = telefone_normalizado,
          custom_attributes = list(
            empresa_id = as.character(empresa),
            cliente_nome = cliente_nome
          )
        )
      ) |>
      httr2::req_timeout(timeout) |>
      chatwoot_perform("contato")

    contato_json <- httr2::resp_body_json(contato_resp)
    contact_identifier <- contato_json$source_id

    if (is.null(contact_identifier) || contact_identifier == "") {
      contact_identifier <- contact_identifier_esperado
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
      httr2::req_timeout(timeout) |>
      chatwoot_perform("conversa")

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

    mensagem_resp <- httr2::request(mensagem_url) |>
      httr2::req_headers(
        "Content-Type" = "application/json",
        "api_access_token" = as.character(config$api_access_token)
      ) |>
      httr2::req_body_json(
        list(
          content = mensagem
        )
      ) |>
      httr2::req_timeout(timeout) |>
      chatwoot_perform("mensagem")

    detalhe_envio <- chatwoot_resumo_resposta(
      mensagem_resp,
      metodo = "public_api",
      conversation_id = conversation_id
    )

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
          httr2::req_timeout(timeout) |>
          chatwoot_perform("anexo")
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
      competencia = competencia,
      detalhe = detalhe_envio
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
