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

  readr::read_csv(
    caminho,
    show_col_types = FALSE,
    col_types = readr::cols(.default = "c")
  ) |>
    dplyr::mutate(
      ativo = as.logical(.data$ativo)
    ) |>
    dplyr::filter(
      .data$empresa_id == empresa,
      .data$ativo == TRUE
    ) |>
    dplyr::slice(1)
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
  texto |>
    gsub("{{cliente_nome}}", as.character(cliente$cliente_nome), x = _, fixed = TRUE) |>
    gsub("{{empresa_id}}", as.character(empresa), x = _, fixed = TRUE) |>
    gsub("{{competencia_pdfs}}", as.character(competencia), x = _, fixed = TRUE) |>
    gsub("{{mes_referencia}}", as.character(mes_email), x = _, fixed = TRUE) |>
    gsub("{{ano_referencia}}", as.character(ano_email), x = _, fixed = TRUE)
}

mensagem_whatsapp_padrao <- function(empresa, cliente, competencia, mes_email, ano_email) {
  substituir_variaveis_whatsapp(
    paste(
      "Olá, {{cliente_nome}}.",
      "Enviamos por e-mail os documentos referentes à competência {{competencia_pdfs}}.",
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
    ano_email = ""
) {
  cliente_nome <- as.character(cliente$cliente_nome)
  telefone <- as.character(cliente$telefone_whatsapp)
  mensagem <- if (is.null(mensagem) || trimws(mensagem) == "") {
    mensagem_whatsapp_padrao(
      empresa = empresa,
      cliente = cliente,
      competencia = competencia,
      mes_email = mes_email,
      ano_email = ano_email
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

    registrar_log_whatsapp(
      empresa = empresa,
      cliente_nome = cliente_nome,
      telefone = telefone_normalizado,
      mensagem = mensagem,
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
