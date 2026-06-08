# =========================================================
# FUNÇÕES DE DADOS
# =========================================================

normalizar_nome <- function(texto) {

  texto |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper() |>
    str_replace_all("[^A-Z0-9]+", " ") |>
    str_squish()
}

normalizar_telefone_br <- function(telefone) {
  telefone <- gsub("[^0-9]", "", as.character(telefone))

  telefone <- ifelse(
    startsWith(telefone, "55") & nchar(telefone) %in% c(12, 13),
    substr(telefone, 3, nchar(telefone)),
    telefone
  )

  telefone
}

formatar_telefone_br <- function(telefone) {
  original <- as.character(telefone)
  digitos <- normalizar_telefone_br(telefone)

  dplyr::case_when(
    is.na(digitos) | digitos == "" ~ "",
    nchar(digitos) == 11 ~ paste0(
      "(",
      substr(digitos, 1, 2),
      ") ",
      substr(digitos, 3, 7),
      "-",
      substr(digitos, 8, 11)
    ),
    nchar(digitos) == 10 ~ paste0(
      "(",
      substr(digitos, 1, 2),
      ") ",
      substr(digitos, 3, 6),
      "-",
      substr(digitos, 7, 10)
    ),
    TRUE ~ original
  )
}

carregar_dados_empresa_config <- function(empresa) {
  caminho <- file.path(pasta_raiz, "_config", "remetentes.csv")
  padrao <- list(
    empresa_nome = as.character(empresa),
    empresa_whatsapp = ""
  )

  if (!file.exists(caminho)) {
    return(padrao)
  }

  remetentes <- readr::read_csv(
    caminho,
    show_col_types = FALSE,
    col_types = readr::cols(.default = "c")
  )

  registro <- remetentes |>
    dplyr::filter(.data$empresa_id == empresa) |>
    dplyr::slice(1)

  if (nrow(registro) == 0) {
    return(padrao)
  }

  if (
    "empresa_nome" %in% names(registro) &&
      !is.na(registro$empresa_nome[1]) &&
      registro$empresa_nome[1] != ""
  ) {
    padrao$empresa_nome <- as.character(registro$empresa_nome[1])
  }

  if (
    "empresa_whatsapp" %in% names(registro) &&
      !is.na(registro$empresa_whatsapp[1])
  ) {
    padrao$empresa_whatsapp <- formatar_telefone_br(registro$empresa_whatsapp[1])
  }

  padrao
}

listar_empresas <- function() {

  dirs <- dir_ls(
    pasta_raiz,
    type = "directory",
    recurse = FALSE
  )

  dirs_validos <- keep(
    dirs,
    ~ dir_exists(file.path(.x, "modelos")) ||
      file_exists(file.path(.x, "destinatarios.csv"))
  )

  dirs_validos |>
    basename() |>
    discard(~ str_detect(.x, "^[0-9]+_")) |>
    sort()
}

buscar_competencias <- function(empresa) {

  caminho <- file.path(
    pasta_raiz,
    empresa,
    "clientes"
  )

  if (!dir_exists(caminho)) {
    return(character(0))
  }

  dir_ls(
    caminho,
    type = "directory",
    recurse = FALSE
  ) |>
    basename() |>
    sort(decreasing = TRUE)
}

buscar_pdfs_cliente <- function(
    empresa,
    competencia,
    cliente_nome
) {

  pasta_base <- file.path(
    pasta_raiz,
    empresa,
    "clientes",
    competencia
  )

  if (!dir_exists(pasta_base)) {

    return(
      list(
        arquivos_pdf = character(0),
        pasta_encontrada = NA_character_,
        total_pdfs = 0,
        status_pdfs = "Sem pasta"
      )
    )
  }

  pastas_clientes <- dir_ls(
    pasta_base,
    type = "directory",
    recurse = FALSE
  )

  if (length(pastas_clientes) == 0) {
    return(
      list(
        arquivos_pdf = character(0),
        pasta_encontrada = NA_character_,
        total_pdfs = 0,
        status_pdfs = "Sem pastas de clientes"
      )
    )
  }

  nome_ref <- normalizar_nome(cliente_nome)
  nomes_pastas <- normalizar_nome(basename(pastas_clientes))
  idx_exato <- which(nomes_pastas == nome_ref)
  idx_prefixo <- which(startsWith(nomes_pastas, paste0(nome_ref, " ")))
  idx_match <- unique(c(idx_exato, idx_prefixo))

  if (length(idx_match) == 0) {
    return(
      list(
        arquivos_pdf = character(0),
        pasta_encontrada = NA_character_,
        total_pdfs = 0,
        status_pdfs = "Pasta do cliente nao encontrada"
      )
    )
  }

  if (length(idx_match) > 1) {
    return(
      list(
        arquivos_pdf = character(0),
        pasta_encontrada = paste(basename(pastas_clientes[idx_match]), collapse = " | "),
        total_pdfs = 0,
        status_pdfs = "Mais de uma pasta possivel para o cliente"
      )
    )
  }

  pasta_cliente <- pastas_clientes[idx_match[1]]

  arquivos_pdf <- dir_ls(
    pasta_cliente,
    regexp = "\\.pdf$",
    recurse = FALSE
  )

  list(
    arquivos_pdf = arquivos_pdf,
    pasta_encontrada = basename(pasta_cliente),
    total_pdfs = length(arquivos_pdf),
    status_pdfs = ifelse(
      length(arquivos_pdf) > 0,
      paste0(
        "OK - ",
        length(arquivos_pdf),
        " PDF(s)"
      ),
      "Sem PDFs"
    )
  )
}

criar_backup_completo <- function() {

  destino <- file.path(
    pasta_raiz,
    "backups",
    paste0(
      "backup-",
      format(Sys.time(), "%Y%m%d-%H%M%S")
    )
  )

  dir_create(destino)

  arquivos <- c(
    file.path(
      pasta_raiz,
      "_config",
      "smtp.csv"
    ),
    file.path(
      pasta_raiz,
      "_config",
      "remetentes.csv"
    )
  )

  arquivos <- arquivos[file_exists(arquivos)]

  for (arquivo in arquivos) {

    destino_arquivo <- file.path(
      destino,
      basename(arquivo)
    )

    file_copy(
      arquivo,
      destino_arquivo,
      overwrite = TRUE
    )
  }

  destino
}
