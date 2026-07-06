# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  autenticado <- reactiveVal(FALSE)
  login_msg <- reactiveVal("")

  output$ui_principal <- renderUI({
    if (autenticado()) {
      ui_app
    } else {
      ui_login
    }
  })

  observeEvent(input$login_payload, {
    payload <- input$login_payload

    if (validar_login(payload$usuario, payload$senha)) {
      autenticado(TRUE)
    } else {
      login_msg("Usuário ou senha inválidos.")
    }
  })

  output$login_msg <- renderText({
    login_msg()
  })

  mensagem_backup <- reactiveVal("Aguardando backup.")
  resultado_envio <- reactiveVal("Aguardando envio.")
  disparo_processamento <- reactiveVal(NULL)
  disparo_proximo_ciclo <- reactiveVal(Sys.time())

  session$userData$remetente_modo_novo <- FALSE
  session$userData$smtp_modo_novo <- FALSE

  atualizar_contador <- function(contador) {
    contador(isolate(contador()) + 1)
  }

  criar_backup_seguro <- function() {
    tryCatch(
      criar_backup_completo(),
      error = function(e) {
        message("Backup não criado: ", conditionMessage(e))
        NULL
      }
    )
  }

  referencia_email_por_competencia <- function(competencia) {
    competencia <- trimws(as.character(competencia))

    if (!grepl("^\\d{4}-\\d{2}$", competencia)) {
      return(NULL)
    }

    ano <- as.integer(substr(competencia, 1, 4))
    mes <- as.integer(substr(competencia, 6, 7))

    if (is.na(ano) || is.na(mes) || mes < 1 || mes > 12) {
      return(NULL)
    }

    mes <- mes - 1

    if (mes == 0) {
      mes <- 12
      ano <- ano - 1
    }

    nomes_meses <- c(
      "Janeiro",
      "Fevereiro",
      "Março",
      "Abril",
      "Maio",
      "Junho",
      "Julho",
      "Agosto",
      "Setembro",
      "Outubro",
      "Novembro",
      "Dezembro"
    )

    list(
      mes = nomes_meses[mes],
      ano = ano
    )
  }

  atualizar_referencia_email <- function(competencia, input_mes, input_ano) {
    referencia <- referencia_email_por_competencia(competencia)

    if (is.null(referencia)) {
      return()
    }

    updateTextInput(session, input_mes, value = referencia$mes)
    updateNumericInput(session, input_ano, value = referencia$ano)
  }

  datatable_padrao <- function(dados, ..., page_length = 10, scroll_x = TRUE) {
    DT::datatable(
      dados,
      ...,
      rownames = FALSE,
      options = list(
        pageLength = page_length,
        scrollX = scroll_x,
        language = list(
          search = "Pesquisa:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ até _END_ de _TOTAL_ registros",
          infoEmpty = "Mostrando 0 até 0 de 0 registros",
          zeroRecords = "Nenhum registro encontrado",
          emptyTable = "Nenhum dado disponível",
          paginate = list(
            previous = "Anterior",
            `next` = "Próximo"
          )
        )
      )
    )
  }

  # =========================================================
  # DASHBOARD
  # =========================================================

  dashboard_logs <- reactive({
    caminho <- file.path(pasta_raiz, "logs", "envios.csv")

    if (!file.exists(caminho)) {
      return(tibble::tibble())
    }

    readr::read_csv(caminho, show_col_types = FALSE)
  })

  dashboard_fila <- reactive({
    caminho <- file.path(pasta_raiz, "logs", "fila_envio.csv")

    if (!file.exists(caminho)) {
      return(tibble::tibble())
    }

    readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )
  })

  dashboard_whatsapp <- reactive({
    caminho <- caminho_log_whatsapp()

    if (!file.exists(caminho)) {
      return(tibble::tibble())
    }

    readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )
  })

  output$dash_empresas <- renderText({
    length(listar_empresas())
  })

  output$dash_clientes_ativos <- renderText({
    total <- 0

    for (empresa in listar_empresas()) {
      caminho <- file.path(pasta_raiz, empresa, "destinatarios.csv")

      if (file.exists(caminho)) {
        dados <- readr::read_csv(
          caminho,
          show_col_types = FALSE,
          col_types = readr::cols(.default = "c")
        )

        if ("ativo" %in% names(dados)) {
          total <- total + sum(as.logical(dados$ativo), na.rm = TRUE)
        }
      }
    }

    total
  })

  output$dash_envios_ok <- renderText({
    logs <- dashboard_logs()

    if (nrow(logs) == 0 || !"status" %in% names(logs)) {
      return(0)
    }

    sum(logs$status == "enviado", na.rm = TRUE)
  })

  output$dash_envios_erro <- renderText({
    logs <- dashboard_logs()

    if (nrow(logs) == 0 || !"status" %in% names(logs)) {
      return(0)
    }

    sum(logs$status == "erro", na.rm = TRUE)
  })

  output$dash_fila_pendente <- renderText({
    fila <- dashboard_fila()

    if (nrow(fila) == 0 || !"status" %in% names(fila)) {
      return(0)
    }

    sum(fila$status == "pendente", na.rm = TRUE)
  })

  output$dash_whatsapp_ok <- renderText({
    logs <- dashboard_whatsapp()

    if (nrow(logs) == 0 || !"status" %in% names(logs)) {
      return(0)
    }

    sum(logs$status == "enviado", na.rm = TRUE)
  })

  output$dash_whatsapp_erro <- renderText({
    logs <- dashboard_whatsapp()

    if (nrow(logs) == 0 || !"status" %in% names(logs)) {
      return(0)
    }

    sum(logs$status == "erro", na.rm = TRUE)
  })

  output$dash_app_version <- renderText({
    versao <- Sys.getenv("APP_VERSION", unset = "dev")

    if (is.na(versao) || versao == "") {
      return("dev")
    }

    substr(versao, 1, 12)
  })

  output$dash_email_worker <- renderText({
    caminho_worker <- file.path(pasta_raiz, "R", "08-email-worker.R")

    if (file.exists(caminho_worker)) {
      return("OK")
    }

    "ausente"
  })

  output$dash_envios_empresa <- DT::renderDT({
    logs <- dashboard_logs()

    if (nrow(logs) == 0 || !"empresa" %in% names(logs)) {
      return(
        datatable_padrao(
          tibble::tibble(
            Empresa = character(),
            Status = character(),
            Total = numeric()
          )
        )
      )
    }

    resumo <- logs |>
      dplyr::count(empresa, status, name = "total") |>
      dplyr::arrange(empresa, status)

    datatable_padrao(
      resumo,
      colnames = c("Empresa", "Status", "Total")
    )
  })

  output$dash_ultimos_envios <- DT::renderDT({
    logs <- dashboard_logs()

    if (nrow(logs) == 0) {
      return(
        datatable_padrao(
          tibble::tibble(
            `Data/Hora` = character(),
            Empresa = character(),
            Cliente = character(),
            Status = character()
          )
        )
      )
    }

    ultimos <- logs |>
      dplyr::arrange(dplyr::desc(data_hora)) |>
      dplyr::select(data_hora, empresa, cliente_nome, status) |>
      utils::head(10)

    datatable_padrao(
      ultimos,
      colnames = c("Data/Hora", "Empresa", "Cliente", "Status")
    )
  })

  # =========================================================
  # CLIENTES
  # =========================================================

  clientes_msg <- reactiveVal("Aguardando edição.")
  cliente_selecionado <- reactiveVal(NULL)
  clientes_refresh <- reactiveVal(0)
  clientes_page_length <- reactiveVal(10)

  atualizar_clientes <- function() {
    atualizar_contador(clientes_refresh)
  }

  ler_clientes_empresa <- function(empresa) {
    caminho <- file.path(pasta_raiz, empresa, "destinatarios.csv")

    if (!file.exists(caminho)) {
      return(
        tibble::tibble(
          cliente_nome = character(),
          email_principal = character(),
          email_copias = character(),
          telefone_whatsapp = character(),
          ativo = logical(),
          observacao = character()
        )
      )
    }

    readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    ) |>
      dplyr::mutate(
        ativo = as.logical(.data$ativo)
      )
  }

  clientes_dados <- reactive({
    clientes_refresh()
    req(input$clientes_empresa)
    ler_clientes_empresa(input$clientes_empresa)
  })

  observeEvent(input$clientes_tabela_page_length, {
    valor <- as.integer(input$clientes_tabela_page_length)

    if (!is.na(valor) && valor > 0) {
      clientes_page_length(valor)
    }
  })

  output$clientes_tabela <- DT::renderDT({
    dados_tabela <- clientes_dados()

    if ("telefone_whatsapp" %in% names(dados_tabela)) {
      dados_tabela$telefone_whatsapp <- formatar_telefone_br(
        dados_tabela$telefone_whatsapp
      )
    }

    DT::datatable(
      dados_tabela,
      selection = "single",
      rownames = FALSE,
      callback = DT::JS(
        "table.on('length.dt', function(e, settings, len) {",
        "  Shiny.setInputValue('clientes_tabela_page_length', len, {priority: 'event'});",
        "});"
      ),
      colnames = c(
        "Cliente",
        "Email Principal",
        "Emails Cópia",
        "WhatsApp",
        "Ativo",
        "Observação"
      ),
      options = list(
        pageLength = clientes_page_length(),
        stateSave = TRUE,
        scrollX = TRUE,
        autoWidth = FALSE,
        columnDefs = list(
          list(
            targets = 3,
            width = "170px",
            className = "dt-nowrap"
          )
        ),
        language = list(
          search = "Pesquisa:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ até _END_ de _TOTAL_ registros",
          infoEmpty = "Mostrando 0 até 0 de 0 registros",
          zeroRecords = "Nenhum registro encontrado",
          emptyTable = "Nenhum dado disponível",
          paginate = list(
            previous = "Anterior",
            `next` = "Próximo"
          )
        )
      )
    )
  })

  observeEvent(input$clientes_tabela_rows_selected, {
    linha <- input$clientes_tabela_rows_selected
    req(length(linha) == 1)

    dados <- clientes_dados()
    cliente <- dados[linha, ]

    cliente_selecionado(linha)

    updateTextInput(session, "cliente_nome_edit", value = cliente$cliente_nome)
    updateTextInput(session, "cliente_email", value = cliente$email_principal)
    updateTextInput(session, "cliente_copias", value = cliente$email_copias)
    updateTextInput(session, "cliente_whatsapp", value = formatar_telefone_br(cliente$telefone_whatsapp))
    updateCheckboxInput(session, "cliente_ativo", value = as.logical(cliente$ativo))
    updateTextAreaInput(session, "cliente_observacao", value = cliente$observacao)

    clientes_msg(paste("Cliente carregado:", cliente$cliente_nome))
  })

  observeEvent(input$novo_cliente, {
    cliente_selecionado(NULL)

    updateTextInput(session, "cliente_nome_edit", value = "")
    updateTextInput(session, "cliente_email", value = "")
    updateTextInput(session, "cliente_copias", value = "")
    updateTextInput(session, "cliente_whatsapp", value = "")
    updateCheckboxInput(session, "cliente_ativo", value = TRUE)
    updateTextAreaInput(session, "cliente_observacao", value = "")

    clientes_msg("Novo cadastro.")
  })

  observeEvent(input$salvar_cliente, {
    req(input$clientes_empresa)

    caminho <- file.path(pasta_raiz, input$clientes_empresa, "destinatarios.csv")
    dados <- clientes_dados()

    novo_registro <- tibble::tibble(
      cliente_nome = as.character(input$cliente_nome_edit),
      email_principal = as.character(input$cliente_email),
      email_copias = as.character(input$cliente_copias),
      telefone_whatsapp = normalizar_telefone_br(input$cliente_whatsapp),
      ativo = as.logical(input$cliente_ativo),
      observacao = as.character(input$cliente_observacao)
    )

    linha <- cliente_selecionado()

    if (is.null(linha)) {
      dados <- dplyr::bind_rows(dados, novo_registro)
    } else {
      dados[linha, ] <- novo_registro
    }

    criar_backup_seguro()
    readr::write_csv(dados, caminho)
    atualizar_clientes()
    cliente_selecionado(NULL)

    clientes_msg("Cliente salvo com sucesso.")
  })

  observeEvent(input$excluir_cliente, {
    req(input$clientes_empresa)

    linha <- cliente_selecionado()

    if (is.null(linha)) {
      clientes_msg("Selecione um cliente antes de excluir.")
      return()
    }

    caminho <- file.path(pasta_raiz, input$clientes_empresa, "destinatarios.csv")
    dados <- clientes_dados()
    cliente_nome <- dados$cliente_nome[linha]

    dados <- dados[-linha, ]

    criar_backup_seguro()
    readr::write_csv(dados, caminho)
    atualizar_clientes()
    cliente_selecionado(NULL)

    clientes_msg(paste("Cliente excluído:", cliente_nome))
  })

  observeEvent(input$importar_clientes, {
    req(input$clientes_empresa)
    req(input$importar_clientes)

    caminho <- file.path(pasta_raiz, input$clientes_empresa, "destinatarios.csv")

    importado <- readr::read_csv(
      input$importar_clientes$datapath,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    ) |>
      dplyr::mutate(
        telefone_whatsapp = normalizar_telefone_br(.data$telefone_whatsapp),
        ativo = as.logical(.data$ativo)
      )

    dados_atuais <- clientes_dados()

    if (isTRUE(input$substituir_clientes)) {
      dados_finais <- importado
    } else {
      dados_finais <- dplyr::bind_rows(dados_atuais, importado) |>
        dplyr::distinct(cliente_nome, .keep_all = TRUE)
    }

    criar_backup_seguro()
    readr::write_csv(dados_finais, caminho)
    atualizar_clientes()

    clientes_msg("Importação concluída com sucesso.")
  })

  observeEvent(input$enviar_whatsapp_cliente, {
    req(input$clientes_empresa)

    linha <- cliente_selecionado()

    if (is.null(linha)) {
      clientes_msg("Selecione um cliente antes de enviar WhatsApp.")
      return()
    }

    cliente <- clientes_dados()[linha, ]

    tryCatch(
      {
        enviar_whatsapp_cliente(
          empresa = input$clientes_empresa,
          cliente = cliente,
          mensagem = input$cliente_whatsapp_msg,
          origem = "manual_cliente"
        )

        clientes_msg(paste("WhatsApp enviado para:", cliente$cliente_nome))
      },
      error = function(e) {
        clientes_msg(paste("Erro WhatsApp:", conditionMessage(e)))
      }
    )
  })

  output$clientes_msg <- renderText({
    clientes_msg()
  })

  # =========================================================
  # PDFS
  # =========================================================

  pdf_msg <- reactiveVal("Aguardando envio.")
  pdf_refresh <- reactiveVal(0)

  atualizar_pdfs <- function() {
    atualizar_contador(pdf_refresh)
    atualizar_disparo()
    atualizar_fila()
  }

  limpar_segmento_caminho <- function(texto) {
    texto <- trimws(as.character(texto))
    texto <- gsub("[/\\\\:*?\"<>|]", " ", texto)
    str_squish(texto)
  }

  validar_competencia_pdf <- function(competencia) {
    competencia <- trimws(as.character(competencia))

    if (!grepl("^\\d{4}-\\d{2}$", competencia)) {
      stop("Informe a competência no formato AAAA-MM. Exemplo: 2026-06.")
    }

    competencia
  }

  caminho_competencia_pdf <- function(empresa, competencia) {
    file.path(
      pasta_raiz,
      empresa,
      "clientes",
      validar_competencia_pdf(competencia)
    )
  }

  caminho_cliente_pdf <- function(empresa, competencia, cliente_nome) {
    file.path(
      caminho_competencia_pdf(empresa, competencia),
      limpar_segmento_caminho(cliente_nome)
    )
  }

  aplicar_associacao_pasta_pdf <- function(empresa, competencia, pasta_pdf, cliente_nome) {
    pasta_competencia <- caminho_competencia_pdf(empresa, competencia)
    origem <- file.path(pasta_competencia, pasta_pdf)
    destino <- caminho_cliente_pdf(empresa, competencia, cliente_nome)

    if (!dir_exists(origem)) {
      stop("Pasta de origem nao encontrada.")
    }

    origem_norm <- normalizePath(origem, winslash = "/", mustWork = TRUE)
    destino_norm <- normalizePath(dirname(destino), winslash = "/", mustWork = TRUE)
    base_norm <- normalizePath(pasta_competencia, winslash = "/", mustWork = TRUE)

    if (!startsWith(origem_norm, paste0(base_norm, "/"))) {
      stop("Pasta de origem invalida.")
    }

    if (!startsWith(destino_norm, base_norm)) {
      stop("Pasta de destino invalida.")
    }

    arquivos_pdf <- dir_ls(
      origem,
      regexp = "\\.pdf$",
      recurse = FALSE
    )

    if (length(arquivos_pdf) == 0) {
      stop("Pasta selecionada nao possui PDFs.")
    }

    dir_create(destino, recurse = TRUE)
    destino_final_norm <- normalizePath(destino, winslash = "/", mustWork = TRUE)

    if (!identical(origem_norm, destino_final_norm)) {
      file_copy(
        arquivos_pdf,
        file.path(destino, basename(arquivos_pdf)),
        overwrite = TRUE
      )

      dir_delete(origem)
    }

    list(
      pasta_pdf = basename(destino),
      total_pdfs = length(dir_ls(destino, regexp = "\\.pdf$", recurse = FALSE))
    )
  }

  validar_pasta_competencia_para_remocao <- function(empresa, competencia) {
    pasta_clientes <- normalizePath(
      file.path(pasta_raiz, empresa, "clientes"),
      winslash = "/",
      mustWork = TRUE
    )

    pasta_competencia <- normalizePath(
      caminho_competencia_pdf(empresa, competencia),
      winslash = "/",
      mustWork = TRUE
    )

    if (!startsWith(pasta_competencia, paste0(pasta_clientes, "/"))) {
      stop("Caminho da competência inválido para remoção.")
    }

    pasta_competencia
  }

  criar_backup_competencia_pdf <- function(empresa, competencia) {
    pasta_competencia <- validar_pasta_competencia_para_remocao(
      empresa,
      competencia
    )

    destino_dir <- file.path(
      pasta_raiz,
      "backups",
      "pdfs",
      empresa
    )

    dir_create(destino_dir, recurse = TRUE)

    destino_zip <- file.path(
      destino_dir,
      paste0(competencia, ".zip")
    )

    if (file_exists(destino_zip)) {
      destino_zip <- file.path(
        destino_dir,
        paste0(
          competencia,
          "-",
          format(Sys.time(), "%Y%m%d-%H%M%S"),
          ".zip"
        )
      )
    }

    wd_anterior <- getwd()
    on.exit(setwd(wd_anterior), add = TRUE)
    setwd(dirname(pasta_competencia))

    status_zip <- utils::zip(
      zipfile = destino_zip,
      files = basename(pasta_competencia),
      flags = "-r"
    )

    if (!file_exists(destino_zip) || !identical(status_zip, 0L)) {
      stop(paste("Backup ZIP não criado para:", competencia))
    }

    destino_zip
  }

  output$ui_pdf_cliente <- renderUI({
    req(input$pdf_empresa)

    clientes <- ler_clientes_empresa(input$pdf_empresa)
    escolhas <- sort(as.character(clientes$cliente_nome))
    escolhas <- escolhas[!is.na(escolhas) & escolhas != ""]

    selectInput(
      "pdf_cliente",
      "Cliente",
      choices = escolhas,
      selected = if (length(escolhas) > 0) escolhas[1] else character(0)
    )
  })

  pdf_resumo <- reactive({
    pdf_refresh()
    req(input$pdf_empresa)

    competencia <- trimws(as.character(input$pdf_competencia))

    if (!grepl("^\\d{4}-\\d{2}$", competencia)) {
      return(
        tibble::tibble(
          Cliente = character(),
          PDFs = integer(),
          Pasta = character()
        )
      )
    }

    pasta_competencia <- caminho_competencia_pdf(input$pdf_empresa, competencia)

    if (!dir_exists(pasta_competencia)) {
      return(
        tibble::tibble(
          Cliente = character(),
          PDFs = integer(),
          Pasta = character()
        )
      )
    }

    pastas <- dir_ls(
      pasta_competencia,
      type = "directory",
      recurse = FALSE
    )

    pastas <- pastas[basename(pastas) != competencia]

    if (length(pastas) == 0) {
      return(
        tibble::tibble(
          Cliente = character(),
          PDFs = integer(),
          Pasta = character()
        )
      )
    }

    tibble::tibble(
      Cliente = basename(pastas),
      PDFs = purrr::map_int(
        pastas,
        ~ length(dir_ls(.x, regexp = "\\.pdf$", recurse = FALSE))
      ),
      Pasta = as.character(pastas)
    ) |>
      dplyr::arrange(.data$Cliente)
  })

  output$pdf_tabela <- DT::renderDT({
    datatable_padrao(
      pdf_resumo(),
      page_length = 15
    )
  })

  pdf_pendencias <- reactive({
    resumo <- pdf_resumo()

    if (nrow(resumo) == 0) {
      return(resumo)
    }

    clientes <- ler_clientes_empresa(input$pdf_empresa)

    if (nrow(clientes) == 0 || !"cliente_nome" %in% names(clientes)) {
      return(
        resumo |>
          dplyr::filter(.data$PDFs > 0)
      )
    }

    verificacoes <- purrr::map(
      clientes$cliente_nome,
      ~ buscar_pdfs_cliente(input$pdf_empresa, input$pdf_competencia, .x)
    )

    pastas_associadas <- purrr::map_chr(verificacoes, "pasta_encontrada")
    pastas_associadas <- pastas_associadas[!is.na(pastas_associadas) & pastas_associadas != ""]

    resumo |>
      dplyr::filter(
        .data$PDFs > 0,
        !.data$Cliente %in% pastas_associadas
      )
  })

  output$pdf_pendencias_tabela <- DT::renderDT({
    datatable_padrao(
      pdf_pendencias(),
      page_length = 10
    )
  })

  sugerir_cliente_pdf <- function(pasta_nome, clientes) {
    clientes <- clientes[!is.na(clientes) & clientes != ""]

    if (length(clientes) == 0) {
      return(
        list(
          cliente = "",
          score = 0
        )
      )
    }

    pasta_norm <- normalizar_nome(pasta_nome)
    clientes_norm <- normalizar_nome(clientes)

    distancias <- stringdist::stringdist(
      pasta_norm,
      clientes_norm,
      method = "jw",
      p = 0.1
    )

    idx <- which.min(distancias)
    score <- round((1 - distancias[idx]) * 100, 1)

    list(
      cliente = clientes[idx],
      score = score
    )
  }

  pdf_sugestoes <- reactive({
    pendencias <- pdf_pendencias()

    if (nrow(pendencias) == 0) {
      return(
        tibble::tibble(
          Pasta = character(),
          PDFs = integer(),
          Cliente_Sugerido = character(),
          Similaridade = numeric(),
          Acao = character()
        )
      )
    }

    clientes <- ler_clientes_empresa(input$pdf_empresa)

    if (nrow(clientes) == 0 || !"cliente_nome" %in% names(clientes)) {
      return(
        tibble::tibble(
          Pasta = pendencias$Cliente,
          PDFs = pendencias$PDFs,
          Cliente_Sugerido = "",
          Similaridade = 0,
          Acao = "Sem cadastro de clientes"
        )
      )
    }

    nomes_clientes <- as.character(clientes$cliente_nome)
    sugestoes <- purrr::map(pendencias$Cliente, sugerir_cliente_pdf, clientes = nomes_clientes)
    scores <- purrr::map_dbl(sugestoes, "score")
    limite <- suppressWarnings(as.numeric(input$pdf_score_sugestao))

    if (is.na(limite)) {
      limite <- 88
    }

    tibble::tibble(
      Pasta = as.character(pendencias$Cliente),
      PDFs = as.integer(pendencias$PDFs),
      Cliente_Sugerido = purrr::map_chr(sugestoes, "cliente"),
      Similaridade = scores,
      Acao = ifelse(scores >= limite, "Pode aplicar", "Revisar manualmente")
    ) |>
      dplyr::arrange(dplyr::desc(.data$Similaridade), .data$Pasta)
  })

  output$pdf_sugestoes_tabela <- DT::renderDT({
    datatable_padrao(
      pdf_sugestoes(),
      page_length = 10,
      colnames = c(
        "Pasta",
        "PDFs",
        "Cliente Sugerido",
        "Similaridade %",
        "Acao"
      )
    )
  })

  output$ui_pdf_pasta_sem_cliente <- renderUI({
    pendencias <- pdf_pendencias()
    escolhas <- sort(as.character(pendencias$Cliente))

    selectInput(
      "pdf_pasta_alias",
      "Pasta sem associacao",
      choices = escolhas,
      selected = if (length(escolhas) > 0) escolhas[1] else character(0)
    )
  })

  output$ui_pdf_cliente_alias <- renderUI({
    req(input$pdf_empresa)

    clientes <- ler_clientes_empresa(input$pdf_empresa)
    escolhas <- sort(as.character(clientes$cliente_nome))
    escolhas <- escolhas[!is.na(escolhas) & escolhas != ""]

    selectInput(
      "pdf_cliente_alias",
      "Cliente correto",
      choices = escolhas,
      selected = if (length(escolhas) > 0) escolhas[1] else character(0)
    )
  })

  salvar_associacao_pdf_selecionada <- function() {
    req(input$pdf_empresa)
    req(input$pdf_competencia)
    req(input$pdf_pasta_alias)
    req(input$pdf_cliente_alias)

    salvar_pdf_alias(
      empresa = input$pdf_empresa,
      pasta_pdf = input$pdf_pasta_alias,
      cliente_nome = input$pdf_cliente_alias
    )

    resultado <- aplicar_associacao_pasta_pdf(
      empresa = input$pdf_empresa,
      competencia = input$pdf_competencia,
      pasta_pdf = input$pdf_pasta_alias,
      cliente_nome = input$pdf_cliente_alias
    )

    atualizar_pdfs()
    resultado
  }

  observeEvent(input$associar_pasta_pdf, {
    tryCatch({
      resultado <- salvar_associacao_pdf_selecionada()
      pdf_msg(paste(
        "Associacao salva:",
        input$pdf_pasta_alias,
        "->",
        input$pdf_cliente_alias,
        "| PDFs:",
        resultado$total_pdfs
      ))
    }, error = function(e) {
      pdf_msg(paste("Erro ao salvar associacao:", conditionMessage(e)))
    })
  })

  observeEvent(input$associar_pasta_pdf_fila, {
    tryCatch({
      associacao <- salvar_associacao_pdf_selecionada()

      cliente <- ler_clientes_empresa(input$pdf_empresa) |>
        dplyr::filter(.data$cliente_nome == input$pdf_cliente_alias) |>
        dplyr::slice(1)

      if (nrow(cliente) == 0) {
        stop("Cliente selecionado nao encontrado.")
      }

      verificacao <- buscar_pdfs_cliente(
        empresa = input$pdf_empresa,
        competencia = input$pdf_competencia,
        cliente_nome = input$pdf_cliente_alias,
        pasta_pdf = associacao$pasta_pdf
      )

      if (verificacao$total_pdfs <= 0) {
        stop("A pasta selecionada nao possui PDFs validos.")
      }

      fila <- carregar_fila()
      if (!"pasta_pdf" %in% names(fila)) {
        fila$pasta_pdf <- ""
      }

      idx <- which(
        fila$empresa == input$pdf_empresa &
          fila$competencia == input$pdf_competencia &
          fila$cliente_nome == input$pdf_cliente_alias
      )

      novo_item <- tibble::tibble(
        empresa = as.character(input$pdf_empresa),
        competencia = as.character(input$pdf_competencia),
        cliente_nome = as.character(input$pdf_cliente_alias),
        email_principal = as.character(cliente$email_principal[1]),
        total_pdfs = as.integer(verificacao$total_pdfs),
        pasta_pdf = as.character(associacao$pasta_pdf),
        status = "pendente",
        data_inclusao = as.character(Sys.time())
      )

      if (length(idx) > 0) {
        fila[idx[1], names(novo_item)] <- novo_item[1, ]
      } else {
        fila <- dplyr::bind_rows(fila, novo_item)
      }

      criar_backup_seguro()
      salvar_fila(fila)
      atualizar_fila()
      atualizar_pdfs()

      pdf_msg(paste("Associacao salva e item adicionado a fila:", input$pdf_cliente_alias))
    }, error = function(e) {
      pdf_msg(paste("Erro ao adicionar a fila:", conditionMessage(e)))
    })
  })

  observeEvent(input$aplicar_sugestoes_pdf, {
    req(input$pdf_empresa)
    req(input$pdf_competencia)

    if (!isTRUE(input$pdf_confirmar_sugestoes)) {
      pdf_msg("Marque a confirmacao antes de aplicar sugestoes.")
      return()
    }

    tryCatch({
      sugestoes <- pdf_sugestoes()
      limite <- suppressWarnings(as.numeric(input$pdf_score_sugestao))

      if (is.na(limite)) {
        limite <- 88
      }

      aplicar <- sugestoes |>
        dplyr::filter(
          .data$Similaridade >= limite,
          .data$Cliente_Sugerido != ""
        )

      if (nrow(aplicar) == 0) {
        pdf_msg("Nenhuma sugestao atingiu a similaridade minima.")
        return()
      }

      resultado <- character(0)

      for (i in seq_len(nrow(aplicar))) {
        item <- aplicar[i, ]

        salvar_pdf_alias(
          empresa = input$pdf_empresa,
          pasta_pdf = item$Pasta,
          cliente_nome = item$Cliente_Sugerido
        )

        associacao <- aplicar_associacao_pasta_pdf(
          empresa = input$pdf_empresa,
          competencia = input$pdf_competencia,
          pasta_pdf = item$Pasta,
          cliente_nome = item$Cliente_Sugerido
        )

        resultado <- c(
          resultado,
          paste0(
            item$Pasta,
            " -> ",
            item$Cliente_Sugerido,
            " (",
            item$Similaridade,
            "%, PDFs: ",
            associacao$total_pdfs,
            ")"
          )
        )
      }

      atualizar_pdfs()
      updateCheckboxInput(session, "pdf_confirmar_sugestoes", value = FALSE)
      pdf_msg(paste(c(
        paste("Sugestoes aplicadas:", nrow(aplicar)),
        resultado
      ), collapse = "\n"))
    }, error = function(e) {
      pdf_msg(paste("Erro ao aplicar sugestoes:", conditionMessage(e)))
    })
  })

  observeEvent(input$pdf_competencia, {
    atualizar_contador(pdf_refresh)
  })

  observeEvent(input$pdf_empresa, {
    atualizar_contador(pdf_refresh)
  })

  observeEvent(input$salvar_pdfs_cliente, {
    req(input$pdf_empresa)
    req(input$pdf_cliente)
    req(input$pdf_arquivos)

    tryCatch({
      destino <- caminho_cliente_pdf(
        input$pdf_empresa,
        input$pdf_competencia,
        input$pdf_cliente
      )

      dir_create(destino, recurse = TRUE)

      arquivos <- input$pdf_arquivos
      nomes <- basename(arquivos$name)
      nomes <- limpar_segmento_caminho(nomes)

      if (!all(grepl("\\.pdf$", nomes, ignore.case = TRUE))) {
        stop("Envie apenas arquivos PDF.")
      }

      destinos <- file.path(destino, nomes)

      file_copy(
        arquivos$datapath,
        destinos,
        overwrite = TRUE
      )

      atualizar_pdfs()

      pdf_msg(
        paste0(
          length(destinos),
          " PDF(s) salvo(s) em ",
          input$pdf_empresa,
          "/clientes/",
          validar_competencia_pdf(input$pdf_competencia),
          "/",
          limpar_segmento_caminho(input$pdf_cliente)
        )
      )
    },
    error = function(e) {
      pdf_msg(paste("Erro ao salvar PDFs:", conditionMessage(e)))
    })
  })

  entrada_zip_segura <- function(nome) {
    nome <- gsub("\\\\", "/", nome)
    nome <- sub("/+$", "", nome)
    partes <- strsplit(nome, "/", fixed = TRUE)[[1]]

    nome != "" &&
      !grepl("^/|^[A-Za-z]:", nome) &&
      !any(partes %in% c("..", ""))
  }

  interpretar_pdf_zip <- function(nome_zip, competencia) {
    partes <- strsplit(nome_zip, "/", fixed = TRUE)[[1]]

    if (length(partes) >= 2 && partes[1] == competencia) {
      partes <- partes[-1]
    }

    if (length(partes) < 2) {
      return(NULL)
    }

    list(
      cliente = limpar_segmento_caminho(partes[1]),
      arquivo = limpar_segmento_caminho(tail(partes, 1)),
      partes = partes
    )
  }

  caminho_log_importacao_pdfs <- function() {
    file.path(pasta_raiz, "logs", "pdf_importacao.csv")
  }

  registrar_log_importacao_pdfs <- function(dados) {
    caminho <- caminho_log_importacao_pdfs()
    dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)

    if (file.exists(caminho)) {
      atual <- readr::read_csv(
        caminho,
        show_col_types = FALSE,
        col_types = readr::cols(.default = "c")
      )

      dados <- dplyr::bind_rows(atual, dados)
    }

    readr::write_csv(dados, caminho)
  }

  observeEvent(input$importar_zip_pdfs, {
    req(input$pdf_empresa)
    req(input$pdf_zip)

    tryCatch({
      competencia <- validar_competencia_pdf(input$pdf_competencia)
      entradas <- utils::unzip(input$pdf_zip$datapath, list = TRUE)

      if (nrow(entradas) == 0) {
        stop("ZIP vazio.")
      }

      nomes_zip <- gsub("\\\\", "/", entradas$Name)

      if (!all(purrr::map_lgl(nomes_zip, entrada_zip_segura))) {
        stop("ZIP possui caminhos inválidos.")
      }

      pdfs_zip <- entradas |>
        dplyr::mutate(nome_zip = nomes_zip) |>
        dplyr::filter(
          grepl("\\.pdf$", .data$nome_zip, ignore.case = TRUE)
        )

      pdfs_validos <- purrr::map(
        pdfs_zip$nome_zip,
        interpretar_pdf_zip,
        competencia = competencia
      )

      manter <- purrr::map_lgl(pdfs_validos, ~ !is.null(.x))
      pdfs_zip <- pdfs_zip[manter, ]
      pdfs_validos <- pdfs_validos[manter]

      if (nrow(pdfs_zip) == 0) {
        stop("ZIP deve conter PDFs em Cliente/arquivo.pdf ou Competência/Cliente/arquivo.pdf.")
      }

      import_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
      plano_importacao <- purrr::map_dfr(seq_len(nrow(pdfs_zip)), function(i) {
        item_zip <- pdfs_validos[[i]]
        cliente_origem <- item_zip$cliente
        cliente_destino <- cliente_origem
        cliente_alias <- resolver_pdf_alias(input$pdf_empresa, cliente_origem)
        alias_aplicado <- FALSE

        if (!is.na(cliente_alias) && trimws(cliente_alias) != "") {
          cliente_destino <- cliente_alias
          alias_aplicado <- TRUE
        }

        tibble::tibble(
          data_hora = as.character(Sys.time()),
          import_id = import_id,
          empresa = as.character(input$pdf_empresa),
          competencia = as.character(competencia),
          arquivo_zip = as.character(input$pdf_zip$name),
          nome_zip = as.character(pdfs_zip$nome_zip[i]),
          cliente_origem = as.character(cliente_origem),
          cliente_destino = as.character(cliente_destino),
          arquivo = as.character(item_zip$arquivo),
          alias_aplicado = as.character(alias_aplicado),
          status = "planejado",
          detalhe = ""
        )
      })

      clientes_origem <- unique(plano_importacao$cliente_origem)
      clientes_destino <- unique(plano_importacao$cliente_destino)

      if (length(clientes_origem) >= 3 && length(clientes_destino) == 1) {
        plano_importacao$status <- "bloqueado"
        plano_importacao$detalhe <- paste(
          "Importacao suspeita:",
          length(clientes_origem),
          "clientes de origem direcionados para um unico destino:",
          clientes_destino[1]
        )
        registrar_log_importacao_pdfs(plano_importacao)

        stop(
          paste0(
            "Importacao bloqueada: ",
            length(clientes_origem),
            " pastas/clientes do ZIP seriam importados para um unico cliente (",
            clientes_destino[1],
            "). Verifique aliases e estrutura do ZIP. Detalhes em logs/pdf_importacao.csv."
          )
        )
      }

      registrar_log_importacao_pdfs(plano_importacao)

      tmp <- tempfile("pdfs-")
      dir_create(tmp)
      utils::unzip(input$pdf_zip$datapath, exdir = tmp)

      total <- 0

      for (i in seq_len(nrow(pdfs_zip))) {
        item_zip <- pdfs_validos[[i]]
        cliente <- plano_importacao$cliente_destino[i]
        arquivo <- item_zip$arquivo

        origem <- do.call(file.path, as.list(c(tmp, item_zip$partes)))

        if (!file_exists(origem)) {
          partes_com_raiz <- strsplit(pdfs_zip$nome_zip[i], "/", fixed = TRUE)[[1]]
          origem <- do.call(file.path, as.list(c(tmp, partes_com_raiz)))
        }

        destino_dir <- caminho_cliente_pdf(
          input$pdf_empresa,
          competencia,
          cliente
        )

        dir_create(destino_dir, recurse = TRUE)
        file_copy(
          origem,
          file.path(destino_dir, arquivo),
          overwrite = TRUE
        )

        total <- total + 1
      }

      plano_importacao$status <- "importado"
      plano_importacao$detalhe <- paste("Destino final:", plano_importacao$cliente_destino)
      registrar_log_importacao_pdfs(plano_importacao)

      atualizar_pdfs()

      pdf_msg(
        paste0(
          total,
          " PDF(s) importado(s) do ZIP para ",
          input$pdf_empresa,
          "/clientes/",
          competencia,
          ". Origem: ",
          length(clientes_origem),
          " cliente(s)/pasta(s); destino: ",
          length(clientes_destino),
          " cliente(s). Log: logs/pdf_importacao.csv"
        )
      )
    },
    error = function(e) {
      pdf_msg(paste("Erro ao importar ZIP:", conditionMessage(e)))
    })
  })

  observeEvent(input$limpar_competencias_pdfs, {
    req(input$pdf_empresa)

    if (!isTRUE(input$pdf_confirmar_limpeza)) {
      pdf_msg("Marque a confirmação antes de remover competências antigas.")
      return()
    }

    tryCatch({
      pdf_msg("Processando limpeza de competências antigas...")

      reter <- as.integer(input$pdf_reter_meses)

      if (is.na(reter) || reter < 1) {
        stop("Informe pelo menos 1 competência para manter.")
      }

      competencias <- buscar_competencias(input$pdf_empresa)
      competencias <- competencias[grepl("^\\d{4}-\\d{2}$", competencias)]
      competencias <- sort(competencias, decreasing = TRUE)

      if (length(competencias) <= reter) {
        pdf_msg(
          paste0(
            "Nada a remover. Empresa possui ",
            length(competencias),
            " competência(s) e a regra mantém ",
            reter,
            "."
          )
        )
        return()
      }

      remover <- competencias[(reter + 1):length(competencias)]
      resultado <- c(
        paste0(
          "Mantendo: ",
          paste(competencias[seq_len(reter)], collapse = ", ")
        )
      )

      withProgress(
        message = "Compactando e removendo competências antigas...",
        value = 0,
        {
          total_remover <- length(remover)
          idx_remocao <- 0

      for (competencia in remover) {
        idx_remocao <- idx_remocao + 1

        setProgress(
          value = (idx_remocao - 1) / total_remover,
          detail = paste("Criando backup de", competencia)
        )

        zip_backup <- criar_backup_competencia_pdf(
          input$pdf_empresa,
          competencia
        )

        setProgress(
          value = (idx_remocao - 0.5) / total_remover,
          detail = paste("Removendo", competencia)
        )

        pasta_competencia <- validar_pasta_competencia_para_remocao(
          input$pdf_empresa,
          competencia
        )

        unlink(
          pasta_competencia,
          recursive = TRUE,
          force = TRUE
        )

        if (dir_exists(pasta_competencia)) {
          stop(paste("Não foi possível remover:", competencia))
        }

        resultado <- c(
          resultado,
          paste0(
            "Removida ",
            competencia,
            " com backup em ",
            zip_backup
          )
        )

        setProgress(
          value = idx_remocao / total_remover,
          detail = paste("Finalizada", competencia)
        )
      }

          setProgress(
            value = 1,
            detail = "Limpeza concluída"
          )
        }
      )

      atualizar_pdfs()
      updateCheckboxInput(session, "pdf_confirmar_limpeza", value = FALSE)

      pdf_msg(paste(resultado, collapse = "\n"))
    },
    error = function(e) {
      pdf_msg(paste("Erro ao limpar competências:", conditionMessage(e)))
    })
  })

  # =========================================================
  # DISPARO
  # =========================================================

  disparo_refresh <- reactiveVal(0)

  atualizar_disparo <- function() {
    atualizar_contador(disparo_refresh)
  }

  output$ui_competencia <- renderUI({
    disparo_refresh()
    req(input$empresa)

    competencias <- buscar_competencias(input$empresa)

    selectInput(
      "competencia",
      "Competência dos PDFs",
      choices = competencias,
      selected = if (length(competencias) > 0) competencias[1] else character(0)
    )
  })

  observeEvent(input$competencia, {
    atualizar_referencia_email(
      competencia = input$competencia,
      input_mes = "mes_email",
      input_ano = "ano_email"
    )
  }, ignoreInit = FALSE)

  dados_clientes <- reactive({
    disparo_refresh()
    req(input$empresa)
    req(input$competencia)

    dados <- ler_clientes_empresa(input$empresa)

    if (nrow(dados) == 0) {
      return(tibble::tibble())
    }

    if (isTRUE(input$somente_ativos)) {
      dados <- dados |>
        dplyr::filter(as.logical(.data$ativo) == TRUE)
    }

    verificacoes <- purrr::map(
      dados$cliente_nome,
      ~ buscar_pdfs_cliente(input$empresa, input$competencia, .x)
    )

    dados$pasta_encontrada <- purrr::map_chr(verificacoes, "pasta_encontrada")
    dados$total_pdfs <- purrr::map_int(verificacoes, "total_pdfs")
    dados$status_pdfs <- purrr::map_chr(verificacoes, "status_pdfs")

    dados
  })

  output$tabela_clientes <- DT::renderDT({
    dados_tabela <- dados_clientes()

    if ("telefone_whatsapp" %in% names(dados_tabela)) {
      dados_tabela$telefone_whatsapp <- formatar_telefone_br(
        dados_tabela$telefone_whatsapp
      )
    }

    datatable_padrao(
      dados_tabela,
      page_length = 15,
      colnames = c(
        "Cliente",
        "Email Principal",
        "Emails Cópia",
        "WhatsApp",
        "Ativo",
        "Observação",
        "Pasta Encontrada",
        "Qtd PDFs",
        "Status PDFs"
      )
    )
  })

  observeEvent(input$atualizar, {
    atualizar_disparo()
    resultado_envio("Lista atualizada.")
  })

  # =========================================================
  # MODELOS DE EMAIL
  # =========================================================

  modelos_msg <- reactiveVal("Aguardando edição.")

  carregar_modelos <- function(empresa) {
    assunto_path <- file.path(pasta_raiz, empresa, "modelos", "assunto.txt")
    corpo_path <- file.path(pasta_raiz, empresa, "modelos", "corpo_email.txt")

    assunto <- if (file.exists(assunto_path)) readr::read_file(assunto_path) else ""
    corpo <- if (file.exists(corpo_path)) readr::read_file(corpo_path) else ""

    updateTextAreaInput(session, "modelo_assunto", value = assunto)
    updateTextAreaInput(session, "modelo_corpo", value = corpo)

    modelos_msg(paste("Modelos carregados para:", empresa))
  }

  observeEvent(input$modelos_empresa, {
    req(input$modelos_empresa)
    carregar_modelos(input$modelos_empresa)
  }, ignoreInit = FALSE)

  observeEvent(input$salvar_modelos, {
    req(input$modelos_empresa)

    modelos_dir <- file.path(pasta_raiz, input$modelos_empresa, "modelos")
    dir.create(modelos_dir, recursive = TRUE, showWarnings = FALSE)

    criar_backup_seguro()
    readr::write_file(input$modelo_assunto, file.path(modelos_dir, "assunto.txt"))
    readr::write_file(input$modelo_corpo, file.path(modelos_dir, "corpo_email.txt"))

    modelos_msg("Modelos salvos com sucesso.")
  })

  output$modelos_msg <- renderText({
    modelos_msg()
  })

  observeEvent(input$visualizar_template, {
    req(input$modelos_empresa)

    empresa_config <- carregar_dados_empresa_config(input$modelos_empresa)
    nome_empresa <- empresa_config$empresa_nome

    template_path <- file.path(
      pasta_raiz,
      input$modelos_empresa,
      "modelos",
      "template_html.html"
    )

    corpo_preview <- gsub("\n", "<br>", input$modelo_corpo, fixed = TRUE)

    if (file.exists(template_path)) {
      template_html <- readr::read_file(template_path)

      preview_html <- template_html |>
        gsub("{{corpo_email}}", corpo_preview, x = _, fixed = TRUE) |>
        gsub("{{empresa_nome}}", nome_empresa, x = _, fixed = TRUE) |>
        gsub("{{empresa_whatsapp}}", empresa_config$empresa_whatsapp, x = _, fixed = TRUE) |>
        gsub("{{cliente_nome}}", "Cliente Exemplo LTDA", x = _, fixed = TRUE) |>
        gsub("{{cliente_email}}", "cliente@exemplo.com", x = _, fixed = TRUE) |>
        gsub("{{email_principal}}", "cliente@exemplo.com", x = _, fixed = TRUE) |>
        gsub("{{cliente_whatsapp}}", "(27) 99999-9999", x = _, fixed = TRUE) |>
        gsub("{{mes_referencia}}", "Abril", x = _, fixed = TRUE) |>
        gsub("{{ano_referencia}}", "2026", x = _, fixed = TRUE) |>
        gsub("{{competencia_pdfs}}", "2026-05", x = _, fixed = TRUE)
    } else {
      preview_html <- paste0(
        "<div style='font-family:Arial; padding:24px;'>",
        corpo_preview,
        "</div>"
      )
    }

    showModal(
      modalDialog(
        title = "Visualização do Template",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Fechar"),
        HTML(preview_html)
      )
    )
  })

  # =========================================================
  # REMETENTES
  # =========================================================

  remetentes_msg <- reactiveVal("Aguardando edição.")
  remetentes_refresh <- reactiveVal(0)
  smtp_opcoes_refresh <- reactiveVal(0)

  atualizar_remetentes <- function() {
    atualizar_contador(remetentes_refresh)
  }

  carregar_smtp_opcoes <- function() {
    caminho <- file.path(pasta_raiz, "_config", "smtp.csv")

    if (!file.exists(caminho)) {
      return(character(0))
    }

    dados <- readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )

    if (!"smtp_id" %in% names(dados) || nrow(dados) == 0) {
      return(character(0))
    }

    dados <- dados |>
      dplyr::filter(!is.na(.data$smtp_id), .data$smtp_id != "") |>
      dplyr::distinct(.data$smtp_id, .keep_all = TRUE)

    rotulos <- dados$smtp_id

    if ("email" %in% names(dados)) {
      rotulos <- ifelse(
        is.na(dados$email) | dados$email == "",
        dados$smtp_id,
        paste0(dados$smtp_id, " - ", dados$email)
      )
    }

    stats::setNames(dados$smtp_id, rotulos)
  }

  output$ui_rem_smtp_id <- renderUI({
    smtp_opcoes_refresh()

    selectInput(
      "rem_smtp_id",
      "SMTP ID",
      choices = carregar_smtp_opcoes(),
      selected = character(0)
    )
  })

  output$ui_rem_empresa_id <- renderUI({
    selectInput(
      "rem_empresa_id",
      "ID da Empresa",
      choices = listar_empresas(),
      selected = character(0)
    )
  })

  carregar_remetentes_dados <- function() {
    caminho <- file.path(pasta_raiz, "_config", "remetentes.csv")

    if (!file.exists(caminho)) {
      return(
        tibble::tibble(
          empresa_id = character(),
          empresa_nome = character(),
          empresa_whatsapp = character(),
          email_remetente = character(),
          nome_remetente = character(),
          smtp_id = character(),
          ativo = logical()
        )
      )
    }

    dados <- readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )

    if (!"empresa_whatsapp" %in% names(dados)) {
      dados$empresa_whatsapp <- ""
    }

    colunas_remetentes <- c(
      "empresa_id",
      "empresa_nome",
      "empresa_whatsapp",
      "email_remetente",
      "nome_remetente",
      "smtp_id",
      "ativo"
    )

    for (coluna in setdiff(colunas_remetentes, names(dados))) {
      dados[[coluna]] <- ""
    }

    dados |>
      dplyr::mutate(
        empresa_whatsapp = normalizar_telefone_br(.data$empresa_whatsapp),
        ativo = as.logical(.data$ativo)
      ) |>
      dplyr::select(dplyr::all_of(colunas_remetentes), dplyr::everything())
  }

  salvar_remetentes_dados <- function(dados) {
    caminho <- file.path(pasta_raiz, "_config", "remetentes.csv")
    dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(dados, caminho)
  }

  remetentes_dados <- reactive({
    remetentes_refresh()
    carregar_remetentes_dados()
  })

  output$remetentes_tabela <- DT::renderDT({
    datatable_padrao(
      remetentes_dados(),
      selection = "single",
      colnames = c(
        "ID Empresa",
        "Nome Empresa",
        "WhatsApp Empresa",
        "Email Remetente",
        "Nome Remetente",
        "SMTP ID",
        "Ativo"
      )
    )
  })

  observeEvent(input$remetentes_tabela_rows_selected, {
    linha <- input$remetentes_tabela_rows_selected
    req(length(linha) == 1)

    dados <- remetentes_dados()
    remetente <- dados[linha, ]

    session$userData$remetente_modo_novo <- FALSE

    updateSelectInput(session, "rem_empresa_id", selected = remetente$empresa_id)
    updateTextInput(session, "rem_empresa_nome", value = remetente$empresa_nome)
    updateTextInput(session, "rem_empresa_whatsapp", value = formatar_telefone_br(remetente$empresa_whatsapp))
    updateTextInput(session, "rem_email", value = remetente$email_remetente)
    updateTextInput(session, "rem_nome", value = remetente$nome_remetente)
    updateSelectInput(session, "rem_smtp_id", selected = remetente$smtp_id)
    updateCheckboxInput(session, "rem_ativo", value = as.logical(remetente$ativo))

    remetentes_msg(paste("Remetente carregado:", remetente$email_remetente))
  })

  observeEvent(input$novo_remetente, {
    session$userData$remetente_modo_novo <- TRUE
    opcoes_smtp <- unname(carregar_smtp_opcoes())

    empresas <- listar_empresas()

    updateSelectInput(
      session,
      "rem_empresa_id",
      selected = if (length(empresas) > 0) empresas[1] else character(0)
    )
    updateTextInput(session, "rem_empresa_nome", value = "")
    updateTextInput(session, "rem_empresa_whatsapp", value = "")
    updateTextInput(session, "rem_email", value = "")
    updateTextInput(session, "rem_nome", value = "")
    updateSelectInput(
      session,
      "rem_smtp_id",
      selected = if (length(opcoes_smtp) > 0) opcoes_smtp[1] else character(0)
    )
    updateCheckboxInput(session, "rem_ativo", value = TRUE)

    remetentes_msg("Novo remetente.")
  })

  observeEvent(input$salvar_remetente, {
    if (is.null(input$rem_empresa_id) || input$rem_empresa_id == "") {
      remetentes_msg("Selecione uma empresa antes de salvar o remetente.")
      return()
    }

    if (is.null(input$rem_smtp_id) || input$rem_smtp_id == "") {
      remetentes_msg("Cadastre um SMTP antes de salvar o remetente.")
      return()
    }

    dados <- carregar_remetentes_dados()

    novo_registro <- tibble::tibble(
      empresa_id = as.character(input$rem_empresa_id),
      empresa_nome = as.character(input$rem_empresa_nome),
      empresa_whatsapp = normalizar_telefone_br(input$rem_empresa_whatsapp),
      email_remetente = as.character(input$rem_email),
      nome_remetente = as.character(input$rem_nome),
      smtp_id = as.character(input$rem_smtp_id),
      ativo = as.logical(input$rem_ativo)
    )

    linha <- input$remetentes_tabela_rows_selected

    if (isTRUE(session$userData$remetente_modo_novo) || length(linha) == 0) {
      dados <- dplyr::bind_rows(dados, novo_registro)
    } else {
      dados[linha, ] <- novo_registro
    }

    criar_backup_seguro()
    salvar_remetentes_dados(dados)
    atualizar_remetentes()

    session$userData$remetente_modo_novo <- FALSE
    remetentes_msg("Remetente salvo com sucesso.")
  })

  observeEvent(input$excluir_remetente, {
    linha <- input$remetentes_tabela_rows_selected

    if (length(linha) == 0) {
      remetentes_msg("Selecione um remetente antes de excluir.")
      return()
    }

    dados <- carregar_remetentes_dados()
    email <- dados$email_remetente[linha]
    dados <- dados[-linha, ]

    criar_backup_seguro()
    salvar_remetentes_dados(dados)
    atualizar_remetentes()

    session$userData$remetente_modo_novo <- FALSE
    remetentes_msg(paste("Remetente excluído:", email))
  })

  observeEvent(input$importar_remetentes, {
    req(input$importar_remetentes)

    importado <- readr::read_csv(
      input$importar_remetentes$datapath,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    ) |>
      dplyr::mutate(
        ativo = as.logical(.data$ativo)
      )

    dados_atuais <- carregar_remetentes_dados()

    if (isTRUE(input$substituir_remetentes)) {
      dados_finais <- importado
    } else {
      dados_finais <- dplyr::bind_rows(dados_atuais, importado) |>
        dplyr::distinct(empresa_id, email_remetente, smtp_id, .keep_all = TRUE)
    }

    criar_backup_seguro()
    salvar_remetentes_dados(dados_finais)
    atualizar_remetentes()

    remetentes_msg("Importação de remetentes concluída.")
  })

  output$remetentes_msg <- renderText({
    remetentes_msg()
  })

  # =========================================================
  # SMTP
  # =========================================================

  smtp_msg <- reactiveVal("Aguardando edição.")
  smtp_refresh <- reactiveVal(0)

  atualizar_smtp <- function() {
    atualizar_contador(smtp_refresh)
    atualizar_contador(smtp_opcoes_refresh)
  }

  carregar_smtp_dados <- function() {
    caminho <- file.path(pasta_raiz, "_config", "smtp.csv")

    if (!file.exists(caminho)) {
      return(
        tibble::tibble(
          smtp_id = character(),
          email = character(),
          host = character(),
          port = character(),
          use_ssl = character(),
          usuario = character(),
          observacao = character(),
          senha = character()
        )
      )
    }

    readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )
  }

  salvar_smtp_dados <- function(dados) {
    caminho <- file.path(pasta_raiz, "_config", "smtp.csv")
    dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(dados, caminho)
  }

  smtp_dados <- reactive({
    smtp_refresh()
    carregar_smtp_dados()
  })

  output$smtp_tabela <- DT::renderDT({
    dados_visiveis <- smtp_dados()

    if (nrow(dados_visiveis) > 0 && "senha" %in% names(dados_visiveis)) {
      dados_visiveis$senha <- ifelse(
        is.na(dados_visiveis$senha) | dados_visiveis$senha == "",
        "",
        "********"
      )
    }

    datatable_padrao(
      dados_visiveis,
      selection = "single",
      colnames = c(
        "SMTP ID",
        "Email",
        "Host",
        "Porta",
        "SSL",
        "Usuário",
        "Observação",
        "Senha"
      )
    )
  })

  observeEvent(input$smtp_tabela_rows_selected, {
    linha <- input$smtp_tabela_rows_selected
    req(length(linha) == 1)

    dados <- carregar_smtp_dados()
    smtp <- dados[linha, ]

    session$userData$smtp_modo_novo <- FALSE

    updateTextInput(session, "smtp_id_form", value = smtp$smtp_id)
    updateTextInput(session, "smtp_email_form", value = smtp$email)
    updateTextInput(session, "smtp_host_form", value = smtp$host)
    updateTextInput(session, "smtp_port_form", value = smtp$port)
    updateCheckboxInput(session, "smtp_ssl_form", value = as.logical(smtp$use_ssl))
    updateTextInput(session, "smtp_usuario_form", value = smtp$usuario)
    updateTextInput(session, "smtp_observacao_form", value = smtp$observacao)
    updateTextInput(session, "smtp_senha_form", value = smtp$senha)

    smtp_msg(paste("SMTP carregado:", smtp$smtp_id))
  })

  observeEvent(input$novo_smtp, {
    session$userData$smtp_modo_novo <- TRUE

    updateTextInput(session, "smtp_id_form", value = "")
    updateTextInput(session, "smtp_email_form", value = "")
    updateTextInput(session, "smtp_host_form", value = "")
    updateTextInput(session, "smtp_port_form", value = "465")
    updateCheckboxInput(session, "smtp_ssl_form", value = TRUE)
    updateTextInput(session, "smtp_usuario_form", value = "")
    updateTextInput(session, "smtp_observacao_form", value = "")
    updateTextInput(session, "smtp_senha_form", value = "")

    smtp_msg("Novo SMTP.")
  })

  observeEvent(input$salvar_smtp, {
    dados <- carregar_smtp_dados()

    novo_registro <- tibble::tibble(
      smtp_id = as.character(input$smtp_id_form),
      email = as.character(input$smtp_email_form),
      host = as.character(input$smtp_host_form),
      port = as.character(input$smtp_port_form),
      use_ssl = as.character(input$smtp_ssl_form),
      usuario = as.character(input$smtp_usuario_form),
      observacao = as.character(input$smtp_observacao_form),
      senha = as.character(input$smtp_senha_form)
    )

    linha <- input$smtp_tabela_rows_selected

    if (isTRUE(session$userData$smtp_modo_novo) || length(linha) == 0) {
      dados <- dplyr::bind_rows(dados, novo_registro)
    } else {
      dados[linha, ] <- novo_registro
    }

    criar_backup_seguro()
    salvar_smtp_dados(dados)
    atualizar_smtp()

    session$userData$smtp_modo_novo <- FALSE
    smtp_msg("SMTP salvo com sucesso.")
  })

  observeEvent(input$excluir_smtp, {
    linha <- input$smtp_tabela_rows_selected

    if (length(linha) == 0) {
      smtp_msg("Selecione um SMTP antes de excluir.")
      return()
    }

    dados <- carregar_smtp_dados()
    smtp_id <- dados$smtp_id[linha]
    dados <- dados[-linha, ]

    criar_backup_seguro()
    salvar_smtp_dados(dados)
    atualizar_smtp()

    session$userData$smtp_modo_novo <- FALSE
    smtp_msg(paste("SMTP excluído:", smtp_id))
  })

  observeEvent(input$importar_smtp, {
    req(input$importar_smtp)

    importado <- readr::read_csv(
      input$importar_smtp$datapath,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )

    dados_atuais <- carregar_smtp_dados()

    if (isTRUE(input$substituir_smtp)) {
      dados_finais <- importado
    } else {
      dados_finais <- dplyr::bind_rows(dados_atuais, importado) |>
        dplyr::distinct(smtp_id, .keep_all = TRUE)
    }

    criar_backup_seguro()
    salvar_smtp_dados(dados_finais)
    atualizar_smtp()

    smtp_msg("Importação de SMTP concluída.")
  })

  observeEvent(input$testar_smtp, {
    req(input$smtp_email_teste)

    tryCatch({
      smtp_temp <- tibble::tibble(
        smtp_id = input$smtp_id_form,
        email = input$smtp_email_form,
        host = input$smtp_host_form,
        port = input$smtp_port_form,
        use_ssl = as.character(input$smtp_ssl_form),
        usuario = input$smtp_usuario_form,
        senha = input$smtp_senha_form
      )

      smtp_senha_env <- paste0("SMTP_TESTE_", toupper(gsub("[^A-Za-z0-9]", "_", input$smtp_id_form)))
      smtp_senha_env <- paste0(
        "SMTP_TESTE_",
        toupper(gsub("[^A-Za-z0-9]", "_", input$smtp_id_form))
      )

      do.call(
        Sys.setenv,
        stats::setNames(
          as.list(as.character(input$smtp_senha_form)),
          smtp_senha_env
        )
      )

      credenciais <- blastula::creds_envvar(
        user = as.character(smtp_temp$usuario),
        pass_envvar = smtp_senha_env,
        host = as.character(smtp_temp$host),
        port = as.numeric(smtp_temp$port),
        use_ssl = as.logical(smtp_temp$use_ssl)
      )

      email_teste <- blastula::compose_email(
        body = blastula::md("
# Teste de SMTP

Se você recebeu esta mensagem, a configuração SMTP está funcionando corretamente.
")
      )

      blastula::smtp_send(
        email = email_teste,
        from = as.character(smtp_temp$usuario),
        to = trimws(input$smtp_email_teste),
        subject = "Teste de SMTP - Disparo de Mensagens",
        credentials = credenciais
      )

      smtp_msg("Teste enviado com sucesso.")
    },
    error = function(e) {
      smtp_msg(
        paste("Erro no teste SMTP:", conditionMessage(e))
      )
    })
  })

  output$smtp_msg <- renderText({
    smtp_msg()
  })

  # =========================================================
  # CHATWOOT
  # =========================================================

  cw_msg <- reactiveVal("Aguardando configuração.")
  cw_refresh <- reactiveVal(0)

  cw_modo_novo <- reactiveVal(FALSE)

  atualizar_cw <- function() {
    atualizar_contador(cw_refresh)
  }

  output$ui_cw_empresa_id <- renderUI({
    selectInput(
      "cw_empresa_id",
      "Empresa ID",
      choices = listar_empresas(),
      selected = character(0)
    )
  })

  caminho_chatwoot <- function() {
    file.path(pasta_raiz, "_config", "chatwoot.csv")
  }

  mensagem_whatsapp_email_enviado_padrao <- function() {
    paste(
      "Olá, {{cliente_nome}}.",
      "{{empresa_nome}} enviou por e-mail os documentos referentes à competência {{competencia_pdfs}}.",
      "Qualquer dúvida, estamos à disposição."
    )
  }

  mensagem_whatsapp_email_falha_padrao <- function() {
    paste(
      "Olá, {{cliente_nome}}.",
      "{{empresa_nome}} está entrando em contato sobre os documentos referentes à competência {{competencia_pdfs}}.",
      "Qualquer dúvida, estamos à disposição."
    )
  }

  normalizar_colunas_chatwoot <- function(dados) {
    colunas_texto <- c(
      "chatwoot_id",
      "empresa_id",
      "base_url",
      "account_id",
      "inbox_identifier",
      "inbox_id",
      "metodo_envio",
      "api_access_token",
      "observacao"
    )

    for (coluna in colunas_texto) {
      if (!coluna %in% names(dados)) {
        dados[[coluna]] <- character(nrow(dados))
      }
    }

    if (!"ativo" %in% names(dados)) {
      dados$ativo <- TRUE
    }

    if (!"enviar_pdfs_whatsapp" %in% names(dados)) {
      dados$enviar_pdfs_whatsapp <- FALSE
    }

    if (!"mensagem_email_enviado" %in% names(dados)) {
      dados$mensagem_email_enviado <- mensagem_whatsapp_email_enviado_padrao()
    }

    if (!"mensagem_email_falha" %in% names(dados)) {
      dados$mensagem_email_falha <- mensagem_whatsapp_email_falha_padrao()
    }

    dados |>
      dplyr::mutate(
        metodo_envio = dplyr::if_else(
          is.na(.data$metodo_envio) | .data$metodo_envio == "",
          "public_api",
          .data$metodo_envio
        )
      ) |>
      dplyr::mutate(
        ativo = as.logical(.data$ativo),
        enviar_pdfs_whatsapp = as.logical(.data$enviar_pdfs_whatsapp),
        mensagem_email_enviado = dplyr::if_else(
          is.na(.data$mensagem_email_enviado) | .data$mensagem_email_enviado == "",
          mensagem_whatsapp_email_enviado_padrao(),
          .data$mensagem_email_enviado
        ),
        mensagem_email_falha = dplyr::if_else(
          is.na(.data$mensagem_email_falha) | .data$mensagem_email_falha == "",
          mensagem_whatsapp_email_falha_padrao(),
          .data$mensagem_email_falha
        )
      ) |>
      dplyr::select(
        chatwoot_id,
        empresa_id,
        base_url,
        account_id,
        inbox_identifier,
        inbox_id,
        metodo_envio,
        api_access_token,
        ativo,
        enviar_pdfs_whatsapp,
        mensagem_email_enviado,
        mensagem_email_falha,
        observacao
      )
  }

  carregar_chatwoot <- function() {
    caminho <- caminho_chatwoot()

    if (!file.exists(caminho)) {
      return(normalizar_colunas_chatwoot(
        tibble::tibble(
          chatwoot_id = character(),
          empresa_id = character(),
          base_url = character(),
          account_id = character(),
          inbox_identifier = character(),
          inbox_id = character(),
          metodo_envio = character(),
          api_access_token = character(),
          ativo = logical(),
          enviar_pdfs_whatsapp = logical(),
          mensagem_email_enviado = character(),
          mensagem_email_falha = character(),
          observacao = character()
        )
      ))
    }

    dados <- readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )

    normalizar_colunas_chatwoot(dados)
  }

  salvar_chatwoot <- function(dados) {
    caminho <- caminho_chatwoot()
    dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(dados, caminho)
  }

  output$cw_tabela <- DT::renderDT({
    cw_refresh()

    dados <- carregar_chatwoot()

    if (nrow(dados) > 0 && "api_access_token" %in% names(dados)) {
      dados$api_access_token <- ifelse(
        is.na(dados$api_access_token) | dados$api_access_token == "",
        "",
        "********"
      )
    }

    datatable_padrao(
      dados,
      selection = "single",
      colnames = c(
        "Chatwoot ID",
        "Empresa ID",
        "URL Base",
        "Account ID",
        "Inbox Identifier",
        "Inbox ID",
        "Metodo Envio",
        "Token",
        "Ativo",
        "Enviar PDFs",
        "Mensagem Email Enviado",
        "Mensagem Email Falha",
        "Observação"
      )
    )
  })

  observeEvent(input$cw_tabela_rows_selected, {
    linha <- input$cw_tabela_rows_selected
    req(length(linha) == 1)

    dados <- carregar_chatwoot()
    cw_modo_novo(FALSE)
    item <- dados[linha, ]

    updateTextInput(session, "cw_id", value = item$chatwoot_id)
    updateSelectInput(session, "cw_empresa_id", selected = item$empresa_id)
    updateTextInput(session, "cw_base_url", value = item$base_url)
    updateTextInput(session, "cw_account_id", value = item$account_id)
    updateTextInput(session, "cw_inbox_identifier", value = item$inbox_identifier)
    updateTextInput(session, "cw_inbox_id", value = item$inbox_id)
    updateSelectInput(session, "cw_metodo_envio", selected = item$metodo_envio)
    updateTextInput(session, "cw_token", value = item$api_access_token)
    updateCheckboxInput(session, "cw_ativo", value = as.logical(item$ativo))
    updateCheckboxInput(session, "cw_enviar_pdfs", value = as.logical(item$enviar_pdfs_whatsapp))
    updateTextAreaInput(session, "cw_msg_email_enviado", value = item$mensagem_email_enviado)
    updateTextAreaInput(session, "cw_msg_email_falha", value = item$mensagem_email_falha)
    updateTextAreaInput(session, "cw_observacao", value = item$observacao)

    cw_msg(paste("Configuração carregada:", item$chatwoot_id))
  })

  observeEvent(input$cw_novo, {
    cw_modo_novo(TRUE)

    updateTextInput(session, "cw_id", value = "")
    empresas <- listar_empresas()

    updateSelectInput(
      session,
      "cw_empresa_id",
      selected = if (length(empresas) > 0) empresas[1] else character(0)
    )
    updateTextInput(session, "cw_base_url", value = "")
    updateTextInput(session, "cw_account_id", value = "")
    updateTextInput(session, "cw_inbox_identifier", value = "")
    updateTextInput(session, "cw_inbox_id", value = "")
    updateSelectInput(session, "cw_metodo_envio", selected = "public_api")
    updateTextInput(session, "cw_token", value = "")
    updateCheckboxInput(session, "cw_ativo", value = TRUE)
    updateCheckboxInput(session, "cw_enviar_pdfs", value = FALSE)
    updateTextAreaInput(session, "cw_msg_email_enviado", value = mensagem_whatsapp_email_enviado_padrao())
    updateTextAreaInput(session, "cw_msg_email_falha", value = mensagem_whatsapp_email_falha_padrao())
    updateTextAreaInput(session, "cw_observacao", value = "")

    cw_msg("Nova configuração.")
  })

  observeEvent(input$cw_salvar, {
    if (is.null(input$cw_empresa_id) || input$cw_empresa_id == "") {
      cw_msg("Selecione uma empresa antes de salvar a configuraÃ§Ã£o Chatwoot.")
      return()
    }

    dados <- carregar_chatwoot()

    novo_registro <- tibble::tibble(
      chatwoot_id = as.character(input$cw_id),
      empresa_id = as.character(input$cw_empresa_id),
      base_url = as.character(input$cw_base_url),
      account_id = as.character(input$cw_account_id),
      inbox_identifier = as.character(input$cw_inbox_identifier),
      inbox_id = as.character(input$cw_inbox_id),
      metodo_envio = as.character(input$cw_metodo_envio),
      api_access_token = as.character(input$cw_token),
      ativo = as.logical(input$cw_ativo),
      enviar_pdfs_whatsapp = as.logical(input$cw_enviar_pdfs),
      mensagem_email_enviado = as.character(input$cw_msg_email_enviado),
      mensagem_email_falha = as.character(input$cw_msg_email_falha),
      observacao = as.character(input$cw_observacao)
    )

    linha <- input$cw_tabela_rows_selected

    if (isTRUE(cw_modo_novo()) || length(linha) == 0) {
      dados <- dplyr::bind_rows(dados, novo_registro)
    } else {
      dados[linha, ] <- novo_registro
    }

    criar_backup_seguro()
    salvar_chatwoot(dados)
    atualizar_cw()

    cw_modo_novo(FALSE)
    cw_msg("Configuração Chatwoot salva com sucesso.")
  })

  observeEvent(input$cw_excluir, {
    linha <- input$cw_tabela_rows_selected

    if (length(linha) == 0) {
      cw_msg("Selecione uma configuração antes de excluir.")
      return()
    }

    dados <- carregar_chatwoot()
    id <- dados$chatwoot_id[linha]

    dados <- dados[-linha, ]

    criar_backup_seguro()
    salvar_chatwoot(dados)
    atualizar_cw()

    cw_modo_novo(FALSE)
    cw_msg(paste("Configuração excluída:", id))
  })

  observeEvent(input$cw_testar_envio, {
    req(input$cw_base_url)
    req(input$cw_inbox_identifier)
    req(input$cw_token)
    req(input$cw_teste_telefone)

    tryCatch({

      telefone <- normalizar_telefone_br(input$cw_teste_telefone)
      telefone_chatwoot <- paste0("+55", telefone)
      contact_identifier_esperado <- chatwoot_contact_identifier(
        input$cw_empresa_id,
        telefone_chatwoot
      )
      base_url <- sub("/+$", "", input$cw_base_url)

      if (nchar(telefone) < 10) {
        stop("Telefone inválido. Informe DDD + número.")
      }

      if (identical(input$cw_metodo_envio, "account_api")) {
        account_id <- trimws(as.character(input$cw_account_id))
        inbox_id <- trimws(as.character(input$cw_inbox_id))

        if (inbox_id == "" && grepl("^[0-9]+$", trimws(as.character(input$cw_inbox_identifier)))) {
          inbox_id <- trimws(as.character(input$cw_inbox_identifier))
        }

        if (account_id == "" || inbox_id == "" || !grepl("^[0-9]+$", inbox_id)) {
          stop("Account ID e Inbox ID numerico sao obrigatorios para testar Account API.")
        }

        contato <- chatwoot_buscar_ou_criar_contato_account(
          base_url = base_url,
          account_id = account_id,
          inbox_id = as.integer(inbox_id),
          token = input$cw_token,
          cliente_nome = input$cw_teste_nome,
          telefone_normalizado = telefone_chatwoot,
          identifier = contact_identifier_esperado,
          timeout = chatwoot_timeout_segundos()
        )

        source_id <- chatwoot_contact_source_id(
          contato = contato,
          inbox_id = as.integer(inbox_id),
          fallback = contact_identifier_esperado
        )

        conversation_id <- chatwoot_obter_conversa_account(
          base_url = base_url,
          account_id = account_id,
          inbox_id = as.integer(inbox_id),
          token = input$cw_token,
          contato = contato,
          source_id = source_id,
          timeout = chatwoot_timeout_segundos(),
          empresa = input$cw_empresa_id,
          competencia = "",
          origem = "teste_chatwoot"
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
            "api_access_token" = input$cw_token
          ) |>
          httr2::req_body_json(
            list(
              content = input$cw_teste_msg,
              message_type = "outgoing",
              private = FALSE,
              content_type = "text",
              content_attributes = list()
            )
          ) |>
          httr2::req_timeout(chatwoot_timeout_segundos()) |>
          chatwoot_perform("teste_account_mensagem")

        detalhe_envio <- chatwoot_resumo_resposta(
          mensagem_resp,
          metodo = "account_api",
          conversation_id = conversation_id
        )

        registrar_log_whatsapp(
          empresa = input$cw_empresa_id,
          cliente_nome = input$cw_teste_nome,
          telefone = input$cw_teste_telefone,
          mensagem = input$cw_teste_msg,
          status = "enviado",
          origem = "teste_chatwoot_account_api",
          detalhe = detalhe_envio
        )

        cw_msg(paste("Mensagem de teste enviada pelo Chatwoot via Account API.", detalhe_envio))
        return()
      }

      contato_url <- paste0(
        base_url,
        "/public/api/v1/inboxes/",
        input$cw_inbox_identifier,
        "/contacts"
      )

      contato_resp <- httr2::request(contato_url) |>
        httr2::req_headers(
          "Content-Type" = "application/json",
          "api_access_token" = input$cw_token
        ) |>
        httr2::req_body_json(
          list(
            identifier = contact_identifier_esperado,
            name = input$cw_teste_nome,
            phone_number = telefone_chatwoot
          )
        ) |>
        chatwoot_perform("teste_contato")

      contato_json <- httr2::resp_body_json(contato_resp)

      contact_identifier <- contato_json$source_id

      if (is.null(contact_identifier) || contact_identifier == "") {
        contact_identifier <- contact_identifier_esperado
      }

      conversa_url <- paste0(
        base_url,
        "/public/api/v1/inboxes/",
        input$cw_inbox_identifier,
        "/contacts/",
        contact_identifier,
        "/conversations"
      )

      conversa_resp <- httr2::request(conversa_url) |>
        httr2::req_headers(
          "Content-Type" = "application/json",
          "api_access_token" = input$cw_token
        ) |>
        httr2::req_body_json(
          list(
            custom_attributes = list()
          )
        ) |>
        chatwoot_perform("teste_conversa")

      conversa_json <- httr2::resp_body_json(conversa_resp)

      conversation_id <- conversa_json$id

      mensagem_url <- paste0(
        base_url,
        "/public/api/v1/inboxes/",
        input$cw_inbox_identifier,
        "/contacts/",
        contact_identifier,
        "/conversations/",
        conversation_id,
        "/messages"
      )

      mensagem_resp <- httr2::request(mensagem_url) |>
        httr2::req_headers(
          "Content-Type" = "application/json",
          "api_access_token" = input$cw_token
        ) |>
        httr2::req_body_json(
          list(
            content = input$cw_teste_msg
          )
        ) |>
        chatwoot_perform("teste_mensagem")

      detalhe_envio <- chatwoot_resumo_resposta(
        mensagem_resp,
        metodo = "public_api",
        conversation_id = conversation_id
      )

      registrar_log_whatsapp(
        empresa = input$cw_empresa_id,
        cliente_nome = input$cw_teste_nome,
        telefone = input$cw_teste_telefone,
        mensagem = input$cw_teste_msg,
        status = "enviado",
        origem = "teste_chatwoot",
        detalhe = detalhe_envio
      )

      cw_msg(paste("Mensagem de teste enviada pelo Chatwoot.", detalhe_envio))
    },
    error = function(e) {
      registrar_log_whatsapp(
        empresa = input$cw_empresa_id,
        cliente_nome = input$cw_teste_nome,
        telefone = input$cw_teste_telefone,
        mensagem = input$cw_teste_msg,
        status = "erro",
        erro = conditionMessage(e),
        origem = "teste_chatwoot"
      )

      cw_msg(
        paste("Erro Chatwoot:", conditionMessage(e))
      )
    })
  })

  output$cw_msg <- renderText({
    cw_msg()
  })

  # =========================================================
  # LOGS
  # =========================================================

  logs_msg <- reactiveVal("Aguardando consulta.")

  caminho_log_processamento <- function() {
    file.path(pasta_raiz, "logs", "processamento.csv")
  }

  registrar_log_processamento <- function(
      origem,
      etapa,
      empresa = "",
      competencia = "",
      cliente_nome = "",
      posicao = "",
      total = "",
      detalhe = "",
      erro = ""
  ) {
    tryCatch({
      caminho <- caminho_log_processamento()
      dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)

      novo_log <- tibble::tibble(
        data_hora = as.character(Sys.time()),
        origem = as.character(origem),
        etapa = as.character(etapa),
        empresa = as.character(empresa),
        competencia = as.character(competencia),
        cliente_nome = as.character(cliente_nome),
        posicao = as.character(posicao),
        total = as.character(total),
        detalhe = as.character(detalhe),
        erro = as.character(erro)
      )

      logs <- if (file.exists(caminho)) {
        readr::read_csv(
          caminho,
          show_col_types = FALSE,
          col_types = readr::cols(.default = "c")
        )
      } else {
        tibble::tibble()
      }

      readr::write_csv(dplyr::bind_rows(logs, novo_log), caminho)
    }, error = function(e) {
      message("Log de processamento nao registrado: ", conditionMessage(e))
    })
  }

  registrar_log_envio <- function(
      empresa,
      competencia,
      cliente,
      mes_email,
      ano_email,
      status,
      erro = ""
  ) {
    tryCatch({
      caminho <- file.path(pasta_raiz, "logs", "envios.csv")
      dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)

      total_pdfs <- if ("total_pdfs" %in% names(cliente)) {
        as.integer(cliente$total_pdfs[1])
      } else {
        verificacao <- buscar_pdfs_cliente(
          empresa,
          competencia,
          cliente$cliente_nome[1]
        )
        as.integer(verificacao$total_pdfs)
      }

      assunto <- tryCatch({
        texto <- readr::read_file(
          file.path(pasta_raiz, empresa, "modelos", "assunto.txt")
        )
        empresa_config <- carregar_dados_empresa_config(empresa)
        cliente_whatsapp <- if ("telefone_whatsapp" %in% names(cliente)) {
          formatar_telefone_br(cliente$telefone_whatsapp[1])
        } else {
          ""
        }

        texto |>
          gsub("{{cliente_nome}}", as.character(cliente$cliente_nome[1]), x = _, fixed = TRUE) |>
          gsub("{{cliente_email}}", as.character(cliente$email_principal[1]), x = _, fixed = TRUE) |>
          gsub("{{email_principal}}", as.character(cliente$email_principal[1]), x = _, fixed = TRUE) |>
          gsub("{{cliente_whatsapp}}", cliente_whatsapp, x = _, fixed = TRUE) |>
          gsub("{{empresa_whatsapp}}", empresa_config$empresa_whatsapp, x = _, fixed = TRUE) |>
          gsub("{{mes_atual}}", mes_email, x = _, fixed = TRUE) |>
          gsub("{{ano_atual}}", as.character(ano_email), x = _, fixed = TRUE) |>
          gsub("{{mes_referencia}}", mes_email, x = _, fixed = TRUE) |>
          gsub("{{ano_referencia}}", as.character(ano_email), x = _, fixed = TRUE) |>
          gsub("{{competencia_pdfs}}", competencia, x = _, fixed = TRUE)
      }, error = function(e) "")

      novo_log <- tibble::tibble(
        data_hora = as.character(Sys.time()),
        empresa = as.character(empresa),
        competencia_pdfs = as.character(competencia),
        cliente_nome = as.character(cliente$cliente_nome[1]),
        email_principal = as.character(cliente$email_principal[1]),
        email_copias = if ("email_copias" %in% names(cliente)) {
          as.character(cliente$email_copias[1])
        } else {
          ""
        },
        assunto = as.character(assunto),
        total_pdfs = as.character(total_pdfs),
        status = as.character(status),
        erro = as.character(erro)
      )

      logs <- if (file.exists(caminho)) {
        readr::read_csv(
          caminho,
          show_col_types = FALSE,
          col_types = readr::cols(.default = "c")
        )
      } else {
        tibble::tibble()
      }

      readr::write_csv(dplyr::bind_rows(logs, novo_log), caminho)
      registrar_log_processamento(
        origem = "log_envio",
        etapa = "log_envio_ok",
        empresa = empresa,
        competencia = competencia,
        cliente_nome = cliente$cliente_nome[1],
        detalhe = paste("Status:", status)
      )
    }, error = function(e) {
      registrar_log_processamento(
        origem = "log_envio",
        etapa = "log_envio_erro",
        empresa = empresa,
        competencia = competencia,
        cliente_nome = if ("cliente_nome" %in% names(cliente)) cliente$cliente_nome[1] else "",
        erro = conditionMessage(e)
      )
      message("Log de envio nao registrado: ", conditionMessage(e))
    })
  }

  carregar_logs_dados <- function() {
    caminho <- file.path(pasta_raiz, "logs", "envios.csv")

    if (!file.exists(caminho)) {
      return(
        tibble::tibble(
          data_hora = character(),
          empresa = character(),
          competencia_pdfs = character(),
          cliente_nome = character(),
          email_principal = character(),
          email_copias = character(),
          assunto = character(),
          total_pdfs = numeric(),
          status = character(),
          erro = character()
        )
      )
    }

    readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )
  }

  limpar_arquivos_logs <- function() {
    arquivos <- c(
      file.path(pasta_raiz, "logs", "envios.csv"),
      caminho_log_whatsapp(),
      caminho_log_processamento()
    )

    criar_backup_seguro()

    removidos <- arquivos[file.exists(arquivos)]

    if (length(removidos) > 0) {
      file.remove(removidos)
    }

    length(removidos)
  }

  texto_contem_filtro <- function(dados, filtro) {
    if (is.null(filtro) || length(filtro) == 0) {
      return(rep(TRUE, nrow(dados)))
    }

    filtro <- trimws(as.character(filtro))

    if (is.na(filtro) || filtro == "") {
      return(rep(TRUE, nrow(dados)))
    }

    texto <- apply(
      as.data.frame(dados, stringsAsFactors = FALSE),
      1,
      function(linha) paste(as.character(linha), collapse = " ")
    )

    grepl(filtro, texto, ignore.case = TRUE, fixed = TRUE)
  }

  manter_ultimo_log_por_cliente <- function(dados, coluna_competencia = "competencia") {
    if (
      !isTRUE(input$logs_ultimo_resultado) ||
        nrow(dados) == 0 ||
        !"data_hora" %in% names(dados) ||
        !"cliente_nome" %in% names(dados)
    ) {
      return(dados)
    }

    grupos <- intersect(c("empresa", coluna_competencia, "cliente_nome"), names(dados))

    if (length(grupos) == 0) {
      return(dados)
    }

    dados |>
      dplyr::arrange(dplyr::desc(.data$data_hora)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grupos))) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
  }

  filtro_tipo_erro <- function(dados) {
    tipo <- if (is.null(input$logs_tipo_erro)) "Todos" else input$logs_tipo_erro

    if (tipo == "Todos" || nrow(dados) == 0) {
      return(rep(TRUE, nrow(dados)))
    }

    texto <- apply(
      as.data.frame(dados, stringsAsFactors = FALSE),
      1,
      function(linha) paste(as.character(linha), collapse = " ")
    )

    padrao <- switch(
      tipo,
      "RCPT 550" = "RCPT failed: 550",
      "Login denied" = "Login denied",
      "Timeout" = "timeout",
      "Falha conexao" = "Failure when receiving data",
      "Chatwoot" = "Chatwoot",
      "Sem email" = "sem e-mail",
      "Sem WhatsApp" = "nao_enviado",
      ""
    )

    if (padrao == "") {
      return(rep(TRUE, nrow(dados)))
    }

    grepl(padrao, texto, ignore.case = TRUE, fixed = TRUE)
  }

  aplicar_filtros_logs <- function(dados, coluna_competencia = "competencia", coluna_status = "status") {
    if (nrow(dados) == 0) {
      return(dados)
    }

    if (!is.null(input$logs_empresa) && input$logs_empresa != "Todas" && "empresa" %in% names(dados)) {
      dados <- dados |>
        dplyr::filter(.data$empresa == input$logs_empresa)
    }

    if (
      coluna_competencia %in% names(dados) &&
        !is.null(input$logs_competencia) &&
        trimws(input$logs_competencia) != ""
    ) {
      competencia_filtro <- trimws(input$logs_competencia)
      dados <- dados |>
        dplyr::filter(.data[[coluna_competencia]] == competencia_filtro)
    }

    if ("cliente_nome" %in% names(dados)) {
      dados <- dados[texto_contem_filtro(dados["cliente_nome"], input$logs_cliente), , drop = FALSE]
    }

    if (
      coluna_status %in% names(dados) &&
        !is.null(input$logs_status) &&
        length(input$logs_status) > 0
    ) {
      if (identical(coluna_status, "etapa")) {
        padroes_status <- paste(input$logs_status, collapse = "|")
        dados <- dados |>
          dplyr::filter(grepl(padroes_status, .data[[coluna_status]], ignore.case = TRUE))
      } else {
        dados <- dados |>
          dplyr::filter(.data[[coluna_status]] %in% input$logs_status)
      }
    }

    if (!is.null(input$logs_origem) && input$logs_origem != "Todas" && "origem" %in% names(dados)) {
      dados <- dados |>
        dplyr::filter(.data$origem == input$logs_origem)
    }

    dados <- dados[filtro_tipo_erro(dados), , drop = FALSE]

    if (!identical(coluna_status, "etapa")) {
      dados <- manter_ultimo_log_por_cliente(dados, coluna_competencia = coluna_competencia)
    }

    if (isTRUE(input$logs_apenas_problemas)) {
      if (coluna_status %in% names(dados)) {
        dados <- dados |>
          dplyr::filter(.data[[coluna_status]] != "enviado")
      } else if ("etapa" %in% names(dados)) {
        dados <- dados |>
          dplyr::filter(grepl("erro|nao_enviado|timeout|falha", .data$etapa, ignore.case = TRUE))
      }
    }

    dados
  }

  logs_filtrados <- reactive({
    input$atualizar_logs
    dados <- carregar_logs_dados()

    aplicar_filtros_logs(dados, coluna_competencia = "competencia_pdfs")
  })

  carregar_logs_whatsapp_dados <- function() {
    caminho <- caminho_log_whatsapp()

    if (!file.exists(caminho)) {
      return(
        tibble::tibble(
          data_hora = character(),
          empresa = character(),
          cliente_nome = character(),
          telefone = character(),
          mensagem = character(),
          status = character(),
          erro = character(),
          origem = character(),
          competencia = character(),
          detalhe = character()
        )
      )
    }

    readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )
  }

  whatsapp_logs_filtrados <- reactive({
    input$atualizar_logs
    dados <- carregar_logs_whatsapp_dados()

    aplicar_filtros_logs(dados, coluna_competencia = "competencia")
  })

  carregar_logs_processamento_dados <- function() {
    caminho <- caminho_log_processamento()

    if (!file.exists(caminho)) {
      return(
        tibble::tibble(
          data_hora = character(),
          origem = character(),
          etapa = character(),
          empresa = character(),
          competencia = character(),
          cliente_nome = character(),
          posicao = character(),
          total = character(),
          detalhe = character(),
          erro = character()
        )
      )
    }

    readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )
  }

  processamento_logs_filtrados <- reactive({
    input$atualizar_logs
    dados <- carregar_logs_processamento_dados()

    aplicar_filtros_logs(dados, coluna_competencia = "competencia", coluna_status = "etapa")
  })

  logs_resumo_clientes <- reactive({
    input$atualizar_logs

    emails <- carregar_logs_dados()
    whatsapps <- carregar_logs_whatsapp_dados()

    emails <- manter_ultimo_log_por_cliente(emails, coluna_competencia = "competencia_pdfs")
    whatsapps <- manter_ultimo_log_por_cliente(whatsapps, coluna_competencia = "competencia")

    clientes_email <- emails |>
      dplyr::select(
        empresa,
        competencia = competencia_pdfs,
        cliente_nome,
        email_principal,
        email_status = status,
        email_erro = erro
      )

    clientes_whatsapp <- whatsapps |>
      dplyr::select(
        empresa,
        competencia,
        cliente_nome,
        telefone,
        whatsapp_status = status,
        whatsapp_erro = erro
      )

    chaves <- dplyr::bind_rows(
      clientes_email |> dplyr::select(empresa, competencia, cliente_nome),
      clientes_whatsapp |> dplyr::select(empresa, competencia, cliente_nome)
    ) |>
      dplyr::filter(.data$cliente_nome != "") |>
      dplyr::distinct()

    if (nrow(chaves) == 0) {
      return(tibble::tibble(
        empresa = character(),
        competencia = character(),
        cliente_nome = character(),
        email_principal = character(),
        telefone = character(),
        email_enviado = logical(),
        whatsapp_enviado = logical(),
        email_erro = character(),
        whatsapp_erro = character(),
        resultado = character()
      ))
    }

    resumo <- chaves |>
      dplyr::left_join(clientes_email, by = c("empresa", "competencia", "cliente_nome")) |>
      dplyr::left_join(clientes_whatsapp, by = c("empresa", "competencia", "cliente_nome")) |>
      dplyr::group_by(.data$empresa, .data$competencia, .data$cliente_nome) |>
      dplyr::summarise(
        email_principal = dplyr::first(stats::na.omit(.data$email_principal), default = ""),
        telefone = dplyr::first(stats::na.omit(.data$telefone), default = ""),
        email_enviado = any(.data$email_status == "enviado", na.rm = TRUE),
        whatsapp_enviado = any(.data$whatsapp_status == "enviado", na.rm = TRUE),
        email_erro = dplyr::first(stats::na.omit(.data$email_erro), default = ""),
        whatsapp_erro = dplyr::first(stats::na.omit(.data$whatsapp_erro), default = ""),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        resultado = dplyr::case_when(
          .data$email_enviado & .data$whatsapp_enviado ~ "Recebeu tudo",
          .data$email_enviado & !.data$whatsapp_enviado ~ "So email",
          !.data$email_enviado & .data$whatsapp_enviado ~ "So WhatsApp",
          TRUE ~ "Nao recebeu nada"
        )
      )

    resumo <- aplicar_filtros_logs(resumo, coluna_competencia = "competencia", coluna_status = "__sem_status__")

    if (!is.null(input$logs_resultado) && input$logs_resultado != "Todos") {
      resumo <- resumo |>
        dplyr::filter(.data$resultado == input$logs_resultado)
    }

    if (isTRUE(input$logs_apenas_problemas)) {
      resumo <- resumo |>
        dplyr::filter(.data$resultado != "Recebeu tudo")
    }

    resumo
  })

  output$logs_tabela <- DT::renderDT({
    datatable_padrao(
      logs_filtrados(),
      selection = "single",
      page_length = 15,
      colnames = c(
        "Data/Hora",
        "Empresa",
        "Competência",
        "Cliente",
        "Email Principal",
        "Emails Cópia",
        "Assunto",
        "Qtd PDFs",
        "Status",
        "Erro"
      )
    )
  })

  output$logs_resumo_tabela <- DT::renderDT({
    dados_tabela <- logs_resumo_clientes()

    if ("telefone" %in% names(dados_tabela)) {
      dados_tabela$telefone <- formatar_telefone_br(dados_tabela$telefone)
    }

    datatable_padrao(
      dados_tabela,
      page_length = 15,
      colnames = c(
        "Empresa",
        "CompetÃªncia",
        "Cliente",
        "Email Principal",
        "Telefone",
        "Email Enviado",
        "WhatsApp Enviado",
        "Erro Email",
        "Erro WhatsApp",
        "Resultado"
      )
    )
  })

  output$whatsapp_logs_tabela <- DT::renderDT({
    dados_tabela <- whatsapp_logs_filtrados()

    if ("telefone" %in% names(dados_tabela)) {
      dados_tabela$telefone <- formatar_telefone_br(dados_tabela$telefone)
    }

    datatable_padrao(
      dados_tabela,
      page_length = 15,
      colnames = c(
        "Data/Hora",
        "Empresa",
        "Cliente",
        "Telefone",
        "Mensagem",
        "Status",
        "Erro",
        "Origem",
        "Competência",
        "Detalhe"
      )
    )
  })

  output$processamento_logs_tabela <- DT::renderDT({
    datatable_padrao(
      processamento_logs_filtrados(),
      page_length = 15,
      colnames = c(
        "Data/Hora",
        "Origem",
        "Etapa",
        "Empresa",
        "Competência",
        "Cliente",
        "Posição",
        "Total",
        "Detalhe",
        "Erro"
      )
    )
  })

  observeEvent(input$reenviar_falhas, {
    dados <- logs_filtrados()

    if (nrow(dados) == 0) {
      logs_msg("Nenhum log encontrado.")
      return()
    }

    falhas <- dados |>
      dplyr::filter(.data$status == "erro")

    if (nrow(falhas) == 0) {
      logs_msg("Nenhuma falha encontrada para reenvio.")
      return()
    }

    resultado <- c(paste0("Reenviando ", nrow(falhas), " falha(s)."))

    for (i in seq_len(nrow(falhas))) {
      falha <- falhas[i, ]
      clientes <- ler_clientes_empresa(falha$empresa)

      cliente <- clientes |>
        dplyr::filter(.data$cliente_nome == falha$cliente_nome) |>
        dplyr::slice(1)

      if (nrow(cliente) == 0) {
        resultado <- c(resultado, paste("Cliente não encontrado:", falha$cliente_nome))
        next
      }

      tryCatch(
        {
          enviar_email_cliente(
            empresa = falha$empresa,
            cliente = cliente,
            competencia = falha$competencia_pdfs,
            mes_email = input$mes_email,
            ano_email = input$ano_email
          )

          resultado <- c(resultado, paste("OK:", falha$cliente_nome))
        },
        error = function(e) {
          resultado <<- c(
            resultado,
            paste("ERRO:", falha$cliente_nome, "-", conditionMessage(e))
          )
        }
      )
    }

    logs_msg(paste(resultado, collapse = "\n"))
  })

  observeEvent(input$limpar_logs, {
    if (!isTRUE(input$confirmar_limpar_logs)) {
      logs_msg("Marque a confirmação antes de limpar os logs.")
      return()
    }

    removidos <- limpar_arquivos_logs()
    atualizar_fila()
    logs_msg(paste("Logs limpos com sucesso. Arquivos removidos:", removidos))
  })

  output$logs_msg <- renderText({
    logs_msg()
  })

  # =========================================================
  # BACKUP
  # =========================================================

  observeEvent(input$backup_agora, {
    withProgress(
      message = "Criando backup...",
      value = 0,
      {
        incProgress(0.25, detail = "Separando arquivos essenciais")
        destino <- criar_backup_completo()
        incProgress(0.75, detail = "Finalizando backup")
        mensagem_backup(paste("Backup criado em:", destino))
      }
    )
  })

  output$mensagem_backup <- renderText({
    mensagem_backup()
  })

  # =========================================================
  # FILA DE ENVIO
  # =========================================================

  fila_msg <- reactiveVal("Aguardando geração da fila.")
  fila_refresh <- reactiveVal(0)
  fila_processamento <- reactiveVal(NULL)
  fila_proximo_ciclo <- reactiveVal(Sys.time())

  atualizar_fila <- function() {
    atualizar_contador(fila_refresh)
  }

  caminho_fila <- function() {
    file.path(pasta_raiz, "logs", "fila_envio.csv")
  }

  colunas_fila <- function() {
    c(
      "empresa",
      "competencia",
      "cliente_nome",
      "email_principal",
      "total_pdfs",
      "pasta_pdf",
      "status",
      "data_inclusao"
    )
  }

  normalizar_colunas_fila <- function(dados) {
    colunas <- colunas_fila()

    for (coluna in colunas) {
      if (!coluna %in% names(dados)) {
        dados[[coluna]] <- ""
      }
    }

    dados |>
      dplyr::select(dplyr::all_of(colunas), dplyr::everything())
  }

  carregar_fila <- function() {
    caminho <- caminho_fila()

    if (!file.exists(caminho)) {
      return(
        tibble::tibble(
          empresa = character(),
          competencia = character(),
          cliente_nome = character(),
          email_principal = character(),
          total_pdfs = numeric(),
          pasta_pdf = character(),
          status = character(),
          data_inclusao = character()
        )
      )
    }

    dados <- readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )

    normalizar_colunas_fila(dados)
  }

  salvar_fila <- function(dados) {
    dados <- normalizar_colunas_fila(dados)

    caminho <- caminho_fila()
    dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(dados, caminho)
  }

  output$ui_fila_competencia <- renderUI({
    fila_refresh()
    req(input$fila_empresa)

    competencias <- buscar_competencias(input$fila_empresa)

    selectInput(
      "fila_competencia",
      "Competência dos PDFs",
      choices = competencias,
      selected = if (length(competencias) > 0) competencias[1] else character(0)
    )
  })

  observeEvent(input$fila_competencia, {
    atualizar_referencia_email(
      competencia = input$fila_competencia,
      input_mes = "fila_mes_email",
      input_ano = "fila_ano_email"
    )
  }, ignoreInit = FALSE)

  observeEvent(input$gerar_fila, {
    if (!is.null(fila_processamento())) {
      fila_msg("Aguarde o processamento atual terminar antes de gerar nova fila.")
      return()
    }

    req(input$fila_empresa)
    req(input$fila_competencia)

    clientes <- ler_clientes_empresa(input$fila_empresa)

    if (nrow(clientes) == 0) {
      fila_msg("Arquivo de clientes não encontrado ou vazio.")
      return()
    }

    if ("ativo" %in% names(clientes)) {
      clientes <- clientes |>
        dplyr::filter(as.logical(.data$ativo) == TRUE)
    }

    verificacoes <- purrr::map(
      clientes$cliente_nome,
      ~ buscar_pdfs_cliente(input$fila_empresa, input$fila_competencia, .x)
    )

    clientes$total_pdfs <- purrr::map_int(verificacoes, "total_pdfs")
    clientes$pasta_pdf <- purrr::map_chr(verificacoes, "pasta_encontrada")

    clientes_validos <- clientes |>
      dplyr::filter(.data$total_pdfs > 0)

    if (nrow(clientes_validos) == 0) {
      fila_msg("Nenhum cliente com PDFs encontrados.")
      return()
    }

    fila <- tibble::tibble(
      empresa = as.character(input$fila_empresa),
      competencia = as.character(input$fila_competencia),
      cliente_nome = as.character(clientes_validos$cliente_nome),
      email_principal = as.character(clientes_validos$email_principal),
      total_pdfs = as.integer(clientes_validos$total_pdfs),
      pasta_pdf = as.character(clientes_validos$pasta_pdf),
      status = "pendente",
      data_inclusao = as.character(Sys.time())
    )

    criar_backup_seguro()
    salvar_fila(fila)
    atualizar_fila()

    fila_msg(paste(nrow(fila), "item(ns) adicionados à fila."))
  })

  observeEvent(input$limpar_fila, {
    if (!is.null(fila_processamento())) {
      fila_msg("Aguarde o processamento atual terminar antes de limpar a fila.")
      return()
    }

    criar_backup_seguro()

    if (file.exists(caminho_fila())) {
      file.remove(caminho_fila())
    }

    atualizar_fila()
    fila_msg("Fila limpa com sucesso.")
  })

  iniciar_processamento_fila <- function(statuses, descricao = "Processando") {
    if (!is.null(fila_processamento())) {
      fila_msg("Processamento da fila ja esta em andamento.")
      return(FALSE)
    }

    fila <- carregar_fila()

    if (nrow(fila) == 0) {
      fila_msg("Fila vazia.")
      return(FALSE)
    }

    pendentes <- which(fila$status %in% statuses)

    if (length(pendentes) == 0) {
      fila_msg("Nao existem itens para processar com os status selecionados.")
      return(FALSE)
    }

    if (any(fila$status[pendentes] %in% c("erro", "processando"))) {
      fila$status[pendentes] <- "pendente"
      salvar_fila(fila)
      atualizar_fila()
    }

    if (isTRUE(input$fila_limpar_logs_antes_processar)) {
      removidos_logs <- limpar_arquivos_logs()
      logs_msg(paste("Logs limpos antes da fila. Arquivos removidos:", removidos_logs))
    }

    criar_backup_seguro()

    resultado <- c(paste0(descricao, " ", length(pendentes), " item(ns)."))
    fila_msg(paste(resultado, collapse = "\n"))
    registrar_log_processamento(
      origem = "fila",
      etapa = "iniciar",
      detalhe = paste(
        "Pendentes:",
        length(pendentes),
        "| Versao:",
        Sys.getenv("APP_VERSION", unset = "dev"),
        "| Worker:",
        file.exists(file.path(pasta_raiz, "R", "08-email-worker.R")),
        "| Status:",
        paste(statuses, collapse = ",")
      ),
      total = length(pendentes)
    )

    fila_processamento(list(
      pendentes = pendentes,
      posicao = 1,
      resultado = resultado,
      mes_email = input$fila_mes_email,
      ano_email = input$fila_ano_email,
      enviar_whatsapp = isTRUE(input$fila_enviar_whatsapp_pos_email),
      whatsapp_intervalo_segundos = input$fila_whatsapp_intervalo_segundos
    ))
    registrar_log_processamento(
      origem = "fila",
      etapa = "agendar_primeiro_item",
      detalhe = "Agendamento inicial criado",
      total = length(pendentes)
    )
    agendar_proximo_item_fila(0.5)
    TRUE
  }

  observeEvent(input$processar_fila, {
    iniciar_processamento_fila(
      statuses = c("pendente", "erro", "processando"),
      descricao = "Processando"
    )
    return()

    if (!is.null(fila_processamento())) {
      fila_msg("Processamento da fila ja esta em andamento.")
      return()
    }

    fila <- carregar_fila()

    if (nrow(fila) == 0) {
      fila_msg("Fila vazia.")
      return()
    }

    pendentes <- which(fila$status %in% c("pendente", "erro", "processando"))

    if (length(pendentes) == 0) {
      fila_msg("Não existem itens pendentes.")
      return()
    }

    criar_backup_seguro()

    resultado <- c(paste0("Processando ", length(pendentes), " item(ns)."))
    fila_msg(paste(resultado, collapse = "\n"))
    registrar_log_processamento(
      origem = "fila",
      etapa = "iniciar",
      detalhe = paste(
        "Pendentes:",
        length(pendentes),
        "| Versao:",
        Sys.getenv("APP_VERSION", unset = "dev"),
        "| Worker:",
        file.exists(file.path(pasta_raiz, "R", "08-email-worker.R"))
      ),
      total = length(pendentes)
    )

    fila_processamento(list(
      pendentes = pendentes,
      posicao = 1,
      resultado = resultado,
      mes_email = input$fila_mes_email,
      ano_email = input$fila_ano_email,
      enviar_whatsapp = isTRUE(input$fila_enviar_whatsapp_pos_email),
      whatsapp_intervalo_segundos = input$fila_whatsapp_intervalo_segundos
    ))
    registrar_log_processamento(
      origem = "fila",
      etapa = "agendar_primeiro_item",
      detalhe = "Agendamento inicial criado",
      total = length(pendentes)
    )
    agendar_proximo_item_fila(0.5)
    return()

    for (idx in seq_along(pendentes)) {
          i <- pendentes[idx]
          item <- fila[i, ]
          cliente <- NULL

          tryCatch(
            {
              clientes <- ler_clientes_empresa(item$empresa)

              cliente <- clientes |>
                dplyr::filter(.data$cliente_nome == item$cliente_nome) |>
                dplyr::slice(1)

              if (nrow(cliente) == 0) {
                stop("Cliente não encontrado.")
              }

              fila$status[i] <- "processando"
              salvar_fila(fila)
              atualizar_fila()

              enviar_email_cliente(
                empresa = item$empresa,
                cliente = cliente,
                competencia = item$competencia,
                mes_email = input$fila_mes_email,
                ano_email = input$fila_ano_email,
                enviar_whatsapp = FALSE,
                pasta_pdf = if ("pasta_pdf" %in% names(item)) item$pasta_pdf else ""
              )

              enviar_whatsapp_apos_tentativa_email(
                empresa = item$empresa,
                cliente = cliente,
                competencia = item$competencia,
                mes_email = input$fila_mes_email,
                ano_email = input$fila_ano_email,
                enviar_whatsapp = isTRUE(input$fila_enviar_whatsapp_pos_email),
                whatsapp_intervalo_segundos = input$fila_whatsapp_intervalo_segundos,
                pasta_pdf = if ("pasta_pdf" %in% names(item)) item$pasta_pdf else "",
                email_status = "email_enviado"
              )

              fila$status[i] <- "enviado"
              salvar_fila(fila)
              atualizar_fila()

              resultado <- c(resultado, paste("OK:", item$cliente_nome))
            },
            error = function(e) {
              if (is.data.frame(cliente) && nrow(cliente) > 0) {
                enviar_whatsapp_apos_tentativa_email(
                  empresa = item$empresa,
                  cliente = cliente,
                  competencia = item$competencia,
                  mes_email = input$fila_mes_email,
                  ano_email = input$fila_ano_email,
                  enviar_whatsapp = isTRUE(input$fila_enviar_whatsapp_pos_email),
                  whatsapp_intervalo_segundos = input$fila_whatsapp_intervalo_segundos,
                  pasta_pdf = if ("pasta_pdf" %in% names(item)) item$pasta_pdf else "",
                  email_status = "email_falha"
                )
              }

              fila$status[i] <<- "erro"
              salvar_fila(fila)
              atualizar_fila()

              resultado <<- c(
                resultado,
                paste("ERRO:", item$cliente_nome, "-", conditionMessage(e))
              )
            }
          )

          fila_msg(paste(resultado, collapse = "\n"))
        }

    resultado <- c(resultado, "Processamento finalizado.")
    fila_msg(paste(resultado, collapse = "\n"))
  })

  observeEvent(input$reprocessar_erros_fila, {
    iniciar_processamento_fila(
      statuses = c("erro", "processando"),
      descricao = "Reprocessando erros"
    )
  })

  observeEvent(input$reprocessar_erros_fila_logs, {
    iniciar_processamento_fila(
      statuses = c("erro", "processando"),
      descricao = "Reprocessando erros"
    )
  })

  agendar_proximo_item_fila <- function(intervalo = 0.5) {
    intervalo <- as.numeric(intervalo)

    if (is.na(intervalo) || intervalo < 0.5) {
      intervalo <- 0.5
    }

    processamento <- isolate(fila_processamento())
    registrar_log_processamento(
      origem = "fila",
      etapa = "agendar_callback",
      posicao = if (is.null(processamento)) "" else processamento$posicao,
      total = if (is.null(processamento)) "" else length(processamento$pendentes),
      detalhe = paste("Intervalo:", intervalo)
    )

    tryCatch(
      later::later(processar_proximo_item_fila, delay = intervalo),
      error = function(e) {
        registrar_log_processamento(
          origem = "fila",
          etapa = "erro_agendamento",
          erro = conditionMessage(e)
        )
        fila_msg(paste("Erro ao agendar proximo item da fila:", conditionMessage(e)))
        fila_processamento(NULL)
      }
    )
  }

  processar_proximo_item_fila <- function() {
    tryCatch({
    processamento <- isolate(fila_processamento())

    registrar_log_processamento(
      origem = "fila",
      etapa = "callback_iniciado",
      posicao = if (is.null(processamento)) "" else processamento$posicao,
      total = if (is.null(processamento)) "" else length(processamento$pendentes)
    )

    if (is.null(processamento)) {
      return()
    }

    fila <- carregar_fila()

    if (processamento$posicao > length(processamento$pendentes)) {
      resultado <- c(processamento$resultado, "Processamento finalizado.")
      fila_msg(paste(resultado, collapse = "\n"))
      fila_processamento(NULL)
      atualizar_fila()
      registrar_log_processamento(
        origem = "fila",
        etapa = "finalizado",
        total = length(processamento$pendentes)
      )
      return()
    }

    i <- processamento$pendentes[processamento$posicao]
    item <- fila[i, ]
    cliente <- NULL
    resultado <- c(
      processamento$resultado,
      paste(
        "Processando",
        processamento$posicao,
        "de",
        length(processamento$pendentes),
        "-",
        item$cliente_nome
      )
    )
    fila_msg(paste(resultado, collapse = "\n"))
    registrar_log_processamento(
      origem = "fila",
      etapa = "processando_item",
      empresa = item$empresa,
      competencia = item$competencia,
      cliente_nome = item$cliente_nome,
      posicao = processamento$posicao,
      total = length(processamento$pendentes)
    )

    tryCatch(
      {
        clientes <- ler_clientes_empresa(item$empresa)

        cliente <- clientes |>
          dplyr::filter(.data$cliente_nome == item$cliente_nome) |>
          dplyr::slice(1)

        if (nrow(cliente) == 0) {
          stop("Cliente nao encontrado.")
        }

        registrar_log_processamento(
          origem = "fila",
          etapa = "enviando_email",
          empresa = item$empresa,
          competencia = item$competencia,
          cliente_nome = item$cliente_nome,
          posicao = processamento$posicao,
          total = length(processamento$pendentes)
        )

        fila$status[i] <- "processando"
        salvar_fila(fila)
        atualizar_fila()

        enviar_email_cliente(
          empresa = item$empresa,
          cliente = cliente,
          competencia = item$competencia,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          enviar_whatsapp = FALSE,
          pasta_pdf = if ("pasta_pdf" %in% names(item)) item$pasta_pdf else "",
          log_callback = function(etapa, detalhe = "") {
            registrar_log_processamento(
              origem = "fila",
              etapa = paste0("email_", etapa),
              empresa = item$empresa,
              competencia = item$competencia,
              cliente_nome = item$cliente_nome,
              posicao = processamento$posicao,
              total = length(processamento$pendentes),
              detalhe = detalhe
            )
          }
        )

        registrar_log_envio(
          empresa = item$empresa,
          competencia = item$competencia,
          cliente = cliente,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          status = "enviado"
        )

        registrar_log_processamento(
          origem = "fila",
          etapa = "enviando_whatsapp",
          empresa = item$empresa,
          competencia = item$competencia,
          cliente_nome = item$cliente_nome,
          posicao = processamento$posicao,
          total = length(processamento$pendentes)
        )

        whatsapp_ok <- enviar_whatsapp_apos_tentativa_email(
          empresa = item$empresa,
          cliente = cliente,
          competencia = item$competencia,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          enviar_whatsapp = isTRUE(processamento$enviar_whatsapp),
          whatsapp_intervalo_segundos = 0,
          pasta_pdf = if ("pasta_pdf" %in% names(item)) item$pasta_pdf else "",
          email_status = "email_enviado"
        )

        registrar_log_processamento(
          origem = "fila",
          etapa = if (isTRUE(whatsapp_ok)) "email_whatsapp_ok" else "email_whatsapp_nao_enviado",
          empresa = item$empresa,
          competencia = item$competencia,
          cliente_nome = item$cliente_nome,
          posicao = processamento$posicao,
          total = length(processamento$pendentes)
        )

        fila$status[i] <- "enviado"
        salvar_fila(fila)
        atualizar_fila()

        resultado <- c(resultado, paste("OK:", item$cliente_nome))
      },
      error = function(e) {
        registrar_log_processamento(
          origem = "fila",
          etapa = "erro_item",
          empresa = item$empresa,
          competencia = item$competencia,
          cliente_nome = item$cliente_nome,
          posicao = processamento$posicao,
          total = length(processamento$pendentes),
          erro = conditionMessage(e)
        )

        if (is.data.frame(cliente) && nrow(cliente) > 0) {
          enviar_whatsapp_apos_tentativa_email(
            empresa = item$empresa,
            cliente = cliente,
            competencia = item$competencia,
            mes_email = processamento$mes_email,
            ano_email = processamento$ano_email,
            enviar_whatsapp = isTRUE(processamento$enviar_whatsapp),
            whatsapp_intervalo_segundos = 0,
            pasta_pdf = if ("pasta_pdf" %in% names(item)) item$pasta_pdf else "",
            email_status = "email_falha"
          )

          registrar_log_envio(
            empresa = item$empresa,
            competencia = item$competencia,
            cliente = cliente,
            mes_email = processamento$mes_email,
            ano_email = processamento$ano_email,
            status = "erro",
            erro = conditionMessage(e)
          )
        }

        fila$status[i] <- "erro"
        salvar_fila(fila)
        atualizar_fila()

        resultado <<- c(
          resultado,
          paste("ERRO:", item$cliente_nome, "-", conditionMessage(e))
        )
      }
    )

    fila_msg(paste(resultado, collapse = "\n"))

    processamento$posicao <- processamento$posicao + 1
    processamento$resultado <- resultado
    fila_processamento(processamento)

    intervalo <- as.numeric(processamento$whatsapp_intervalo_segundos)

    if (is.na(intervalo) || intervalo < 0) {
      intervalo <- 0
    }

    agendar_proximo_item_fila(max(0.5, intervalo))
    }, error = function(e) {
      registrar_log_processamento(
        origem = "fila",
        etapa = "erro_callback",
        erro = conditionMessage(e)
      )
      fila_msg(paste("Erro no processamento da fila:", conditionMessage(e)))
      fila_processamento(NULL)
      atualizar_fila()
    })
  }

  observe({
    return()

    processamento <- isolate(fila_processamento())

    if (is.null(processamento)) {
      return()
    }

    invalidateLater(500, session)

    if (Sys.time() < fila_proximo_ciclo()) {
      return()
    }

    fila <- carregar_fila()

    if (processamento$posicao > length(processamento$pendentes)) {
      resultado <- c(processamento$resultado, "Processamento finalizado.")
      fila_msg(paste(resultado, collapse = "\n"))
      fila_processamento(NULL)
      atualizar_fila()
      return()
    }

    i <- processamento$pendentes[processamento$posicao]
    item <- fila[i, ]
    cliente <- NULL
    resultado <- c(
      processamento$resultado,
      paste(
        "Processando",
        processamento$posicao,
        "de",
        length(processamento$pendentes),
        "-",
        item$cliente_nome
      )
    )
    fila_msg(paste(resultado, collapse = "\n"))

    tryCatch(
      {
        clientes <- ler_clientes_empresa(item$empresa)

        cliente <- clientes |>
          dplyr::filter(.data$cliente_nome == item$cliente_nome) |>
          dplyr::slice(1)

        if (nrow(cliente) == 0) {
          stop("Cliente nao encontrado.")
        }

        fila$status[i] <- "processando"
        salvar_fila(fila)
        atualizar_fila()

        enviar_email_cliente(
          empresa = item$empresa,
          cliente = cliente,
          competencia = item$competencia,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          enviar_whatsapp = FALSE,
          pasta_pdf = if ("pasta_pdf" %in% names(item)) item$pasta_pdf else ""
        )

        enviar_whatsapp_apos_tentativa_email(
          empresa = item$empresa,
          cliente = cliente,
          competencia = item$competencia,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          enviar_whatsapp = isTRUE(processamento$enviar_whatsapp),
          whatsapp_intervalo_segundos = 0,
          pasta_pdf = if ("pasta_pdf" %in% names(item)) item$pasta_pdf else "",
          email_status = "email_enviado"
        )

        fila$status[i] <- "enviado"
        salvar_fila(fila)
        atualizar_fila()

        resultado <- c(resultado, paste("OK:", item$cliente_nome))
      },
      error = function(e) {
        if (is.data.frame(cliente) && nrow(cliente) > 0) {
          enviar_whatsapp_apos_tentativa_email(
            empresa = item$empresa,
            cliente = cliente,
            competencia = item$competencia,
            mes_email = processamento$mes_email,
            ano_email = processamento$ano_email,
            enviar_whatsapp = isTRUE(processamento$enviar_whatsapp),
            whatsapp_intervalo_segundos = 0,
            email_status = "email_falha"
          )
        }

        fila$status[i] <- "erro"
        salvar_fila(fila)
        atualizar_fila()

        resultado <<- c(
          resultado,
          paste("ERRO:", item$cliente_nome, "-", conditionMessage(e))
        )
      }
    )

    fila_msg(paste(resultado, collapse = "\n"))

    processamento$posicao <- processamento$posicao + 1
    processamento$resultado <- resultado
    intervalo <- as.numeric(processamento$whatsapp_intervalo_segundos)

    if (is.na(intervalo) || intervalo < 0) {
      intervalo <- 0
    }

    fila_processamento(processamento)
    fila_proximo_ciclo(Sys.time() + max(0.5, intervalo))
  })

  output$fila_tabela <- DT::renderDT({
    fila_refresh()
    dados_tabela <- carregar_fila()
    status_filtro <- input$fila_status_filtro

    if (is.null(status_filtro) || length(status_filtro) == 0) {
      status_filtro <- c("pendente", "processando", "erro")
    }

    dados_tabela <- dados_tabela |>
      dplyr::filter(.data$status %in% status_filtro)

    datatable_padrao(
      dados_tabela,
      selection = "single",
      page_length = 15,
      colnames = c(
        "Empresa",
        "Competência",
        "Cliente",
        "Email Principal",
        "Qtd PDFs",
        "Pasta PDF",
        "Status",
        "Data Inclusão"
      )
    )
  })

  output$ui_fila_editar_cliente <- renderUI({
    empresa <- input$fila_empresa

    if (is.null(empresa) || empresa == "") {
      empresas <- listar_empresas()
      empresa <- if (length(empresas) > 0) empresas[1] else ""
    }

    clientes <- ler_clientes_empresa(empresa)
    escolhas <- sort(as.character(clientes$cliente_nome))
    escolhas <- escolhas[!is.na(escolhas) & escolhas != ""]

    selectInput(
      "fila_editar_cliente",
      "Cliente",
      choices = escolhas,
      selected = if (length(escolhas) > 0) escolhas[1] else character(0)
    )
  })

  fila_filtrada_atual <- function() {
    dados <- carregar_fila()
    status_filtro <- input$fila_status_filtro

    if (is.null(status_filtro) || length(status_filtro) == 0) {
      status_filtro <- c("pendente", "processando", "erro")
    }

    dados |>
      dplyr::filter(.data$status %in% status_filtro)
  }

  indice_fila_selecionada <- function() {
    linha <- input$fila_tabela_rows_selected

    if (is.null(linha) || length(linha) == 0) {
      return(integer(0))
    }

    fila <- carregar_fila()
    filtrada <- fila_filtrada_atual()

    if (nrow(filtrada) < linha) {
      return(integer(0))
    }

    item <- filtrada[linha, ]

    idx <- which(
      fila$empresa == item$empresa[1] &
        fila$competencia == item$competencia[1] &
        fila$cliente_nome == item$cliente_nome[1] &
        fila$data_inclusao == item$data_inclusao[1]
    )

    idx[1]
  }

  observeEvent(input$fila_tabela_rows_selected, {
    idx <- indice_fila_selecionada()

    if (length(idx) == 0 || is.na(idx)) {
      return()
    }

    item <- carregar_fila()[idx, ]

    updateSelectInput(session, "fila_editar_cliente", selected = item$cliente_nome[1])
    updateTextInput(session, "fila_editar_pasta_pdf", value = item$pasta_pdf[1])
    updateSelectInput(session, "fila_editar_status", selected = item$status[1])
  })

  observeEvent(input$salvar_item_fila, {
    if (!is.null(fila_processamento())) {
      fila_msg("Aguarde o processamento atual terminar antes de editar a fila.")
      return()
    }

    idx <- indice_fila_selecionada()

    if (length(idx) == 0 || is.na(idx)) {
      fila_msg("Selecione um item da fila para editar.")
      return()
    }

    tryCatch({
      fila <- carregar_fila()
      item <- fila[idx, ]
      cliente <- ler_clientes_empresa(item$empresa[1]) |>
        dplyr::filter(.data$cliente_nome == input$fila_editar_cliente) |>
        dplyr::slice(1)

      if (nrow(cliente) == 0) {
        stop("Cliente selecionado nao encontrado.")
      }

      pasta_pdf <- trimws(as.character(input$fila_editar_pasta_pdf))

      verificacao <- buscar_pdfs_cliente(
        empresa = item$empresa[1],
        competencia = item$competencia[1],
        cliente_nome = input$fila_editar_cliente,
        pasta_pdf = pasta_pdf
      )

      if (pasta_pdf != "" && verificacao$total_pdfs > 0) {
        salvar_pdf_alias(
          empresa = item$empresa[1],
          pasta_pdf = pasta_pdf,
          cliente_nome = input$fila_editar_cliente
        )
      }

      fila$cliente_nome[idx] <- as.character(input$fila_editar_cliente)
      fila$email_principal[idx] <- as.character(cliente$email_principal[1])
      fila$total_pdfs[idx] <- as.integer(verificacao$total_pdfs)
      fila$pasta_pdf[idx] <- pasta_pdf
      fila$status[idx] <- as.character(input$fila_editar_status)

      criar_backup_seguro()
      salvar_fila(fila)
      atualizar_fila()
      atualizar_pdfs()

      fila_msg(paste("Item atualizado:", input$fila_editar_cliente))
    }, error = function(e) {
      fila_msg(paste("Erro ao atualizar item:", conditionMessage(e)))
    })
  })

  observeEvent(input$excluir_item_fila, {
    if (!is.null(fila_processamento())) {
      fila_msg("Aguarde o processamento atual terminar antes de excluir item da fila.")
      return()
    }

    idx <- indice_fila_selecionada()

    if (length(idx) == 0 || is.na(idx)) {
      fila_msg("Selecione um item da fila para excluir.")
      return()
    }

    fila <- carregar_fila()
    cliente_nome <- fila$cliente_nome[idx]
    fila <- fila[-idx, , drop = FALSE]

    criar_backup_seguro()
    salvar_fila(fila)
    atualizar_fila()

    fila_msg(paste("Item removido da fila:", cliente_nome))
  })

  output$fila_msg <- renderText({
    fila_msg()
  })

  # =========================================================
  # ENVIO
  # =========================================================

  observeEvent(input$enviar_todos, {
    if (!is.null(disparo_processamento())) {
      resultado_envio("Envio ja esta em andamento.")
      return()
    }

    if (!isTRUE(input$confirmar_envio)) {
      resultado_envio("Marque a confirmação antes de enviar.")
      return()
    }

    clientes <- dados_clientes() |>
      dplyr::filter(.data$total_pdfs > 0)

    if (nrow(clientes) == 0) {
      resultado_envio("Nenhum cliente com PDF encontrado.")
      return()
    }

    criar_backup_seguro()

    resultado <- c(paste0("Iniciando envio: ", nrow(clientes), " cliente(s)."))
    resultado_envio(paste(resultado, collapse = "\n"))
    registrar_log_processamento(
      origem = "disparo",
      etapa = "iniciar",
      empresa = input$empresa,
      competencia = input$competencia,
      detalhe = paste("Clientes:", nrow(clientes)),
      total = nrow(clientes)
    )

    disparo_processamento(list(
      clientes = clientes,
      posicao = 1,
      resultado = resultado,
      empresa = input$empresa,
      competencia = input$competencia,
      mes_email = input$mes_email,
      ano_email = input$ano_email,
      enviar_whatsapp = isTRUE(input$enviar_whatsapp_pos_email),
      whatsapp_intervalo_segundos = input$whatsapp_intervalo_segundos
    ))
    registrar_log_processamento(
      origem = "disparo",
      etapa = "agendar_primeiro_item",
      empresa = input$empresa,
      competencia = input$competencia,
      detalhe = "Agendamento inicial criado",
      total = nrow(clientes)
    )
    agendar_proximo_item_disparo(0.5)
    return()

    for (i in seq_len(nrow(clientes))) {
      cliente <- clientes[i, ]

      tryCatch(
        {
          enviar_email_cliente(
            empresa = input$empresa,
            cliente = cliente,
            competencia = input$competencia,
            mes_email = input$mes_email,
            ano_email = input$ano_email,
            enviar_whatsapp = FALSE
          )

          enviar_whatsapp_apos_tentativa_email(
            empresa = input$empresa,
            cliente = cliente,
            competencia = input$competencia,
            mes_email = input$mes_email,
            ano_email = input$ano_email,
            enviar_whatsapp = isTRUE(input$enviar_whatsapp_pos_email),
            whatsapp_intervalo_segundos = input$whatsapp_intervalo_segundos,
            email_status = "email_enviado"
          )

          resultado <- c(resultado, paste("OK:", cliente$cliente_nome))
        },
        error = function(e) {
          enviar_whatsapp_apos_tentativa_email(
            empresa = input$empresa,
            cliente = cliente,
            competencia = input$competencia,
            mes_email = input$mes_email,
            ano_email = input$ano_email,
            enviar_whatsapp = isTRUE(input$enviar_whatsapp_pos_email),
            whatsapp_intervalo_segundos = input$whatsapp_intervalo_segundos,
            email_status = "email_falha"
          )

          resultado <<- c(
            resultado,
            paste("ERRO:", cliente$cliente_nome, "-", conditionMessage(e))
          )
        }
      )

      resultado_envio(paste(resultado, collapse = "\n"))
    }

    resultado <- c(resultado, "Envio finalizado.")
    resultado_envio(paste(resultado, collapse = "\n"))
    atualizar_disparo()
  })

  agendar_proximo_item_disparo <- function(intervalo = 0.5) {
    intervalo <- as.numeric(intervalo)

    if (is.na(intervalo) || intervalo < 0.5) {
      intervalo <- 0.5
    }

    processamento <- isolate(disparo_processamento())
    registrar_log_processamento(
      origem = "disparo",
      etapa = "agendar_callback",
      empresa = if (is.null(processamento)) "" else processamento$empresa,
      competencia = if (is.null(processamento)) "" else processamento$competencia,
      posicao = if (is.null(processamento)) "" else processamento$posicao,
      total = if (is.null(processamento)) "" else nrow(processamento$clientes),
      detalhe = paste("Intervalo:", intervalo)
    )

    tryCatch(
      later::later(processar_proximo_item_disparo, delay = intervalo),
      error = function(e) {
        registrar_log_processamento(
          origem = "disparo",
          etapa = "erro_agendamento",
          erro = conditionMessage(e)
        )
        resultado_envio(paste("Erro ao agendar proximo item do disparo:", conditionMessage(e)))
        disparo_processamento(NULL)
      }
    )
  }

  processar_proximo_item_disparo <- function() {
    tryCatch({
    processamento <- isolate(disparo_processamento())

    registrar_log_processamento(
      origem = "disparo",
      etapa = "callback_iniciado",
      empresa = if (is.null(processamento)) "" else processamento$empresa,
      competencia = if (is.null(processamento)) "" else processamento$competencia,
      posicao = if (is.null(processamento)) "" else processamento$posicao,
      total = if (is.null(processamento)) "" else nrow(processamento$clientes)
    )

    if (is.null(processamento)) {
      return()
    }

    if (processamento$posicao > nrow(processamento$clientes)) {
      resultado <- c(processamento$resultado, "Envio finalizado.")
      resultado_envio(paste(resultado, collapse = "\n"))
      disparo_processamento(NULL)
      atualizar_disparo()
      registrar_log_processamento(
        origem = "disparo",
        etapa = "finalizado",
        empresa = processamento$empresa,
        competencia = processamento$competencia,
        total = nrow(processamento$clientes)
      )
      return()
    }

    cliente <- processamento$clientes[processamento$posicao, ]
    resultado <- c(
      processamento$resultado,
      paste(
        "Processando",
        processamento$posicao,
        "de",
        nrow(processamento$clientes),
        "-",
        cliente$cliente_nome
      )
    )
    resultado_envio(paste(resultado, collapse = "\n"))
    registrar_log_processamento(
      origem = "disparo",
      etapa = "processando_item",
      empresa = processamento$empresa,
      competencia = processamento$competencia,
      cliente_nome = cliente$cliente_nome,
      posicao = processamento$posicao,
      total = nrow(processamento$clientes)
    )

    tryCatch(
      {
        registrar_log_processamento(
          origem = "disparo",
          etapa = "enviando_email",
          empresa = processamento$empresa,
          competencia = processamento$competencia,
          cliente_nome = cliente$cliente_nome,
          posicao = processamento$posicao,
          total = nrow(processamento$clientes)
        )

        enviar_email_cliente(
          empresa = processamento$empresa,
          cliente = cliente,
          competencia = processamento$competencia,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          enviar_whatsapp = FALSE,
          pasta_pdf = if ("pasta_encontrada" %in% names(cliente)) cliente$pasta_encontrada else "",
          log_callback = function(etapa, detalhe = "") {
            registrar_log_processamento(
              origem = "disparo",
              etapa = paste0("email_", etapa),
              empresa = processamento$empresa,
              competencia = processamento$competencia,
              cliente_nome = cliente$cliente_nome,
              posicao = processamento$posicao,
              total = nrow(processamento$clientes),
              detalhe = detalhe
            )
          }
        )

        registrar_log_envio(
          empresa = processamento$empresa,
          competencia = processamento$competencia,
          cliente = cliente,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          status = "enviado"
        )

        registrar_log_processamento(
          origem = "disparo",
          etapa = "enviando_whatsapp",
          empresa = processamento$empresa,
          competencia = processamento$competencia,
          cliente_nome = cliente$cliente_nome,
          posicao = processamento$posicao,
          total = nrow(processamento$clientes)
        )

        whatsapp_ok <- enviar_whatsapp_apos_tentativa_email(
          empresa = processamento$empresa,
          cliente = cliente,
          competencia = processamento$competencia,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          enviar_whatsapp = isTRUE(processamento$enviar_whatsapp),
          whatsapp_intervalo_segundos = 0,
          pasta_pdf = if ("pasta_encontrada" %in% names(cliente)) cliente$pasta_encontrada else "",
          email_status = "email_enviado"
        )

        registrar_log_processamento(
          origem = "disparo",
          etapa = if (isTRUE(whatsapp_ok)) "email_whatsapp_ok" else "email_whatsapp_nao_enviado",
          empresa = processamento$empresa,
          competencia = processamento$competencia,
          cliente_nome = cliente$cliente_nome,
          posicao = processamento$posicao,
          total = nrow(processamento$clientes)
        )

        resultado <- c(resultado, paste("OK:", cliente$cliente_nome))
      },
      error = function(e) {
        registrar_log_processamento(
          origem = "disparo",
          etapa = "erro_item",
          empresa = processamento$empresa,
          competencia = processamento$competencia,
          cliente_nome = cliente$cliente_nome,
          posicao = processamento$posicao,
          total = nrow(processamento$clientes),
          erro = conditionMessage(e)
        )

        enviar_whatsapp_apos_tentativa_email(
          empresa = processamento$empresa,
          cliente = cliente,
          competencia = processamento$competencia,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          enviar_whatsapp = isTRUE(processamento$enviar_whatsapp),
          whatsapp_intervalo_segundos = 0,
          pasta_pdf = if ("pasta_encontrada" %in% names(cliente)) cliente$pasta_encontrada else "",
          email_status = "email_falha"
        )

        registrar_log_envio(
          empresa = processamento$empresa,
          competencia = processamento$competencia,
          cliente = cliente,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          status = "erro",
          erro = conditionMessage(e)
        )

        resultado <<- c(
          resultado,
          paste("ERRO:", cliente$cliente_nome, "-", conditionMessage(e))
        )
      }
    )

    resultado_envio(paste(resultado, collapse = "\n"))

    processamento$posicao <- processamento$posicao + 1
    processamento$resultado <- resultado
    disparo_processamento(processamento)

    intervalo <- as.numeric(processamento$whatsapp_intervalo_segundos)

    if (is.na(intervalo) || intervalo < 0) {
      intervalo <- 0
    }

    agendar_proximo_item_disparo(max(0.5, intervalo))
    }, error = function(e) {
      registrar_log_processamento(
        origem = "disparo",
        etapa = "erro_callback",
        erro = conditionMessage(e)
      )
      resultado_envio(paste("Erro no processamento do disparo:", conditionMessage(e)))
      disparo_processamento(NULL)
      atualizar_disparo()
    })
  }

  observe({
    return()

    processamento <- isolate(disparo_processamento())

    if (is.null(processamento)) {
      return()
    }

    invalidateLater(500, session)

    if (Sys.time() < disparo_proximo_ciclo()) {
      return()
    }

    if (processamento$posicao > nrow(processamento$clientes)) {
      resultado <- c(processamento$resultado, "Envio finalizado.")
      resultado_envio(paste(resultado, collapse = "\n"))
      disparo_processamento(NULL)
      atualizar_disparo()
      return()
    }

    cliente <- processamento$clientes[processamento$posicao, ]
    resultado <- c(
      processamento$resultado,
      paste(
        "Processando",
        processamento$posicao,
        "de",
        nrow(processamento$clientes),
        "-",
        cliente$cliente_nome
      )
    )
    resultado_envio(paste(resultado, collapse = "\n"))

    tryCatch(
      {
        enviar_email_cliente(
          empresa = processamento$empresa,
          cliente = cliente,
          competencia = processamento$competencia,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          enviar_whatsapp = FALSE
        )

        enviar_whatsapp_apos_tentativa_email(
          empresa = processamento$empresa,
          cliente = cliente,
          competencia = processamento$competencia,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          enviar_whatsapp = isTRUE(processamento$enviar_whatsapp),
          whatsapp_intervalo_segundos = 0,
          email_status = "email_enviado"
        )

        resultado <- c(resultado, paste("OK:", cliente$cliente_nome))
      },
      error = function(e) {
        enviar_whatsapp_apos_tentativa_email(
          empresa = processamento$empresa,
          cliente = cliente,
          competencia = processamento$competencia,
          mes_email = processamento$mes_email,
          ano_email = processamento$ano_email,
          enviar_whatsapp = isTRUE(processamento$enviar_whatsapp),
          whatsapp_intervalo_segundos = 0,
          email_status = "email_falha"
        )

        resultado <<- c(
          resultado,
          paste("ERRO:", cliente$cliente_nome, "-", conditionMessage(e))
        )
      }
    )

    resultado_envio(paste(resultado, collapse = "\n"))

    processamento$posicao <- processamento$posicao + 1
    processamento$resultado <- resultado
    intervalo <- as.numeric(processamento$whatsapp_intervalo_segundos)

    if (is.na(intervalo) || intervalo < 0) {
      intervalo <- 0
    }

    disparo_processamento(processamento)
    disparo_proximo_ciclo(Sys.time() + max(0.5, intervalo))
  })

  output$resultado_envio <- renderText({
    resultado_envio()
  })
}
