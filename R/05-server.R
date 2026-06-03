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
  
  session$userData$remetente_modo_novo <- FALSE
  session$userData$smtp_modo_novo <- FALSE
  
  atualizar_contador <- function(contador) {
    contador(contador() + 1)
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
  
  output$clientes_tabela <- DT::renderDT({
    datatable_padrao(
      clientes_dados(),
      selection = "single",
      colnames = c(
        "Cliente",
        "Email Principal",
        "Emails Cópia",
        "WhatsApp",
        "Ativo",
        "Observação"
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
    updateTextInput(session, "cliente_whatsapp", value = cliente$telefone_whatsapp)
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
      telefone_whatsapp = as.character(input$cliente_whatsapp),
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
          grepl("\\.pdf$", .data$nome_zip, ignore.case = TRUE),
          grepl("/", .data$nome_zip, fixed = TRUE)
        )
      
      if (nrow(pdfs_zip) == 0) {
        stop("ZIP deve conter PDFs dentro de pastas de clientes.")
      }
      
      tmp <- tempfile("pdfs-")
      dir_create(tmp)
      utils::unzip(input$pdf_zip$datapath, exdir = tmp)
      
      total <- 0
      
      for (i in seq_len(nrow(pdfs_zip))) {
        nome_zip <- pdfs_zip$nome_zip[i]
        partes <- strsplit(nome_zip, "/", fixed = TRUE)[[1]]
        cliente <- limpar_segmento_caminho(partes[1])
        arquivo <- limpar_segmento_caminho(tail(partes, 1))
        
        origem <- do.call(file.path, as.list(c(tmp, partes)))
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
      
      atualizar_pdfs()
      
      pdf_msg(
        paste0(
          total,
          " PDF(s) importado(s) do ZIP para ",
          input$pdf_empresa,
          "/clientes/",
          competencia
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
      
      for (competencia in remover) {
        zip_backup <- criar_backup_competencia_pdf(
          input$pdf_empresa,
          competencia
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
      }
      
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
    datatable_padrao(
      dados_clientes(),
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
    
    nome_empresa <- input$modelos_empresa
    caminho_remetentes <- file.path(pasta_raiz, "_config", "remetentes.csv")
    
    if (file.exists(caminho_remetentes)) {
      remetentes <- readr::read_csv(caminho_remetentes, show_col_types = FALSE)
      
      registro <- remetentes |>
        dplyr::filter(.data$empresa_id == input$modelos_empresa) |>
        dplyr::slice(1)
      
      if (
        nrow(registro) > 0 &&
        "empresa_nome" %in% names(registro) &&
        !is.na(registro$empresa_nome[1]) &&
        registro$empresa_nome[1] != ""
      ) {
        nome_empresa <- registro$empresa_nome[1]
      }
    }
    
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
        gsub("{{cliente_nome}}", "Cliente Exemplo LTDA", x = _, fixed = TRUE) |>
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
          email_remetente = character(),
          nome_remetente = character(),
          smtp_id = character(),
          ativo = logical()
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
  
  carregar_chatwoot <- function() {
    caminho <- caminho_chatwoot()
    
    if (!file.exists(caminho)) {
      return(
        tibble::tibble(
          chatwoot_id = character(),
          empresa_id = character(),
          base_url = character(),
          account_id = character(),
          inbox_identifier = character(),
          api_access_token = character(),
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
        "Token",
        "Ativo",
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
    updateTextInput(session, "cw_token", value = item$api_access_token)
    updateCheckboxInput(session, "cw_ativo", value = as.logical(item$ativo))
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
    updateTextInput(session, "cw_token", value = "")
    updateCheckboxInput(session, "cw_ativo", value = TRUE)
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
      api_access_token = as.character(input$cw_token),
      ativo = as.logical(input$cw_ativo),
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
      
      telefone <- gsub("[^0-9]", "", input$cw_teste_telefone)
      
      if (nchar(telefone) < 10) {
        stop("Telefone inválido. Informe DDD + número.")
      }
      
      contato_url <- paste0(
        input$cw_base_url,
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
            name = input$cw_teste_nome,
            phone_number = paste0("+55", telefone)
          )
        ) |>
        httr2::req_perform()
      
      contato_json <- httr2::resp_body_json(contato_resp)
      
      contact_identifier <- contato_json$source_id
      
      if (is.null(contact_identifier) || contact_identifier == "") {
        contact_identifier <- contato_json$id
      }
      
      conversa_url <- paste0(
        input$cw_base_url,
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
        httr2::req_perform()
      
      conversa_json <- httr2::resp_body_json(conversa_resp)
      
      conversation_id <- conversa_json$id
      
      mensagem_url <- paste0(
        input$cw_base_url,
        "/public/api/v1/inboxes/",
        input$cw_inbox_identifier,
        "/contacts/",
        contact_identifier,
        "/conversations/",
        conversation_id,
        "/messages"
      )
      
      httr2::request(mensagem_url) |>
        httr2::req_headers(
          "Content-Type" = "application/json",
          "api_access_token" = input$cw_token
        ) |>
        httr2::req_body_json(
          list(
            content = input$cw_teste_msg
          )
        ) |>
        httr2::req_perform()
      
      cw_msg("Mensagem de teste enviada pelo Chatwoot.")
    },
    error = function(e) {
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
    
    readr::read_csv(caminho, show_col_types = FALSE)
  }
  
  logs_filtrados <- reactive({
    input$atualizar_logs
    dados <- carregar_logs_dados()
    
    if (nrow(dados) == 0) {
      return(dados)
    }
    
    if (!is.null(input$logs_empresa) && input$logs_empresa != "Todas") {
      dados <- dados |>
        dplyr::filter(.data$empresa == input$logs_empresa)
    }
    
    if (!is.null(input$logs_competencia) && trimws(input$logs_competencia) != "") {
      dados <- dados |>
        dplyr::filter(.data$competencia_pdfs == input$logs_competencia)
    }
    
    dados
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
  
  atualizar_fila <- function() {
    atualizar_contador(fila_refresh)
  }
  
  caminho_fila <- function() {
    file.path(pasta_raiz, "logs", "fila_envio.csv")
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
          status = character(),
          data_inclusao = character()
        )
      )
    }
    
    readr::read_csv(
      caminho,
      show_col_types = FALSE,
      col_types = readr::cols(.default = "c")
    )
  }
  
  salvar_fila <- function(dados) {
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
  
  observeEvent(input$gerar_fila, {
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
      status = "pendente",
      data_inclusao = as.character(Sys.time())
    )
    
    criar_backup_seguro()
    salvar_fila(fila)
    atualizar_fila()
    
    fila_msg(paste(nrow(fila), "item(ns) adicionados à fila."))
  })
  
  observeEvent(input$limpar_fila, {
    criar_backup_seguro()
    
    if (file.exists(caminho_fila())) {
      file.remove(caminho_fila())
    }
    
    atualizar_fila()
    fila_msg("Fila limpa com sucesso.")
  })
  
  observeEvent(input$processar_fila, {
    fila <- carregar_fila()
    
    if (nrow(fila) == 0) {
      fila_msg("Fila vazia.")
      return()
    }
    
    pendentes <- which(fila$status %in% c("pendente", "erro"))
    
    if (length(pendentes) == 0) {
      fila_msg("Não existem itens pendentes.")
      return()
    }
    
    criar_backup_seguro()
    
    resultado <- c(paste0("Processando ", length(pendentes), " item(ns)."))
    fila_msg(paste(resultado, collapse = "\n"))
    
    withProgress(
      message = "Processando fila...",
      value = 0,
      {
        total_itens <- length(pendentes)
        
        for (idx in seq_along(pendentes)) {
          i <- pendentes[idx]
          item <- fila[i, ]
          
          incProgress(
            amount = 1 / total_itens,
            detail = paste("Enviando para", item$cliente_nome)
          )
          
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
                ano_email = input$fila_ano_email
              )
              
              fila$status[i] <- "enviado"
              salvar_fila(fila)
              atualizar_fila()
              
              resultado <- c(resultado, paste("OK:", item$cliente_nome))
            },
            error = function(e) {
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
      }
    )
    
    resultado <- c(resultado, "Processamento finalizado.")
    fila_msg(paste(resultado, collapse = "\n"))
  })
  
  output$fila_tabela <- DT::renderDT({
    fila_refresh()
    
    datatable_padrao(
      carregar_fila(),
      page_length = 15,
      colnames = c(
        "Empresa",
        "Competência",
        "Cliente",
        "Email Principal",
        "Qtd PDFs",
        "Status",
        "Data Inclusão"
      )
    )
  })
  
  output$fila_msg <- renderText({
    fila_msg()
  })
  
  # =========================================================
  # ENVIO
  # =========================================================
  
  observeEvent(input$enviar_todos, {
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
    
    for (i in seq_len(nrow(clientes))) {
      cliente <- clientes[i, ]
      
      tryCatch(
        {
          enviar_email_cliente(
            empresa = input$empresa,
            cliente = cliente,
            competencia = input$competencia,
            mes_email = input$mes_email,
            ano_email = input$ano_email
          )
          
          resultado <- c(resultado, paste("OK:", cliente$cliente_nome))
        },
        error = function(e) {
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
  
  output$resultado_envio <- renderText({
    resultado_envio()
  })
}
