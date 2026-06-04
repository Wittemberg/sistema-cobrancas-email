# =========================================================
# INTERFACE
# =========================================================

ui_app <- fluidPage(
  tags$head(
    tags$link(
      rel = "icon",
      type = "image/svg+xml",
      href = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 64 64'%3E%3Crect width='64' height='64' rx='12' fill='%231f1f1f'/%3E%3Cpath d='M8 10h8l5 30 7-30h8l7 30 5-30h8L47 54h-8l-7-29-7 29h-8L8 10z' fill='%23ffffff'/%3E%3Crect x='8' y='55' width='48' height='5' fill='%23d40000'/%3E%3C/svg%3E"
    ),
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
      }

      .navbar {
        background: linear-gradient(90deg, #1f1f1f, #3a3a3a);
        border-bottom: 4px solid #d40000;
        position: sticky;
        top: 0;
        z-index: 1000;
        width: 100%;
      }

      .navbar-collapse {
        max-height: calc(100vh - 56px);
        overflow-y: auto;
      }

      .navbar-brand {
        color: white !important;
        font-size: 24px;
        font-weight: bold;
      }

      .btn-primary {
        background-color: #d40000;
        border-color: #d40000;
      }

      .btn-primary:hover {
        background-color: #a00000;
      }

      table.dataTable thead {
        background-color: #2a2a2a;
        color: white;
      }

      h3 {
        border-left: 5px solid #d40000;
        padding-left: 10px;
      }

      .tela-padrao {
        display: flex;
        align-items: flex-start;
        gap: 24px;
        width: 100%;
      }

      .coluna-edicao {
        width: 430px;
        min-width: 430px;
        max-width: 430px;
      }

      .coluna-conteudo {
        flex: 1;
        min-width: 0;
      }

      .painel-formulario {
        background-color: #ffffff;
        border: 1px solid #dddddd;
        border-radius: 8px;
        padding: 24px;
        margin-top: 24px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.05);
        width: 100%;
        box-sizing: border-box;
      }

      .painel-conteudo {
        padding-top: 24px;
        width: 100%;
        box-sizing: border-box;
      }

      .painel-formulario h3,
      .painel-conteudo h3 {
        margin-top: 0;
        margin-bottom: 18px;
      }
    ")),

    tags$script(HTML("
      function formatTelefoneBR(value) {
        var digits = String(value || '').replace(/\\D/g, '');

        if (digits.indexOf('55') === 0 && (digits.length === 12 || digits.length === 13)) {
          digits = digits.substring(2);
        }

        if (digits.length > 11) {
          digits = digits.substring(0, 11);
        }

        if (digits.length > 10) {
          return '(' + digits.substring(0, 2) + ') ' + digits.substring(2, 7) + '-' + digits.substring(7, 11);
        }

        if (digits.length > 6) {
          return '(' + digits.substring(0, 2) + ') ' + digits.substring(2, 6) + '-' + digits.substring(6, 10);
        }

        if (digits.length > 2) {
          return '(' + digits.substring(0, 2) + ') ' + digits.substring(2);
        }

        return digits;
      }

      $(document).on('input blur', '#cliente_whatsapp, #cw_teste_telefone, #rem_empresa_whatsapp', function() {
        $(this).val(formatTelefoneBR($(this).val()));
      });
    "))
  ),

  navbarPage(
    title = "Disparo de Mensagens",
    id = "abas",

    tabPanel(
      "Dashboard",
      br(),

      fluidRow(
        column(3, div(class = "well", h4("Empresas"), h2(textOutput("dash_empresas")))),
        column(3, div(class = "well", h4("Clientes Ativos"), h2(textOutput("dash_clientes_ativos")))),
        column(2, div(class = "well", h4("Envios OK"), h2(textOutput("dash_envios_ok")))),
        column(2, div(class = "well", h4("Erros"), h2(textOutput("dash_envios_erro")))),
        column(2, div(class = "well", h4("Fila Pendente"), h2(textOutput("dash_fila_pendente"))))
      ),
      fluidRow(
        column(3, div(class = "well", h4("WhatsApp OK"), h2(textOutput("dash_whatsapp_ok")))),
        column(3, div(class = "well", h4("WhatsApp Erros"), h2(textOutput("dash_whatsapp_erro")))),
        column(3, div(class = "well", h4("Versao"), h5(textOutput("dash_app_version")))),
        column(3, div(class = "well", h4("Worker Email"), h5(textOutput("dash_email_worker"))))
      ),
      fluidRow(
        column(
          6,
          div(
            class = "painel-conteudo",
            h3("Envios por Empresa"),
            DTOutput("dash_envios_empresa")
          )
        ),
        column(
          6,
          div(
            class = "painel-conteudo",
            h3("Últimos Envios"),
            DTOutput("dash_ultimos_envios")
          )
        )
      )
    ),

    tabPanel(
      "Disparo",

      div(
        class = "tela-padrao",

        div(
          class = "coluna-edicao",
          div(
            class = "painel-formulario",

            selectInput("empresa", "Empresa", choices = listar_empresas()),
            uiOutput("ui_competencia"),
            textInput("mes_email", "Mês de referência do email", value = "Abril"),
            numericInput("ano_email", "Ano de referência do email", value = 2026, min = 2020, max = 2100),
            checkboxInput("somente_ativos", "Mostrar somente clientes ativos", value = TRUE),
            actionButton("atualizar", "Atualizar Lista"),

            hr(),

            checkboxInput(
              "confirmar_envio",
              "Confirmo que revisei os dados para envio em massa",
              value = FALSE
            ),

            checkboxInput(
              "enviar_whatsapp_pos_email",
              "Enviar WhatsApp após e-mail",
              value = FALSE
            ),

            numericInput(
              "whatsapp_intervalo_segundos",
              "Intervalo WhatsApp (segundos)",
              value = 10,
              min = 0,
              max = 300,
              step = 1
            ),

            actionButton("enviar_todos", "Enviar Todos", class = "btn-primary"),

            hr(),

            actionButton("backup_agora", "Criar Backup Agora"),

            br(), br(),

            verbatimTextOutput("mensagem_backup")
          )
        ),

        div(
          class = "coluna-conteudo",
          div(
            class = "painel-conteudo",

            h3("Clientes aptos para envio"),

            DTOutput("tabela_clientes"),

            br(),

            verbatimTextOutput("resultado_envio")
          )
        )
      )
    ),

    tabPanel(
      "Fila",

      div(
        class = "tela-padrao",

        div(
          class = "coluna-edicao",
          div(
            class = "painel-formulario",

            h3("Fila de Envio"),

            selectInput(
              "fila_empresa",
              "Empresa",
              choices = listar_empresas()
            ),

            uiOutput("ui_fila_competencia"),

            textInput(
              "fila_mes_email",
              "Mês de referência do email",
              value = "Abril"
            ),

            numericInput(
              "fila_ano_email",
              "Ano de referência do email",
              value = 2026,
              min = 2020,
              max = 2100
            ),

            hr(),

            numericInput(
              "fila_whatsapp_intervalo_segundos",
              "Intervalo WhatsApp (segundos)",
              value = 10,
              min = 0,
              max = 300,
              step = 1
            ),

            checkboxInput(
              "fila_enviar_whatsapp_pos_email",
              "Enviar WhatsApp após e-mail",
              value = FALSE
            ),

            actionButton(
              "gerar_fila",
              "Gerar Fila",
              class = "btn-primary"
            ),

            br(),
            br(),

            actionButton(
              "processar_fila",
              "Processar Fila"
            ),

            br(),
            br(),

            actionButton(
              "limpar_fila",
              "Limpar Fila"
            ),

            br(),
            br(),

            verbatimTextOutput("fila_msg")
          )
        ),

        div(
          class = "coluna-conteudo",
          div(
            class = "painel-conteudo",

            h3("Itens da Fila"),

            checkboxGroupInput(
              "fila_status_filtro",
              "Status",
              choices = c("pendente", "processando", "erro", "enviado"),
              selected = c("pendente", "processando", "erro"),
              inline = TRUE
            ),

            DTOutput("fila_tabela")
          )
        )
      )
    ),

    tabPanel(
      "Clientes",

      div(
        class = "tela-padrao",

        div(
          class = "coluna-edicao",
          div(
            class = "painel-formulario",

            h3("Cadastro de Clientes"),

            selectInput("clientes_empresa", "Empresa", choices = listar_empresas()),

            hr(),

            fileInput("importar_clientes", "Importar clientes CSV", accept = ".csv"),

            checkboxInput(
              "substituir_clientes",
              "Substituir lista atual ao importar",
              value = FALSE
            ),

            hr(),

            textInput("cliente_nome_edit", "Cliente"),
            textInput("cliente_email", "Email Principal"),
            textInput("cliente_copias", "Emails Cópia"),
            textInput("cliente_whatsapp", "WhatsApp"),

            checkboxInput("cliente_ativo", "Cliente ativo", value = TRUE),

            textAreaInput(
              "cliente_observacao",
              "Observação",
              rows = 3
            ),

            fluidRow(
              column(6, actionButton("novo_cliente", "Novo")),
              column(6, actionButton("salvar_cliente", "Salvar", class = "btn-primary"))
            ),

            br(),

            actionButton("excluir_cliente", "Excluir Cliente"),

            hr(),

            textAreaInput(
              "cliente_whatsapp_msg",
              "Mensagem WhatsApp",
              value = "Olá, {{cliente_nome}}. Entramos em contato pela {{empresa_nome}}.",
              rows = 4
            ),

            actionButton(
              "enviar_whatsapp_cliente",
              "Enviar WhatsApp"
            ),

            br(), br(),

            verbatimTextOutput("clientes_msg")
          )
        ),

        div(
          class = "coluna-conteudo",
          div(
            class = "painel-conteudo",
            h3("Clientes cadastrados"),
            DTOutput("clientes_tabela")
          )
        )
      )
    ),

    tabPanel(
      "PDFs",

      div(
        class = "tela-padrao",

        div(
          class = "coluna-edicao",
          div(
            class = "painel-formulario",

            h3("Nova Competência"),

            selectInput("pdf_empresa", "Empresa", choices = listar_empresas()),

            textInput(
              "pdf_competencia",
              "Competência dos PDFs",
              value = format(Sys.Date(), "%Y-%m"),
              placeholder = "2026-06"
            ),

            uiOutput("ui_pdf_cliente"),

            fileInput(
              "pdf_arquivos",
              "Enviar PDFs para o cliente",
              accept = ".pdf",
              multiple = TRUE
            ),

            actionButton(
              "salvar_pdfs_cliente",
              "Salvar PDFs",
              class = "btn-primary"
            ),

            hr(),

            fileInput(
              "pdf_zip",
              "Importar ZIP por pastas",
              accept = ".zip"
            ),

            helpText("Formato do ZIP: Nome do Cliente/arquivo.pdf ou 2026-06/Nome do Cliente/arquivo.pdf"),

            actionButton(
              "importar_zip_pdfs",
              "Importar ZIP"
            ),

            hr(),

            numericInput(
              "pdf_reter_meses",
              "Manter últimas competências",
              value = 3,
              min = 1,
              max = 24,
              step = 1
            ),

            checkboxInput(
              "pdf_confirmar_limpeza",
              "Confirmo compactar e remover competências antigas",
              value = FALSE
            ),

            actionButton(
              "limpar_competencias_pdfs",
              "Compactar e Remover Antigas"
            ),

            br(), br(),

            verbatimTextOutput("pdf_msg")
          )
        ),

        div(
          class = "coluna-conteudo",
          div(
            class = "painel-conteudo",

            h3("PDFs da Competência"),

            DTOutput("pdf_tabela")
          )
        )
      )
    ),

    tabPanel(
      "Modelos",

      div(
        class = "tela-padrao",

        div(
          class = "coluna-edicao",
          div(
            class = "painel-formulario",

            h3("Empresa"),

            selectInput("modelos_empresa", "Empresa", choices = listar_empresas()),

            hr(),

            tags$p(class = "help-block", HTML("Chaves dispon&iacute;veis:")),

            tags$ul(
              tags$li("{{cliente_nome}}"),
              tags$li("{{cliente_email}}"),
              tags$li("{{email_principal}}"),
              tags$li("{{cliente_whatsapp}}"),
              tags$li("{{empresa_nome}}"),
              tags$li("{{empresa_whatsapp}}"),
              tags$li("{{mes_referencia}}"),
              tags$li("{{ano_referencia}}"),
              tags$li("{{competencia_pdfs}}")
            ),

            hr(),

            actionButton("salvar_modelos", "Salvar Modelos", class = "btn-primary"),

            br(),
            br(),

            actionButton(
              "visualizar_template",
              "Visualizar no Template"
            ),

            br(), br(),

            verbatimTextOutput("modelos_msg")
          )
        ),

        div(
          class = "coluna-conteudo",
          div(
            class = "painel-conteudo",

            h3("Modelos de Email"),

            textAreaInput(
              "modelo_assunto",
              "Assunto",
              rows = 3,
              width = "100%"
            ),

            textAreaInput(
              "modelo_corpo",
              "Corpo do Email",
              rows = 16,
              width = "100%"
            )
          )
        )
      )
    ),

    tabPanel(
      "Remetentes",

      div(
        class = "tela-padrao",

        div(
          class = "coluna-edicao",
          div(
            class = "painel-formulario",

            h3("Cadastro de Remetentes"),

            fileInput("importar_remetentes", "Importar remetentes CSV", accept = ".csv"),

            checkboxInput(
              "substituir_remetentes",
              "Substituir lista atual ao importar",
              value = FALSE
            ),

            hr(),

            uiOutput("ui_rem_empresa_id"),
            textInput("rem_empresa_nome", "Nome da Empresa"),
            textInput("rem_empresa_whatsapp", "WhatsApp da Empresa"),
            textInput("rem_email", "Email Remetente"),
            textInput("rem_nome", "Nome Remetente"),
            uiOutput("ui_rem_smtp_id"),

            checkboxInput("rem_ativo", "Remetente ativo", value = TRUE),

            fluidRow(
              column(6, actionButton("novo_remetente", "Novo")),
              column(6, actionButton("salvar_remetente", "Salvar", class = "btn-primary"))
            ),

            br(),

            actionButton("excluir_remetente", "Excluir Remetente"),

            br(), br(),

            verbatimTextOutput("remetentes_msg")
          )
        ),

        div(
          class = "coluna-conteudo",
          div(
            class = "painel-conteudo",
            h3("Remetentes cadastrados"),
            DTOutput("remetentes_tabela")
          )
        )
      )
    ),

    tabPanel(
      "SMTP",

      div(
        class = "tela-padrao",

        div(
          class = "coluna-edicao",
          div(
            class = "painel-formulario",

            h3("Configuração SMTP"),

            fileInput("importar_smtp", "Importar SMTP CSV", accept = ".csv"),

            checkboxInput(
              "substituir_smtp",
              "Substituir lista atual ao importar",
              value = FALSE
            ),

            hr(),

            textInput("smtp_id_form", "SMTP ID"),
            textInput("smtp_email_form", "Email"),
            textInput("smtp_host_form", "Host"),
            textInput("smtp_port_form", "Porta"),
            checkboxInput("smtp_ssl_form", "Usar SSL", value = TRUE),
            textInput("smtp_usuario_form", "Usuário"),
            textInput("smtp_observacao_form", "Observação"),
            passwordInput("smtp_senha_form", "Senha"),

            textInput(
              "smtp_email_teste",
              "Email para teste",
              value = ""
            ),

            actionButton(
              "testar_smtp",
              "Testar Envio",
              class = "btn-primary"
            ),

            br(), br(),

            fluidRow(
              column(6, actionButton("novo_smtp", "Novo")),
              column(6, actionButton("salvar_smtp", "Salvar", class = "btn-primary"))
            ),

            br(),

            actionButton("excluir_smtp", "Excluir SMTP"),

            br(), br(),

            verbatimTextOutput("smtp_msg")
          )
        ),

        div(
          class = "coluna-conteudo",
          div(
            class = "painel-conteudo",
            h3("SMTP cadastrados"),
            DTOutput("smtp_tabela")
          )
        )
      )
    ),

    tabPanel(
      "Chatwoot",

      div(
        class = "tela-padrao",

        div(
          class = "coluna-edicao",
          div(
            class = "painel-formulario",

            h3("Configuração Chatwoot"),

            textInput("cw_id", "Chatwoot ID"),
            uiOutput("ui_cw_empresa_id"),
            textInput("cw_base_url", "URL Base"),
            textInput("cw_account_id", "Account ID"),
            textInput("cw_inbox_identifier", "Inbox Identifier"),
            passwordInput("cw_token", "API Access Token"),

            checkboxInput("cw_ativo", "Ativo", value = TRUE),
            checkboxInput("cw_enviar_pdfs", "Enviar PDFs pelo WhatsApp", value = FALSE),

            textAreaInput(
              "cw_msg_email_enviado",
              "Mensagem quando o e-mail for enviado",
              value = "Olá, {{cliente_nome}}. {{empresa_nome}} enviou por e-mail os documentos referentes à competência {{competencia_pdfs}}. Qualquer dúvida, estamos à disposição.",
              rows = 4
            ),

            textAreaInput(
              "cw_msg_email_falha",
              "Mensagem quando o e-mail falhar",
              value = "Olá, {{cliente_nome}}. {{empresa_nome}} está entrando em contato sobre os documentos referentes à competência {{competencia_pdfs}}. Qualquer dúvida, estamos à disposição.",
              rows = 4
            ),

            textAreaInput(
              "cw_observacao",
              "Observação",
              rows = 3
            ),

            fluidRow(
              column(6, actionButton("cw_novo", "Novo")),
              column(6, actionButton("cw_salvar", "Salvar", class = "btn-primary"))
            ),

            br(),

            actionButton("cw_excluir", "Excluir Configuração"),

            hr(),

            textInput(
              "cw_teste_nome",
              "Nome para teste",
              value = "Cliente Teste"
            ),

            textInput(
              "cw_teste_telefone",
              "WhatsApp para teste",
              value = ""
            ),

            textAreaInput(
              "cw_teste_msg",
              "Mensagem de teste",
              value = "Teste de envio via Chatwoot.",
              rows = 4
            ),

            actionButton(
              "cw_testar_envio",
              "Testar Envio",
              class = "btn-primary"
            ),

            br(), br(),

            tags$p(class = "help-block", HTML("Chaves dispon&iacute;veis:")),

            tags$ul(
              tags$li("{{cliente_nome}}"),
              tags$li("{{cliente_email}}"),
              tags$li("{{email_principal}}"),
              tags$li("{{cliente_whatsapp}}"),
              tags$li("{{empresa_nome}}"),
              tags$li("{{empresa_whatsapp}}"),
              tags$li("{{mes_referencia}}"),
              tags$li("{{ano_referencia}}"),
              tags$li("{{competencia_pdfs}}")
            ),

            br(),

            verbatimTextOutput("cw_msg")
          )
        ),

        div(
          class = "coluna-conteudo",
          div(
            class = "painel-conteudo",

            h3("Configurações cadastradas"),

            DTOutput("cw_tabela")
          )
        )
      )
    ),

    tabPanel(
      "Logs",

      div(
        class = "tela-padrao",

        div(
          class = "coluna-edicao",
          div(
            class = "painel-formulario",

            h3("Filtros"),

            selectInput(
              "logs_empresa",
              "Empresa",
              choices = c("Todas", listar_empresas())
            ),

            textInput(
              "logs_competencia",
              "Competência",
              value = ""
            ),

            actionButton("atualizar_logs", "Atualizar Logs"),

            hr(),

            actionButton(
              "reenviar_falhas",
              "Reenviar Falhas",
              class = "btn-primary"
            ),

            br(), br(),

            verbatimTextOutput("logs_msg")
          )
        ),

        div(
          class = "coluna-conteudo",
          div(
            class = "painel-conteudo",

            h3("Histórico de Envios"),

            DTOutput("logs_tabela"),

            br(),

            h3("Histórico WhatsApp"),

            DTOutput("whatsapp_logs_tabela"),

            br(),

            h3("Histórico de Processamento"),

            DTOutput("processamento_logs_tabela")
          )
        )
      )
    )
  )
)
ui_login <- fluidPage(
  tags$head(
    tags$link(
      rel = "icon",
      type = "image/svg+xml",
      href = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 64 64'%3E%3Crect width='64' height='64' rx='12' fill='%231f1f1f'/%3E%3Cpath d='M8 10h8l5 30 7-30h8l7 30 5-30h8L47 54h-8l-7-29-7 29h-8L8 10z' fill='%23ffffff'/%3E%3Crect x='8' y='55' width='48' height='5' fill='%23d40000'/%3E%3C/svg%3E"
    ),
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
      }

      .login-box {
        max-width: 420px;
        margin: 90px auto;
        background: #ffffff;
        padding: 28px;
        border-radius: 10px;
        border-top: 5px solid #d40000;
        box-shadow: 0 2px 10px rgba(0,0,0,0.12);
      }

      .btn-primary {
        background-color: #d40000;
        border-color: #d40000;
      }

      .btn-primary:hover {
        background-color: #a00000;
      }
    ")),

    tags$script(HTML("
  function enviarLogin() {
    if (!window.Shiny) {
      return;
    }

    Shiny.setInputValue(
      'login_payload',
      {
        usuario: $('#login_usuario').val(),
        senha: $('#login_senha').val(),
        nonce: Date.now()
      },
      { priority: 'event' }
    );
  }

  $(document).on('keydown', '#login_usuario, #login_senha', function(e) {
    if (e.key === 'Enter') {
      e.preventDefault();
      enviarLogin();
    }
  });

  $(document).on('click', '#login_entrar', function(e) {
    e.preventDefault();
    enviarLogin();
  });
"))  ),

  div(
    class = "login-box",

    h2("Disparo de Mensagens"),

    textInput("login_usuario", "Usuário"),

    passwordInput("login_senha", "Senha"),

    actionButton(
      "login_entrar",
      "Entrar",
      class = "btn-primary"
    ),

    br(),
    br(),

    verbatimTextOutput("login_msg"),

    helpText("Usuário inicial: admin | Defina APP_ADMIN_PASSWORD no Portainer antes do primeiro deploy")
  )
)

ui <- uiOutput("ui_principal")
