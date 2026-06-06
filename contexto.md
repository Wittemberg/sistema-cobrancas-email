# Contexto do Projeto

## Nome

Sistema de Cobrancas por Email e WhatsApp

## Objetivo

Aplicacao web em R + Shiny para gestao operacional de cobrancas com:

- envio de e-mails com PDFs anexos
- envio de mensagens WhatsApp via Chatwoot
- templates HTML por empresa
- cadastro de clientes
- cadastro de SMTPs e remetentes
- fila de envio
- reprocessamento de erros
- auditoria de e-mails, WhatsApp e processamento

## Estado Atual

O sistema e **multiempresa em uma unica instalacao**.

Cada empresa possui:

- clientes proprios
- PDFs proprios
- modelos proprios
- remetentes proprios
- configuracao Chatwoot propria

Porem, ainda existem recursos globais:

- usuarios
- logs
- fila
- configuracoes SMTP/remetentes/Chatwoot em CSVs globais
- backups

Por isso, o projeto ainda nao e SaaS multi-tenant completo.

## Stack

```text
R 4.4.3
Shiny
shinythemes
DT
readr
dplyr
stringr
stringdist
stringi
purrr
glue
htmltools
fs
blastula
httr2
jsonlite
tibble
curl
later
Docker
GitHub Actions
Portainer
Traefik
```

## Arquivos R

```text
R/01-config.R        Configuracao global, pacotes e pastas
R/02-dados.R         Funcoes de dados, empresas, PDFs e backups
R/03-email.R         Envio de e-mail, SMTP, rotacao, retry e WhatsApp pos-email
R/04-ui.R            Interface Shiny
R/05-server.R        Regras de negocio e eventos da interface
R/06-auth.R          Login simples
R/07-chatwoot.R      Chatwoot e WhatsApp
R/08-email-worker.R  Worker de e-mail por subprocesso
```

## Persistencia

O sistema atual nao usa banco de dados.

Persistencia em CSV e arquivos:

```text
_config/
logs/
backups/
empresa/destinatarios.csv
empresa/modelos/
empresa/clientes/
```

## Estrutura de Empresa

```text
empresa/
|-- destinatarios.csv
|-- modelos/
|   |-- assunto.txt
|   |-- corpo_email.txt
|   `-- template_html.html
`-- clientes/
    `-- 2026-06/
        `-- Cliente/
            `-- arquivo.pdf
```

## Login

Arquivo:

```text
_config/usuarios.csv
```

Campos:

```csv
usuario,senha,ativo
```

Variaveis para usuario inicial:

```text
APP_ADMIN_USER
APP_ADMIN_PASSWORD
```

Limitacao atual:

- senha em texto
- sem perfil de permissao
- sem tenant por usuario

## Clientes

Arquivo:

```text
empresa/destinatarios.csv
```

Campos:

```csv
cliente_nome,email_principal,email_copias,telefone_whatsapp,ativo,observacao
```

Recursos:

- cadastro
- edicao
- exclusao
- importacao CSV
- envio WhatsApp manual
- mascara de telefone na tela
- preservacao de quantidade de linhas na tabela

## PDFs

Estrutura:

```text
empresa/clientes/competencia/cliente/
```

Recursos:

- upload direto por cliente
- importacao ZIP por pastas
- listagem por competencia
- backup ZIP antes de remover competencias antigas
- remocao mantendo ultimas X competencias

## Modelos

Arquivos:

```text
empresa/modelos/assunto.txt
empresa/modelos/corpo_email.txt
empresa/modelos/template_html.html
```

Chaves disponiveis:

```text
{{cliente_nome}}
{{cliente_email}}
{{email_principal}}
{{cliente_whatsapp}}
{{empresa_nome}}
{{empresa_whatsapp}}
{{mes_referencia}}
{{ano_referencia}}
{{competencia_pdfs}}
{{corpo_email}}
```

## SMTP e Remetentes

Arquivos:

```text
_config/smtp.csv
_config/remetentes.csv
_config/smtp_rotacao.csv
```

Recursos:

- cadastro de SMTP
- teste SMTP
- remetente vinculado a empresa
- `SMTP ID` como lista fechada na tela Remetentes
- rotacao de SMTP/remetente em blocos de 10
- retry com outro SMTP ativo da empresa quando o envio falha

Eventos de processamento:

```text
email_smtp_subprocesso_inicio
email_smtp_subprocesso_ok
email_smtp_subprocesso_erro
email_smtp_send_inicio
email_smtp_send_erro
email_smtp_retry
email_smtp_send_ok
```

## Envio de E-mail

Funcao principal:

```r
enviar_email_cliente()
```

O envio em interface usa subprocesso:

```text
R/08-email-worker.R
```

Motivo:

- evitar travamento da interface
- aplicar timeout
- isolar falhas SMTP

## WhatsApp / Chatwoot

Arquivo:

```text
R/07-chatwoot.R
```

Arquivo de configuracao:

```text
_config/chatwoot.csv
```

Recursos:

- configuracao por empresa
- teste de envio
- envio manual por cliente
- envio automatico apos tentativa de e-mail
- mensagem para e-mail enviado
- mensagem para e-mail com falha
- envio de PDFs pelo WhatsApp quando habilitado
- timeout por `CHATWOOT_TIMEOUT_SECONDS`
- log em `logs/whatsapp.csv`

## Fila

Arquivo:

```text
logs/fila_envio.csv
```

Campos:

```csv
empresa,competencia,cliente_nome,email_principal,total_pdfs,status,data_inclusao
```

Status:

```text
pendente
processando
enviado
erro
```

Recursos:

- gerar fila
- processar fila
- reprocessar erros
- limpar fila
- filtrar status exibidos
- limpar logs antes de processar

## Logs

Arquivos:

```text
logs/envios.csv
logs/whatsapp.csv
logs/processamento.csv
logs/fila_envio.csv
```

Tela Logs:

- filtro por empresa
- filtro por competencia
- filtro por cliente
- filtro por status
- filtro por origem
- filtro por tipo de erro
- filtro por resultado por cliente
- filtro apenas problemas
- filtro ultimo resultado por cliente
- resumo por cliente
- reenvio de falhas
- reprocessamento de erros da fila
- limpeza de logs com confirmacao

## Deploy

Dominio atual:

```text
cobrancas.wrtec.com.br
```

Imagem:

```text
ghcr.io/wittemberg/sistema-cobrancas-email
```

Stack:

```text
docker-stack.portainer.yml
```

Workflow:

```text
.github/workflows/docker-portainer.yml
```

## Limitacoes Atuais

- sem banco de dados
- CSVs globais
- fila global
- logs globais
- usuarios globais
- sem isolamento real por tenant
- senhas em texto
- dados operacionais montados por empresa fixa na stack
- Dockerfile ainda copia pastas de empresas de exemplo/runtime

## Direcao Tecnica

Curto prazo:

- manter multiempresa por filesystem
- melhorar seguranca da autenticacao
- criar camada de caminho por tenant
- remover empresas fixas do Dockerfile/stack

Medio prazo:

- estrutura `data/tenants/<tenant_id>/`
- usuarios vinculados a tenant
- logs e fila por tenant
- painel admin de tenants

Longo prazo:

- PostgreSQL
- storage S3/MinIO para PDFs
- worker separado da UI
- billing/plano/limites
- subdominio por tenant

Ver [DOCUMENTACAO-SAAS.md](DOCUMENTACAO-SAAS.md).
