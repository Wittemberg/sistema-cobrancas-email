# CONTEXTO DO PROJETO

## Nome

Sistema de Disparo Inteligente de Mensagens

---

# Objetivo

Sistema web desenvolvido em R + Shiny para gerenciamento e envio automatizado de:

- E-mails com anexos PDF
- Mensagens WhatsApp via Chatwoot
- Templates HTML personalizados por empresa
- Controle de fila de processamento
- Dashboard operacional
- Auditoria completa de envios

O sistema é multiempresa.

Cada empresa possui:

- Clientes próprios
- SMTP próprio
- Remetentes próprios
- Templates próprios
- Integração Chatwoot própria
- PDFs próprios

---

# Stack

## Linguagem

R

## Framework

Shiny

## Pacotes Principais

```r
shiny
DT
dplyr
readr
blastula
glue
htmltools
httr2
jsonlite
fs
```

---

# Estrutura do Projeto

```text
sistema-cobrancas-email/

├── app.R

├── R/
│   ├── 01-global.R
│   ├── 02-ui.R
│   ├── 03-email.R
│   ├── 04-utils.R
│   ├── 05-server.R
│   ├── 06-chatwoot-ui.R
│   └── 07-chatwoot.R

├── logs/
│   ├── envios.csv
│   ├── fila_envio.csv
│   └── whatsapp.csv

├── _config/
│   ├── usuarios.csv
│   ├── remetentes.csv
│   ├── smtp.csv
│   └── chatwoot.csv

└── empresas/
```

---

# Arquitetura

O sistema NÃO utiliza banco de dados.

Toda persistência é feita através de arquivos CSV.

---

# Estrutura das Empresas

Cada empresa possui uma pasta própria.

Exemplo:

```text
awe/

├── destinatarios.csv

├── modelos/
│   ├── assunto.txt
│   ├── corpo_email.txt
│   └── template_html.html

└── PDFs/
    ├── 2026-04/
    ├── 2026-05/
    └── ...
```

---

# Funcionalidades Implementadas

# Login

Autenticação simples.

Arquivo:

```text
_config/usuarios.csv
```

Estrutura:

```csv
usuario,senha,ativo
admin,admin123,TRUE
```

Funcionalidades:

- Login por usuário/senha
- Enter no campo senha executa login
- Controle de sessão

---

# Dashboard

KPIs:

- Empresas
- Clientes Ativos
- Envios OK
- Erros
- Fila Pendente

Tabelas:

- Envios por empresa
- Últimos envios

Origem:

```text
logs/envios.csv
logs/fila_envio.csv
```

---

# Clientes

CRUD completo.

Arquivo:

```text
empresa/destinatarios.csv
```

Campos:

```csv
cliente_nome
email_principal
email_copias
telefone_whatsapp
ativo
observacao
```

Recursos:

- Inclusão
- Edição
- Exclusão
- Importação CSV

Importante:

Ao salvar, a tabela precisa ser atualizada imediatamente.

Problema já identificado anteriormente:
a tela salvava corretamente mas não atualizava visualmente.

---

# Remetentes

Arquivo:

```text
_config/remetentes.csv
```

Estrutura:

```csv
empresa_id
empresa_nome
email_remetente
nome_remetente
smtp_id
ativo
```

Função:

Relacionar empresa ao SMTP utilizado.

---

# SMTP

Arquivo:

```text
_config/smtp.csv
```

Estrutura:

```csv
smtp_id
email
host
port
use_ssl
usuario
senha
observacao
```

Recursos implementados:

- CRUD
- Teste SMTP
- SSL configurável

---

# Modelos

Arquivos:

```text
empresa/modelos/assunto.txt
empresa/modelos/corpo_email.txt
empresa/modelos/template_html.html
```

---

# Placeholders Disponíveis

Os modelos podem utilizar:

```text
{{cliente_nome}}
{{empresa_nome}}
{{mes_referencia}}
{{ano_referencia}}
{{competencia_pdfs}}
{{corpo_email}}
```

---

# Template HTML

Cada empresa possui template próprio.

Exemplo:

```html
<!DOCTYPE html>
<html>
<body>

<div>
{{corpo_email}}
</div>

</body>
</html>
```

---

# Preview do Template

Implementado.

Na aba Modelos existe botão:

```text
Visualizar no Template
```

O preview deve:

- Respeitar a empresa selecionada
- Carregar template da empresa
- Renderizar HTML
- Substituir placeholders

---

# E-mails

Arquivo principal:

```text
R/03-email.R
```

Função principal:

```r
enviar_email_cliente()
```

Responsabilidades:

- Carregar remetente
- Carregar SMTP
- Carregar modelo
- Carregar template HTML
- Substituir placeholders
- Buscar PDFs
- Anexar PDFs
- Enviar e-mail
- Registrar logs

---

# Problemas já Resolvidos

## HTML aparecendo como texto

Errado:

```r
compose_email(
  body = corpo_html
)
```

Correto:

```r
compose_email(
  body = htmltools::HTML(corpo_html)
)
```

---

## Placeholders não substituídos

Foi necessário utilizar:

```r
gsub(..., fixed = TRUE)
```

---

## Variáveis entre chaves

Exemplo:

```text
{Abril}/{2026}
```

Problema ocorreu após uso de placeholders antigos.

Hoje o padrão correto é:

```text
{{mes_referencia}}
{{ano_referencia}}
```

A substituição ocorre antes da montagem do template.

---

# PDFs

Função:

```r
buscar_pdfs_cliente()
```

Objetivo:

Localizar PDFs da competência selecionada.

Exemplo:

```text
empresa/PDFs/2026-05/
```

Retorno:

```r
list(
  arquivos_pdf = ...
)
```

---

# Fila de Envio

Implementada.

Arquivo:

```text
logs/fila_envio.csv
```

Campos:

```csv
empresa
competencia
cliente_nome
email_principal
status
data_inclusao
```

Status:

```text
pendente
enviado
erro
```

---

# Fluxo da Fila

## Gerar Fila

Localiza clientes válidos.

Insere registros em:

```text
logs/fila_envio.csv
```

---

## Processar Fila

Percorre todos os pendentes.

Executa:

```r
enviar_email_cliente()
```

Atualiza status.

---

## Limpar Fila

Remove registros.

---

# Logs

Arquivo:

```text
logs/envios.csv
```

Campos:

```csv
data_hora
empresa
cliente_nome
status
mensagem
```

---

# Chatwoot

Integração validada.

Arquivo:

```text
_config/chatwoot.csv
```

Estrutura:

```csv
chatwoot_id
empresa_id
url_base
account_id
inbox_identifier
api_access_token
ativo
observacao
```

---

# Funcionalidades Chatwoot Implementadas

## CRUD

- Novo
- Salvar
- Excluir

---

## Teste de Envio

Implementado.

Fluxo:

1. Informar telefone
2. Informar mensagem
3. Enviar via API Chatwoot

Resultado:

Mensagem recebida corretamente no Chatwoot.

---

# Configuração Validada

Exemplo:

```text
URL:
https://chat.exemplo.com

Account:
1

Inbox Identifier:
xxxxxxxxxxxxxxxx

Token:
xxxxxxxxxxxxxxxx
```

---

# Situação Atual do Chatwoot

Funcionando.

Já foi validado:

- Autenticação
- Inbox
- API
- Recebimento de mensagens

---

# Próxima Implementação

# WhatsApp por Cliente

Objetivo:

Permitir envio direto para o cliente cadastrado.

Utilizar:

```csv
telefone_whatsapp
```

do cadastro de clientes.

---

# Função Planejada

Arquivo:

```text
R/07-chatwoot.R
```

Função:

```r
enviar_whatsapp_cliente(
    empresa,
    cliente,
    mensagem
)
```

Responsabilidades:

- Carregar configuração Chatwoot
- Ler telefone do cliente
- Enviar mensagem
- Registrar log

---

# Logs WhatsApp

Arquivo:

```text
logs/whatsapp.csv
```

Estrutura planejada:

```csv
data_hora
empresa
cliente_nome
telefone
mensagem
status
erro
```

---

# Evolução Planejada

## Fase 1

Envio WhatsApp individual.

---

## Fase 2

Botão WhatsApp na aba Clientes.

---

## Fase 3

Logs WhatsApp.

---

## Fase 4

KPIs WhatsApp no Dashboard.

---

## Fase 5

Integração automática:

Após envio do e-mail:

```text
E-mail enviado
↓
WhatsApp enviado
↓
Log registrado
```

---

# Regras Técnicas

Sempre usar:

```r
show_col_types = FALSE
```

---

Sempre usar:

```r
col_types = cols(.default = "c")
```

ao carregar CSVs.

Motivo:

Evitar problemas de tipagem automática.

---

Para HTML sempre usar:

```r
htmltools::HTML(...)
```

---

O sistema deve permanecer:

- Multiempresa
- Modular
- Baseado em CSV
- Sem banco de dados
- Compatível com Shiny local
- Compatível com futura migração para PostgreSQL