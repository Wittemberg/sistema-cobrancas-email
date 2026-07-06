# Sistema de Cobrancas por Email e WhatsApp

Aplicacao Shiny em R para operacao de cobrancas com envio de e-mails com PDFs anexos e mensagens WhatsApp via Chatwoot.

O sistema atual e **multiempresa em uma unica instalacao**: uma mesma aplicacao gerencia varias empresas, cada uma com clientes, modelos, PDFs, remetentes, SMTPs e configuracao Chatwoot proprios. Ele ainda nao e um SaaS multi-tenant completo; a evolucao recomendada esta documentada em [DOCUMENTACAO-SAAS.md](DOCUMENTACAO-SAAS.md).

## Stack

- R 4.4.3
- Shiny
- DT
- readr / dplyr / purrr
- blastula
- httr2
- fs
- later
- Docker
- GitHub Actions
- Portainer + Traefik

## Estrutura Principal

```text
.
|-- app.R
|-- run-app.R
|-- Dockerfile
|-- docker-stack.portainer.yml
|-- R/
|   |-- 01-config.R
|   |-- 02-dados.R
|   |-- 03-email.R
|   |-- 04-ui.R
|   |-- 05-server.R
|   |-- 06-auth.R
|   |-- 07-chatwoot.R
|   `-- 08-email-worker.R
|-- _config/
|-- logs/
|-- backups/
|-- awe/
|-- tecnoteam/
`-- wr-tecnologia/
```

## Dados Fora do Git

O repositorio e publico. Dados sensiveis e operacionais ficam fora do Git e devem ser mantidos em volumes persistentes no servidor:

- `_config/*.csv`
- `logs/*.csv`
- `backups/`
- `*/destinatarios*.csv`
- `*/clientes/`

Esses dados sao montados no container pela stack Portainer.

## Pastas por Empresa

Cada empresa e identificada pelo nome da pasta na raiz da aplicacao.

```text
empresa/
|-- destinatarios.csv
|-- modelos/
|   |-- assunto.txt
|   |-- corpo_email.txt
|   `-- template_html.html
`-- clientes/
    `-- 2026-06/
        `-- Nome do Cliente/
            |-- arquivo1.pdf
            `-- arquivo2.pdf
```

As telas que usam o campo `Empresa` listam as pastas que contem `modelos/` ou `destinatarios.csv`.

## Configuracoes Globais

Os CSVs de configuracao ficam em `_config/`.

### usuarios.csv

```csv
usuario,senha,ativo
admin,admin123,TRUE
```

Observacao: a autenticacao atual e simples e usa senha em texto. Para uso SaaS, isso precisa migrar para senha com hash e permissao por tenant.

### smtp.csv

```csv
smtp_id,email,host,port,use_ssl,usuario,senha,observacao
smtp_awe_comercial,comercial@awecloudsolution.com,smtp.titan.email,465,TRUE,comercial@awecloudsolution.com,senha,
```

### remetentes.csv

```csv
empresa_id,empresa_nome,empresa_whatsapp,email_remetente,nome_remetente,smtp_id,ativo
awe,AWE Cloud Solution,27999999999,comercial@awecloudsolution.com,AWE,smtp_awe_comercial,TRUE
```

### chatwoot.csv

```csv
chatwoot_id,empresa_id,base_url,account_id,inbox_identifier,inbox_id,metodo_envio,api_access_token,enviar_pdfs_whatsapp,mensagem_email_enviado,mensagem_email_falha,ativo,observacao
```

### smtp_rotacao.csv

Arquivo criado em runtime para controlar a rotacao de remetentes/SMTPs por empresa.

```csv
empresa_id,contador_envios
awe,180
```

## Funcionalidades

### Login

- Login por usuario e senha.
- Enter no campo de senha executa login.
- Usuario inicial criado por `APP_ADMIN_USER` e `APP_ADMIN_PASSWORD` quando `_config/usuarios.csv` ainda nao existe.

### Dashboard

Exibe:

- empresas
- clientes ativos
- envios OK
- erros
- fila pendente
- WhatsApp OK
- WhatsApp erros
- versao da imagem
- status do worker de e-mail

### Clientes

Permite:

- cadastrar clientes
- editar clientes
- excluir clientes
- importar CSV
- substituir lista ao importar
- enviar WhatsApp manual para o cliente selecionado

Campos principais:

```csv
cliente_nome,email_principal,email_copias,telefone_whatsapp,ativo,observacao
```

`email_copias` aceita multiplos e-mails separados por `;`, `,`, espaco ou quebra de linha.

### PDFs

Permite:

- criar competencia no formato `AAAA-MM`
- enviar PDFs para um cliente
- importar ZIP por pastas
- compactar e remover competencias antigas

Formato do ZIP:

```text
Nome do Cliente/arquivo.pdf
```

Os arquivos sao gravados em:

```text
empresa/clientes/competencia/cliente/
```

Ao gerar fila ou disparo, a pasta do cliente precisa corresponder ao nome do cadastro apos normalizacao de acentos, pontuacao e espacos. Tambem sao aceitas pastas que comecem pelo nome do cliente e tenham um sufixo, como `CLIENTE-907...-Inter_Empresas`. O sistema nao associa mais um cliente a uma pasta apenas por semelhanca, para evitar envios indevidos quando o ZIP contem menos clientes do que a base cadastrada.

Pastas que sobrarem sem associacao aparecem na aba PDFs em `Pastas sem cliente associado`. O operador pode associar manualmente a pasta ao cliente correto e, se desejar, adicionar o item diretamente a fila. Essa associacao tambem move/mescla os PDFs para a pasta canonica do cliente na competencia atual. O vinculo fica gravado em `_config/pdf_aliases.csv` e passa a ser reutilizado nas proximas importacoes quando a pasta tiver o mesmo padrao de nome.

Ao remover competencias antigas, o sistema cria backup ZIP individual em:

```text
backups/pdfs/empresa/competencia.zip
```

### Modelos

Cada empresa possui:

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

### Remetentes

Relaciona empresa, remetente e SMTP.

O campo `SMTP ID` usa lista fechada baseada nos SMTPs cadastrados.

### SMTP

Permite:

- cadastrar SMTP
- editar SMTP
- excluir SMTP
- importar CSV
- testar SMTP

### Disparo

Processa clientes aptos para a empresa e competencia selecionadas.

Regras:

- cliente precisa ter PDFs na competencia
- cliente sem e-mail principal gera erro de e-mail, mas o fluxo pode seguir para WhatsApp se houver telefone
- WhatsApp pode ser enviado como complemento/fallback
- o processamento e assíncrono com `later`

### Fila

Arquivo:

```text
logs/fila_envio.csv
```

Status:

- `pendente`
- `processando`
- `enviado`
- `erro`

Funcionalidades:

- gerar fila
- processar fila
- reprocessar erros
- limpar fila
- editar cliente, pasta PDF e status de um item selecionado
- excluir um item selecionado
- limpar logs antes de processar
- filtrar status exibidos

### Envio de E-mail

Arquivo principal:

```text
R/03-email.R
```

O envio usa subprocesso por cliente via:

```text
R/08-email-worker.R
```

Isso evita travar a interface Shiny quando o SMTP demora ou falha.

Recursos atuais:

- timeout de SMTP
- rotacao de remetentes em blocos de 10
- retry com outro SMTP ativo da mesma empresa em caso de falha
- logs de tentativas SMTP
- suporte a copias multiplas
- anexos PDF por cliente

Eventos relevantes no log de processamento:

- `email_smtp_subprocesso_inicio`
- `email_smtp_subprocesso_ok`
- `email_smtp_subprocesso_erro`
- `email_smtp_send_inicio`
- `email_smtp_send_erro`
- `email_smtp_retry`
- `email_smtp_send_ok`

### WhatsApp / Chatwoot

Arquivo principal:

```text
R/07-chatwoot.R
```

Metodos de envio:

- `public_api`: metodo original, usa `/public/api/v1/inboxes/{inbox_identifier}`.
- `account_api`: usa `/api/v1/accounts/{account_id}` e envia como `message_type = outgoing`; exige `account_id` e `inbox_id` numerico.

Recursos:

- configuracao por empresa
- mensagens parametrizadas
- mensagem para e-mail enviado
- mensagem para falha de e-mail
- envio manual na tela Clientes
- envio automatico no Disparo/Fila
- anexar PDFs no WhatsApp quando habilitado
- timeout nas chamadas Chatwoot
- log de envios em `logs/whatsapp.csv`

Variavel opcional:

```text
CHATWOOT_TIMEOUT_SECONDS=60
```

### Logs

Arquivos:

```text
logs/envios.csv
logs/whatsapp.csv
logs/processamento.csv
logs/fila_envio.csv
```

A tela Logs possui filtros por:

- empresa
- competencia
- cliente
- status
- origem
- tipo de erro
- resultado por cliente
- apenas problemas
- ultimo resultado por cliente

Tambem possui:

- historico de e-mails
- resumo por cliente
- historico WhatsApp
- historico de processamento
- reenvio de falhas
- reprocessamento de erros da fila
- limpeza de logs com confirmacao

## Deploy

O deploy atual usa:

- GitHub Actions para build/push da imagem
- GHCR: `ghcr.io/wittemberg/sistema-cobrancas-email`
- Portainer webhook para redeploy
- Traefik em Docker Swarm
- dominio `cobrancas.wrtec.com.br`

Detalhes em [DEPLOY-PORTAINER.md](DEPLOY-PORTAINER.md).

## Evolucao Para SaaS

Estado atual:

- multiempresa em uma unica instalacao
- configuracoes globais
- usuarios globais
- logs globais
- fila global
- storage baseado em pastas

Para SaaS multi-tenant real, sera necessario criar isolamento por tenant, usuario por tenant, permissoes, dados separados e preferencialmente migrar para banco de dados.

Plano recomendado em [DOCUMENTACAO-SAAS.md](DOCUMENTACAO-SAAS.md).
