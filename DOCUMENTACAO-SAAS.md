# Documentacao SaaS Multi-Tenant

Este documento descreve o que precisa mudar para transformar o sistema atual em um SaaS multi-tenant.

## Diagnostico

O projeto atual e multiempresa, mas nao multi-tenant.

Multiempresa atual:

- uma instalacao
- uma base de usuarios
- uma pasta por empresa
- configuracoes globais
- fila global
- logs globais

SaaS multi-tenant esperado:

- varios tenants isolados
- usuarios associados a tenants
- dados filtrados por tenant
- logs e filas separados por tenant
- permissoes por papel
- possibilidade de subdominio ou dominio por tenant
- plano/limites por tenant

## Conceitos

### Tenant

Representa uma organizacao cliente do SaaS.

Campos sugeridos:

```text
tenant_id
nome
slug
dominio
plano
ativo
data_criacao
```

### Empresa

No sistema atual, empresa e a unidade operacional que possui clientes, modelos, SMTP, Chatwoot e PDFs.

Em SaaS, empresa deve pertencer a um tenant:

```text
tenant_id
empresa_id
empresa_nome
empresa_whatsapp
ativo
```

Um tenant pode ter uma ou mais empresas.

### Usuario

Campos sugeridos:

```text
usuario_id
tenant_id
nome
email
senha_hash
perfil
ativo
ultimo_login
```

Perfis sugeridos:

- `super_admin`
- `tenant_admin`
- `operador`
- `leitura`

## Estrutura de Dados Recomendada

### Fase 1: filesystem multi-tenant

Mais rapida e menos disruptiva.

```text
data/
|-- tenants/
|   |-- tenant_a/
|   |   |-- _config/
|   |   |-- logs/
|   |   |-- backups/
|   |   `-- empresas/
|   |       `-- awe/
|   |           |-- destinatarios.csv
|   |           |-- modelos/
|   |           `-- clientes/
|   `-- tenant_b/
`-- global/
    |-- tenants.csv
    `-- usuarios.csv
```

Vantagens:

- aproveita grande parte do codigo atual
- reduz risco de migracao
- permite validar o produto SaaS antes do banco

Desvantagens:

- concorrencia limitada
- auditoria limitada
- consultas e filtros menos eficientes
- backup por arquivo exige disciplina

### Fase 2: PostgreSQL + storage

Modelo recomendado para SaaS definitivo.

PostgreSQL:

- tenants
- usuarios
- empresas
- clientes
- smtp
- remetentes
- chatwoot_config
- competencias
- fila_envio
- envios
- whatsapp_envios
- processamento_logs

Storage local/S3/MinIO:

- PDFs
- backups
- anexos
- templates HTML se preferir versionar como arquivo

## Mudancas Necessarias no Codigo

### 1. Criar contexto de tenant

Hoje o sistema usa `pasta_raiz` global.

Criar funcoes:

```r
tenant_atual <- function(session) {
  session$userData$tenant_id
}

caminho_tenant <- function(tenant_id, ...) {
  file.path(pasta_raiz, "data", "tenants", tenant_id, ...)
}

caminho_empresa <- function(tenant_id, empresa_id, ...) {
  caminho_tenant(tenant_id, "empresas", empresa_id, ...)
}
```

### 2. Alterar login

Arquivo atual:

```text
R/06-auth.R
```

Mudancas:

- validar usuario por email/senha
- carregar `tenant_id` e perfil na sessao
- armazenar senha com hash
- impedir login de usuario/tenant inativo

### 3. Filtrar empresas por tenant

Hoje:

```r
listar_empresas()
```

Precisa virar:

```r
listar_empresas(tenant_id)
```

Todas as telas devem chamar usando o tenant da sessao.

### 4. Alterar caminhos de clientes, PDFs e modelos

Hoje:

```text
empresa/destinatarios.csv
empresa/modelos/
empresa/clientes/
```

Novo:

```text
data/tenants/<tenant_id>/empresas/<empresa_id>/destinatarios.csv
data/tenants/<tenant_id>/empresas/<empresa_id>/modelos/
data/tenants/<tenant_id>/empresas/<empresa_id>/clientes/
```

### 5. Separar configuracoes por tenant

Hoje:

```text
_config/smtp.csv
_config/remetentes.csv
_config/chatwoot.csv
_config/smtp_rotacao.csv
```

Novo:

```text
data/tenants/<tenant_id>/_config/smtp.csv
data/tenants/<tenant_id>/_config/remetentes.csv
data/tenants/<tenant_id>/_config/chatwoot.csv
data/tenants/<tenant_id>/_config/smtp_rotacao.csv
```

### 6. Separar logs e fila por tenant

Hoje:

```text
logs/envios.csv
logs/whatsapp.csv
logs/processamento.csv
logs/fila_envio.csv
```

Novo:

```text
data/tenants/<tenant_id>/logs/envios.csv
data/tenants/<tenant_id>/logs/whatsapp.csv
data/tenants/<tenant_id>/logs/processamento.csv
data/tenants/<tenant_id>/logs/fila_envio.csv
```

### 7. Controle de permissoes

Sugestao:

- `super_admin`: gerencia todos os tenants
- `tenant_admin`: gerencia empresas, usuarios e configuracoes do tenant
- `operador`: clientes, PDFs, disparos e logs
- `leitura`: dashboard e logs

### 8. Remover empresas fixas da imagem Docker

Hoje o Dockerfile copia:

```dockerfile
COPY awe ./awe
COPY tecnoteam ./tecnoteam
COPY wr-tecnologia ./wr-tecnologia
```

Para SaaS, remover essas linhas e usar somente volume persistente:

```text
/srv/shiny-server/data
```

### 9. Ajustar stack Portainer

Trocar montagens por empresa fixa por:

```yaml
- /root/sistema-cobrancas-email/data:/srv/shiny-server/data
```

### 10. Provisionamento de tenant

Criar tela ou rotina para:

1. criar tenant
2. criar usuario admin do tenant
3. criar estrutura de pastas
4. criar empresa inicial
5. copiar modelos padrao
6. inicializar CSVs vazios

## Modelo de Banco Recomendado

Quando migrar para PostgreSQL, tabelas principais:

```text
tenants
usuarios
empresas
clientes
smtp_contas
remetentes
chatwoot_configs
competencias
pdfs
fila_envio
envios_email
envios_whatsapp
processamento_logs
planos
limites_tenant
```

## Limites por Plano

Possiveis limites:

- empresas por tenant
- usuarios por tenant
- clientes por tenant
- envios de e-mail por dia
- envios WhatsApp por dia
- armazenamento de PDFs
- retencao de logs
- retencao de backups

## Subdominio por Tenant

Opcoes:

1. `app.wrtec.com.br` com tenant selecionado apos login
2. `<tenant>.wrtec.com.br`
3. dominio proprio do cliente

Para subdominio:

- Traefik precisa aceitar wildcard ou rotas por tenant
- app precisa identificar tenant pelo `Host`
- login deve validar usuario naquele tenant

## Ordem Recomendada de Implementacao

1. Criar `data/tenants` e funcoes de caminho.
2. Criar `tenants.csv` global.
3. Alterar usuarios para incluir `tenant_id` e `perfil`.
4. Carregar tenant na sessao apos login.
5. Alterar `listar_empresas`, clientes, PDFs e modelos para usar tenant.
6. Mover `_config` para dentro do tenant.
7. Mover logs e fila para dentro do tenant.
8. Ajustar Dockerfile e stack para volume unico `data`.
9. Criar tela admin de tenants.
10. Migrar para PostgreSQL.
11. Separar worker de envio da UI.

## Riscos

- Misturar dados entre tenants se alguma funcao continuar usando caminho global.
- Concorrencia em CSV se muitos usuarios/processamentos rodarem ao mesmo tempo.
- Logs antigos podem ficar fora do tenant se a migracao nao for completa.
- Tokens SMTP/Chatwoot precisam ser protegidos.
- Subdominio por tenant exige decisao de DNS/Traefik.

## Decisao Recomendada

Para reduzir risco:

1. Fazer primeiro uma versao filesystem multi-tenant.
2. Validar operacao com 2 ou 3 tenants.
3. Migrar entidades criticas para PostgreSQL.
4. Depois implementar billing/planos e worker dedicado.
