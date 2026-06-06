# Deploy Portainer WRTEC

Este documento descreve o deploy atual da aplicacao no Portainer/Traefik.

## Arquivos Envolvidos

- `Dockerfile`
- `docker-stack.portainer.yml`
- `.github/workflows/docker-portainer.yml`

## Imagem Docker

Imagem publicada no GHCR:

```text
ghcr.io/wittemberg/sistema-cobrancas-email:latest
```

O workflow tambem publica uma tag com o SHA do commit.

## Build e Deploy Automatico

Workflow:

```text
.github/workflows/docker-portainer.yml
```

Fluxo:

1. push na branch `main`
2. build da imagem Docker
3. push para GHCR
4. chamada opcional do webhook do Portainer

O webhook do Portainer so e chamado quando:

- o secret `ENABLE_PORTAINER_DEPLOY` esta com valor `true`; ou
- o workflow manual e executado com `trigger_portainer=true`.

## Secrets do GitHub

Crie no repositorio:

```text
PORTAINER_WEBHOOK_URL
ENABLE_PORTAINER_DEPLOY
```

Valores:

- `PORTAINER_WEBHOOK_URL`: URL do webhook da stack no Portainer.
- `ENABLE_PORTAINER_DEPLOY`: use `true` para ativar deploy automatico apos push na `main`.

## Stack Portainer

Arquivo:

```text
docker-stack.portainer.yml
```

Servico:

```text
cobrancas-email
```

Dominio:

```text
cobrancas.wrtec.com.br
```

Rede externa:

```text
interna
```

Porta interna:

```text
3838
```

## Traefik

Labels principais:

```yaml
traefik.enable=true
traefik.docker.network=interna
traefik.http.routers.cobrancas-email.rule=Host(`cobrancas.wrtec.com.br`)
traefik.http.routers.cobrancas-email.entrypoints=websecure
traefik.http.routers.cobrancas-email.tls=true
traefik.http.routers.cobrancas-email.tls.certresolver=letsencryptresolver
traefik.http.services.cobrancas-email.loadbalancer.server.port=3838
```

## Variaveis da Stack

Obrigatorias/recomendadas:

```text
TZ=America/Sao_Paulo
APP_ADMIN_USER=admin
APP_ADMIN_PASSWORD=<senha_inicial>
CHATWOOT_TIMEOUT_SECONDS=60
```

`APP_ADMIN_PASSWORD` e obrigatoria no primeiro deploy quando `_config/usuarios.csv` ainda nao existe.

## Volumes Persistentes

A aplicacao usa CSVs e PDFs. Por isso, a stack monta bind mounts em `/root/sistema-cobrancas-email`.

Montagens atuais:

```yaml
- /root/sistema-cobrancas-email/config:/srv/shiny-server/_config
- /root/sistema-cobrancas-email/logs:/srv/shiny-server/logs
- /root/sistema-cobrancas-email/backups:/srv/shiny-server/backups
- /root/sistema-cobrancas-email/awe:/srv/shiny-server/awe
- /root/sistema-cobrancas-email/tecnoteam:/srv/shiny-server/tecnoteam
- /root/sistema-cobrancas-email/wr-tecnologia:/srv/shiny-server/wr-tecnologia
```

Crie antes do deploy:

```bash
mkdir -p /root/sistema-cobrancas-email/{config,logs,backups,awe,tecnoteam,wr-tecnologia}
mkdir -p /root/sistema-cobrancas-email/{awe,tecnoteam,wr-tecnologia}/modelos
mkdir -p /root/sistema-cobrancas-email/{awe,tecnoteam,wr-tecnologia}/clientes
```

Copie para essas pastas:

- `config/*.csv`
- `empresa/destinatarios.csv`
- `empresa/modelos/*`
- `empresa/clientes/*`

## GHCR no Portainer

A stack usa imagem privada/publicada no GHCR. Se o repositorio/imagem estiver privado, configure o registry `ghcr.io` no Portainer.

## Atualizacao Manual

No Portainer:

1. abra a stack `sistema-cobrancas-email`
2. use `Pull and redeploy`
3. mantenha `Re-pull image` ligado
4. se necessario, habilite `Force redeployment`

## Limpeza e Backup

Backups da aplicacao sao gravados em:

```text
/root/sistema-cobrancas-email/backups
```

Antes de limpar logs pelo app, a rotina chama backup seguro.

## Observacao Para SaaS

Para SaaS multi-tenant, a stack deve mudar. As montagens por empresa fixa (`awe`, `tecnoteam`, `wr-tecnologia`) nao escalam.

Modelo recomendado:

```yaml
- /root/sistema-cobrancas-email/data:/srv/shiny-server/data
```

E a aplicacao passaria a gravar dados em:

```text
data/tenants/<tenant_id>/
```

Detalhes em [DOCUMENTACAO-SAAS.md](DOCUMENTACAO-SAAS.md).
