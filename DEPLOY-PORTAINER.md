# Deploy Portainer WRTEC

## Stack

Use o arquivo `docker-stack.portainer.yml` no Portainer.

A stack foi preparada para o Traefik informado:

- rede externa: `interna`
- entrypoint HTTPS: `websecure`
- certresolver: `letsencryptresolver`
- dominio: `cobrancas.wrtec.com.br`
- porta interna da aplicacao: `3838`

## Pastas persistentes

A aplicacao usa arquivos CSV e PDFs. A stack usa bind mounts em `/root/sistema-cobrancas-email`, facilitando backup e copia manual dos dados:

- `/root/sistema-cobrancas-email/config` em `/srv/shiny-server/_config`
- `/root/sistema-cobrancas-email/logs` em `/srv/shiny-server/logs`
- `/root/sistema-cobrancas-email/backups` em `/srv/shiny-server/backups`
- `/root/sistema-cobrancas-email/awe` em `/srv/shiny-server/awe`
- `/root/sistema-cobrancas-email/tecnoteam` em `/srv/shiny-server/tecnoteam`
- `/root/sistema-cobrancas-email/wr-tecnologia` em `/srv/shiny-server/wr-tecnologia`

Crie antes do deploy:

```bash
mkdir -p /root/sistema-cobrancas-email/{config,logs,backups,awe,tecnoteam,wr-tecnologia}
mkdir -p /root/sistema-cobrancas-email/{awe,tecnoteam,wr-tecnologia}/modelos
mkdir -p /root/sistema-cobrancas-email/{awe,tecnoteam,wr-tecnologia}/clientes
```

Copie para essas pastas os arquivos que nao entram no Git, especialmente `config/*.csv`, `destinatarios.csv`, `modelos/*` e `clientes/*`.

## GitHub Actions

O workflow `.github/workflows/docker-portainer.yml` publica a imagem no GHCR a cada push na branch `main`.

O webhook do Portainer so e chamado quando uma destas condicoes for verdadeira:

- variavel do repositorio `ENABLE_PORTAINER_DEPLOY=true` em pushes na `main`;
- execucao manual do workflow com `trigger_portainer=true`.

## Secrets e variaveis

Crie no repositorio GitHub:

- Secret `PORTAINER_WEBHOOK_URL`: URL do webhook da stack no Portainer.
- Secret `ENABLE_PORTAINER_DEPLOY`: use `false` ou nao crie enquanto o deploy automatico nao deve rodar; altere para `true` quando quiser atualizar a stack a cada push na `main`.

Na stack Portainer, defina antes do primeiro deploy:

- `APP_ADMIN_USER`: usuario inicial, padrao `admin`.
- `APP_ADMIN_PASSWORD`: senha inicial gravada em `_config/usuarios.csv` quando o volume ainda estiver vazio.
