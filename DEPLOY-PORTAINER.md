# Deploy Portainer WRTEC

## Stack

Use o arquivo `docker-stack.portainer.yml` no Portainer.

A stack foi preparada para o Traefik informado:

- rede externa: `interna`
- entrypoint HTTPS: `websecure`
- certresolver: `letsencryptresolver`
- dominio: `cobranca.wrtec.com.br`
- porta interna da aplicacao: `3838`

## Volumes persistentes

A aplicacao usa arquivos CSV e PDFs. Os volumes abaixo preservam configuracoes e dados entre atualizacoes:

- `cobrancas_config` em `/srv/app/_config`
- `cobrancas_logs` em `/srv/app/logs`
- `cobrancas_backups` em `/srv/app/backups`
- `cobrancas_awe` em `/srv/app/awe`
- `cobrancas_tecnoteam` em `/srv/app/tecnoteam`
- `cobrancas_wr_tecnologia` em `/srv/app/wr-tecnologia`

## GitHub Actions

O workflow `.github/workflows/docker-portainer.yml` publica a imagem no GHCR a cada push na branch `main`.

O webhook do Portainer so e chamado quando uma destas condicoes for verdadeira:

- variavel do repositorio `ENABLE_PORTAINER_DEPLOY=true` em pushes na `main`;
- execucao manual do workflow com `trigger_portainer=true`.

## Secrets e variaveis

Crie no repositorio GitHub:

- Secret `PORTAINER_WEBHOOK_URL`: URL do webhook da stack no Portainer.
- Variable `ENABLE_PORTAINER_DEPLOY`: use `false` ou nao crie enquanto o DNS nao estiver pronto; altere para `true` quando quiser deploy automatico.

Na stack Portainer, defina antes do primeiro deploy:

- `APP_ADMIN_USER`: usuario inicial, padrao `admin`.
- `APP_ADMIN_PASSWORD`: senha inicial gravada em `_config/usuarios.csv` quando o volume ainda estiver vazio.
