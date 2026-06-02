# Sistema de Disparo Inteligente de Mensagens

Aplicacao Shiny em R para disparo operacional de e-mails com PDFs e mensagens via Chatwoot, com persistencia em CSV por empresa.

## Publicacao

O projeto esta preparado para:

- construir a imagem Docker no GitHub Actions;
- publicar em `ghcr.io/wittemberg/sistema-cobrancas-email`;
- atualizar a stack no Portainer via webhook;
- responder em `https://cobranca.wrtec.com.br` usando Traefik na rede Swarm `interna`.

## Dados operacionais

O repositorio e publico. Por seguranca, arquivos com credenciais, destinatarios, logs, backups e PDFs de clientes ficam fora do Git:

- `_config/*.csv`
- `logs/*.csv`
- `backups/`
- `*/destinatarios*.csv`
- `*/clientes/`

Esses dados devem ser mantidos nos volumes da stack Portainer.

## Deploy no Portainer

1. Crie o registro DNS `cobranca.wrtec.com.br` apontando para o servidor do Traefik.
2. Defina `APP_ADMIN_PASSWORD` nas variaveis da stack antes do primeiro deploy, ou carregue seu `_config/usuarios.csv` no volume.
3. Crie uma stack no Portainer usando `docker-stack.portainer.yml`.
4. Habilite o webhook da stack no Portainer.
5. No GitHub, crie o secret `PORTAINER_WEBHOOK_URL` com a URL do webhook.
6. Quando quiser ativar deploy automatico em push na `main`, crie a variavel `ENABLE_PORTAINER_DEPLOY` com valor `true`.

Enquanto `ENABLE_PORTAINER_DEPLOY` nao estiver como `true`, o Actions so constroi e publica a imagem.
