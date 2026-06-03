# Sistema de Disparo Inteligente de Mensagens

Aplicacao Shiny em R para disparo operacional de e-mails com PDFs e mensagens via Chatwoot, com persistencia em CSV por empresa.

## Publicacao

O projeto esta preparado para:

- construir a imagem Docker no GitHub Actions;
- publicar em `ghcr.io/wittemberg/sistema-cobrancas-email`;
- atualizar a stack no Portainer via webhook;
- responder em `https://cobrancas.wrtec.com.br` usando Traefik na rede Swarm `interna`.

## Dados operacionais

O repositorio e publico. Por seguranca, arquivos com credenciais, destinatarios, logs, backups e PDFs de clientes ficam fora do Git:

- `_config/*.csv`
- `logs/*.csv`
- `backups/`
- `*/destinatarios*.csv`
- `*/clientes/`

Esses dados devem ser mantidos nas pastas persistentes do servidor em `/root/sistema-cobrancas-email`.

## Upload de PDFs

A aba `PDFs` permite criar uma nova competência no formato `AAAA-MM`, como `2026-06`.

Os PDFs podem ser enviados de duas formas:

- upload direto de um ou mais PDFs para um cliente selecionado;
- importação de ZIP em lote, com o formato `Nome do Cliente/arquivo.pdf`.

Os arquivos são gravados em `empresa/clientes/competencia/cliente`, que é a estrutura usada pelas telas `Disparo` e `Fila`.

Na mesma aba, a limpeza de competências antigas permite manter as últimas `X` competências. Antes de remover cada pasta antiga, o sistema cria um ZIP individual em:

```text
backups/pdfs/empresa/competencia.zip
```

## Chatwoot e WhatsApp

A aba `Chatwoot` cadastra a integração por empresa em `_config/chatwoot.csv` e permite testar o envio.

Na aba `Clientes`, o botão `Enviar WhatsApp` envia uma mensagem para o cliente selecionado usando `telefone_whatsapp`.

Nas abas `Disparo` e `Fila`, a opção `Enviar WhatsApp após e-mail` envia uma mensagem automática depois que o e-mail for enviado com sucesso.

Os envios WhatsApp são registrados em:

```text
logs/whatsapp.csv
```

O Dashboard exibe KPIs de WhatsApp enviado e erro, e a aba `Logs` mostra o histórico WhatsApp.

## Deploy no Portainer

1. Crie o registro DNS `cobrancas.wrtec.com.br` apontando para o servidor do Traefik.
2. Crie as pastas persistentes em `/root/sistema-cobrancas-email`.
3. Copie os CSVs, modelos e PDFs operacionais para essas pastas.
4. Defina `APP_ADMIN_PASSWORD` nas variaveis da stack antes do primeiro deploy, ou carregue seu `usuarios.csv` em `/root/sistema-cobrancas-email/config`.
5. Crie uma stack no Portainer usando `docker-stack.portainer.yml`.
6. Habilite o webhook da stack no Portainer.
7. No GitHub, crie o secret `PORTAINER_WEBHOOK_URL` com a URL do webhook.
8. Quando quiser ativar deploy automatico em push na `main`, crie o secret `ENABLE_PORTAINER_DEPLOY` com valor `true`.

Enquanto `ENABLE_PORTAINER_DEPLOY` nao estiver como `true`, o Actions so constroi e publica a imagem.
