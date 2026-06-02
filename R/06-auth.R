# =========================================================
# AUTENTICAÇÃO
# =========================================================

caminho_usuarios <- function() {
  file.path(pasta_raiz, "_config", "usuarios.csv")
}

criar_usuarios_padrao <- function() {
  caminho <- caminho_usuarios()
  
  if (!file.exists(caminho)) {
    usuario_inicial <- Sys.getenv("APP_ADMIN_USER", unset = "admin")
    senha_inicial <- Sys.getenv("APP_ADMIN_PASSWORD", unset = "admin123")
    
    usuarios <- tibble::tibble(
      usuario = usuario_inicial,
      senha = senha_inicial,
      ativo = TRUE
    )
    
    readr::write_csv(usuarios, caminho)
  }
}

carregar_usuarios <- function() {
  criar_usuarios_padrao()
  
  readr::read_csv(
    caminho_usuarios(),
    show_col_types = FALSE,
    col_types = readr::cols(.default = "c")
  )
}

validar_login <- function(usuario, senha) {
  usuarios <- carregar_usuarios()
  
  usuarios$ativo <- as.logical(usuarios$ativo)
  
  any(
    usuarios$usuario == usuario &
      usuarios$senha == senha &
      usuarios$ativo == TRUE
  )
}
