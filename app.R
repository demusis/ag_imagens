#
# Aplicativo Shiny para Análise Geométrica sobre Imagens
#

# Carregamento das bibliotecas necessárias
library(shiny)
library(magick)
library(colourpicker)
library(shinythemes)
library(rclipboard)

# Aumenta o limite de upload de arquivo para 30MB
options(shiny.maxRequestSize = 30*1024^2)

# --- Definição da Interface do Usuário (UI) ---
ui <- fluidPage(
  theme = shinytheme("flatly"),
  rclipboardSetup(),
  tags$head(
    tags$style(HTML("
      :root {
        --royal-blue: #4169E1; --light-gray: #f5f5f5; --dark-text: #333;
        --success-green: #28a745; --danger-red: #dc3545;
      }
      .title-panel, h1 { color: var(--royal-blue); font-weight: bold; }
      .well { background-color: var(--light-gray); border: 1px solid #ddd;
              box-shadow: 0 2px 5px rgba(0,0,0,0.1); border-radius: 8px; }
      .nav-tabs > li > a { color: var(--dark-text); }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        color: white; background-color: var(--royal-blue); border-color: var(--royal-blue); font-weight: bold;
      }
      .btn-primary { background-color: var(--royal-blue); border-color: var(--royal-blue); }
      #btn_calcular_razao { background-color: var(--success-green); border-color: var(--success-green); color: white; }
      #btn_apagar_elemento { background-color: var(--danger-red); border-color: var(--danger-red); color: white; }
      .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar { background: var(--royal-blue); border-top: 1px solid var(--royal-blue); border-bottom: 1px solid var(--royal-blue); }
      .js-irs-0 .irs-from, .js-irs-0 .irs-to { background: var(--royal-blue); }
      .plot-container { border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }
      #log_operacoes { white-space: pre-wrap; word-wrap: break-word; }
    "))
  ),
  titlePanel("Ferramenta de Análise Geométrica sobre Imagem"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      tabsetPanel(
        id = "abas_controles",
        tabPanel("Configurar",
                 br(),
                 h5("1. Carregar Imagem"),
                 fileInput("upload_imagem", "Selecione o arquivo:", accept = c("image/png", "image/jpeg", "image/bmp")),
                 hr(),
                 h5("2. Configurar Canvas"),
                 uiOutput("ui_margem_slider")
        ),
        tabPanel("Adicionar",
                 br(),
                 selectInput("acao_adicionar", "Selecione o elemento:",
                             choices = c("Adicionar Ponto Finito", "Adicionar Ponto no Infinito", "Adicionar Reta", "Ponto de Interseção")),
                 hr(),
                 uiOutput("ui_adicionar_elemento")
        ),
        tabPanel("Razão Cruzada",
                 br(),
                 h5("Medição por Escala"),
                 p("Selecione 4 pontos (máx. 1 no infinito)."),
                 selectizeInput("pontos_razao_cruzada", "Pontos:", choices = NULL, multiple = TRUE, options = list(maxItems = 4)),
                 hr(),
                 uiOutput("ui_razao_cruzada_controles"),
                 actionButton("btn_calcular_razao", "Calcular Distância"),
                 hr(),
                 h5("Resultado"),
                 htmlOutput("resultado_razao_cruzada")
        ),
        tabPanel("Gerenciar",
                 br(),
                 h5("Selecionar Elemento"),
                 uiOutput("selecao_elemento_gerenciar_ui"),
                 hr(),
                 h5("Alterar Propriedades"),
                 uiOutput("edicao_elemento_ui"),
                 hr(),
                 h5("Ações"),
                 actionButton("btn_apagar_elemento", "Apagar Selecionado", icon = icon("trash-alt")),
                 actionButton("btn_limpar_tudo", "Limpar Tudo", icon = icon("trash")),
                 hr(),
                 h5("Salvar/Carregar Sessão"),
                 p("Salve ou carregue o conjunto de pontos e retas."),
                 downloadButton("salvar_sessao_btn", "Salvar Sessão (.rds)", icon = icon("save")),
                 br(), br(),
                 fileInput("carregar_sessao_input", "Carregar Sessão (.rds)", accept = ".rds")
        )
      )
    ),
    mainPanel(
      width = 10,
      tabsetPanel(
        id = "abas_main",
        tabPanel("Imagem", 
                 div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                     div(style="flex-grow: 1; margin-right: 20px;",
                         sliderInput("zoom_slider", "Zoom:", min = 25, max = 250, value = 100, step = 5, post = "%", width = "100%")
                     ),
                     div(downloadButton("salvar_imagem_btn", "Salvar Imagem", icon = icon("save"), class = "btn-primary"))
                 ),
                 div(class = "plot-container", style = "width: 100%; height: 600px; overflow: auto;",
                     uiOutput("zoomable_plot_ui")
                 )
        ),
        tabPanel("Log de Operações",
                 uiOutput("log_panel_ui")
        )
      )
    )
  )
)

# --- Definição da Lógica do Servidor (Server) ---
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    imagem_original = NULL,
    pontos = data.frame(
      id = character(), type = character(), x = numeric(), y = numeric(), 
      cor = character(), cex_simbolo = numeric(), cex_fonte = numeric(), 
      origem = character(), mostrar_legenda = logical(), 
      mostrar_visualizacao = logical(), legenda = character(),
      stringsAsFactors = FALSE
    ),
    retas = list(),
    log = character(),
    resultado_razao = ""
  )
  
  # --- Funções Auxiliares ---
  log_msg <- function(msg) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    rv$log <- c(paste0("[", timestamp, "] ", msg), rv$log)
  }
  
  is_legenda_unica <- function(legenda, id_a_ignorar = NULL) {
    legendas_pontos <- rv$pontos$legenda
    legendas_retas <- sapply(rv$retas, `[[`, "legenda")
    
    if (!is.null(id_a_ignorar)) {
      if (grepl("^P", id_a_ignorar)) {
        legendas_pontos <- legendas_pontos[rv$pontos$id != id_a_ignorar]
      } else {
        legendas_retas <- legendas_retas[names(rv$retas) != id_a_ignorar]
      }
    }
    todas_as_legendas <- c(legendas_pontos, legendas_retas)
    return(!(legenda %in% todas_as_legendas))
  }
  
  calcular_equacao_reta <- function(p1, p2) {
    if (abs(p2$x - p1$x) < 1e-9) { return(list(m = Inf, c = p1$x)) } 
    else { m <- (p2$y - p1$y) / (p2$x - p1$x); c <- p1$y - m * p1$x; return(list(m = m, c = c)) }
  }
  
  distancia_euclidiana <- function(p1, p2) { sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2) }
  
  gerar_novo_id <- function(prefixo, existentes) {
    ids_com_prefixo <- grep(paste0("^", prefixo), existentes, value = TRUE)
    if (length(ids_com_prefixo) == 0) { return(paste0(prefixo, 1)) } 
    else { numeros <- as.numeric(gsub(prefixo, "", ids_com_prefixo)); return(paste0(prefixo, max(numeros, na.rm = TRUE) + 1)) }
  }
  
  projetar_ponto_na_linha <- function(ponto, fit) {
    if(is.null(fit) || any(is.na(coef(fit)))) return(ponto)
    a <- coef(fit)[2]; b <- -1; c <- coef(fit)[1]
    x0 <- ponto$x; y0 <- ponto$y
    x_proj <- (b * (b * x0 - a * y0) - a * c) / (a^2 + b^2)
    y_proj <- (a * (-b * x0 + a * y0) - b * c) / (a^2 + b^2)
    return(data.frame(x = x_proj, y = y_proj))
  }
  
  # --- Observadores de Eventos ---
  
  observeEvent(input$upload_imagem, {
    req(input$upload_imagem$datapath)
    tryCatch({
      rv$imagem_original <- image_read(input$upload_imagem$datapath)
      rv$pontos <- data.frame(id=character(), type=character(), x=numeric(), y=numeric(), cor=character(), cex_simbolo=numeric(), cex_fonte=numeric(), origem=character(), mostrar_legenda=logical(), mostrar_visualizacao=logical(), legenda=character(), stringsAsFactors=FALSE)
      rv$retas <- list()
      rv$log <- character()
      log_msg(paste0("Imagem '", input$upload_imagem$name, "' carregada."))
    }, error = function(e) { log_msg(paste("Erro:", e$message)) })
  })
  
  observeEvent(input$plot_click, {
    req(rv$imagem_original, input$acao_adicionar == "Adicionar Ponto Finito")
    
    req(input$legenda_ponto)
    legenda_final <- trimws(input$legenda_ponto)
    if (legenda_final == "") {
      showNotification("A legenda é obrigatória.", type = "error")
      return()
    }
    if (!is_legenda_unica(legenda_final)) {
      showNotification("Essa legenda já existe. Escolha uma legenda única.", type = "error")
      return()
    }
    
    req(input$cor_ponto, input$tamanho_simbolo_ponto, input$tamanho_fonte_ponto, !is.null(input$add_ponto_mostrar_legenda))
    
    margem <- input$margem_canvas; info_img <- image_info(rv$imagem_original)
    x_canvas <- input$plot_click$x; y_canvas <- input$plot_click$y
    x_img <- x_canvas - margem; y_img <- y_canvas - margem
    
    if (x_img >= 0 && x_img <= info_img$width && y_img >= 0 && y_img <= info_img$height) {
      novo_id <- gerar_novo_id("P", rv$pontos$id)
      rv$pontos <- rbind(rv$pontos, data.frame(
        id = novo_id, type = "finito", x = x_img, y = y_img, 
        cor = input$cor_ponto, cex_simbolo = input$tamanho_simbolo_ponto, cex_fonte = input$tamanho_fonte_ponto, 
        origem = "clique", mostrar_legenda = input$add_ponto_mostrar_legenda, 
        mostrar_visualizacao = TRUE, legenda = legenda_final
      ))
      log_msg(sprintf("Ponto Finito %s ('%s') adicionado em (x=%.2f, y=%.2f).", novo_id, legenda_final, x_img, y_img))
    }
  })
  
  observeEvent(input$btn_criar_ponto_infinito, {
    req(input$legenda_ponto)
    legenda_final <- trimws(input$legenda_ponto)
    if (legenda_final == "") {
      showNotification("A legenda é obrigatória.", type = "error")
      return()
    }
    if (!is_legenda_unica(legenda_final)) {
      showNotification("Essa legenda já existe. Escolha uma legenda única.", type = "error")
      return()
    }
    
    req(input$ponto_dir1, input$ponto_dir2, input$cor_ponto, input$tamanho_simbolo_ponto, input$tamanho_fonte_ponto, !is.null(input$add_ponto_mostrar_legenda))
    
    if(input$ponto_dir1 == input$ponto_dir2) { log_msg("Erro: Pontos para definir a direção devem ser diferentes."); return() }
    
    p1 <- rv$pontos[rv$pontos$id == input$ponto_dir1, ]; p2 <- rv$pontos[rv$pontos$id == input$ponto_dir2, ]
    vx <- p2$x - p1$x; vy <- p2$y - p1$y
    magnitude <- sqrt(vx^2 + vy^2)
    
    if(magnitude < 1e-6) { log_msg("Erro: Pontos selecionados são coincidentes."); return() }
    
    vx_norm <- vx / magnitude; vy_norm <- vy / magnitude
    novo_id <- gerar_novo_id("Pinf", rv$pontos$id)
    
    rv$pontos <- rbind(rv$pontos, data.frame(
      id = novo_id, type = "infinito", x = vx_norm, y = vy_norm,
      cor = input$cor_ponto, cex_simbolo = input$tamanho_simbolo_ponto, cex_fonte = input$tamanho_fonte_ponto,
      origem = paste0("direcao:", p1$id, ",", p2$id), mostrar_legenda = input$add_ponto_mostrar_legenda,
      mostrar_visualizacao = TRUE, legenda = legenda_final
    ))
    log_msg(sprintf("Ponto no Infinito %s ('%s') criado com direção definida por %s -> %s.", novo_id, legenda_final, p1$id, p2$id))
  })
  
  observeEvent(input$btn_criar_reta, {
    req(input$legenda_reta)
    legenda_final <- trimws(input$legenda_reta)
    if (legenda_final == "") {
      showNotification("A legenda é obrigatória.", type = "error")
      return()
    }
    if (!is_legenda_unica(legenda_final)) {
      showNotification("Essa legenda já existe. Escolha uma legenda única.", type = "error")
      return()
    }
    
    req(input$ponto1_reta, input$ponto2_reta, input$cor_reta, input$espessura_reta, input$tamanho_fonte_reta, !is.null(input$prolongar_reta), !is.null(input$add_reta_mostrar_legenda))
    
    if (input$ponto1_reta == input$ponto2_reta) { log_msg("Erro: Pontos para a reta devem ser diferentes."); return() }
    
    p1_data <- rv$pontos[rv$pontos$id == input$ponto1_reta, ]; p2_data <- rv$pontos[rv$pontos$id == input$ponto2_reta, ]
    
    if(p1_data$type == "infinito" && p2_data$type == "infinito") { log_msg("Erro: Não é possível criar uma reta entre dois pontos no infinito."); return() }
    if(p1_data$type == "infinito") { temp <- p1_data; p1_data <- p2_data; p2_data <- temp }
    
    if (p2_data$type == "finito") { eq_nova <- calcular_equacao_reta(p1_data, p2_data)
    } else {
      dir_vec <- c(p2_data$x, p2_data$y)
      if(abs(dir_vec[1]) < 1e-9) { eq_nova <- list(m = Inf, c = p1_data$x)
      } else { m <- dir_vec[2] / dir_vec[1]; c <- p1_data$y - m * p1_data$x; eq_nova <- list(m = m, c = c) }
    }
    
    id <- gerar_novo_id("L", names(rv$retas))
    
    rv$retas[[id]] <- list(id = id, p1 = p1_data$id, p2 = p2_data$id, eq = eq_nova, cor = input$cor_reta, espessura = input$espessura_reta, tamanho_fonte = input$tamanho_fonte_reta, prolongar = input$prolongar_reta, mostrar_legenda = input$add_reta_mostrar_legenda, legenda = legenda_final)
    
    eq_texto <- if (is.infinite(eq_nova$m)) sprintf("x = %.2f", eq_nova$c) else sprintf("y = %.2fx + %.2f", eq_nova$m, eq_nova$c)
    log_msg(sprintf("Reta %s ('%s') criada (Pontos: %s, %s). Equação: %s", id, legenda_final, p1_data$id, p2_data$id, eq_texto))
  })
  
  observeEvent(input$btn_calcular_razao, {
    pontos_selecionados <- input$pontos_razao_cruzada
    if(length(pontos_selecionados) != 4) { rv$resultado_razao <- "<p style='color:red;'>Erro: Selecione exatamente 4 pontos.</p>"; return() }
    
    pontos_data <- rv$pontos[rv$pontos$id %in% pontos_selecionados, ]
    pontos_infinitos <- pontos_data[pontos_data$type == "infinito", ]
    pontos_finitos <- pontos_data[pontos_data$type == "finito", ]
    
    if(nrow(pontos_infinitos) > 1) { log_msg("Cálculo de Razão Cruzada: Falha. Mais de um ponto no infinito selecionado."); rv$resultado_razao <- "<p style='color:red;'>Erro: Selecione no máximo um ponto no infinito.</p>"; return() }
    
    if(nrow(pontos_infinitos) == 1) {
      req(input$ponto_ref1, input$ponto_ref2, input$ponto_alvo1, input$ponto_alvo2)
      if (input$ponto_ref1 == input$ponto_ref2 || input$ponto_alvo1 == input$ponto_alvo2) { rv$resultado_razao <- "<p style='color:red;'>Erro: Os pontos em um par devem ser diferentes.</p>"; return() }
      
      is_vertical <- diff(range(pontos_finitos$x)) < 1e-6
      if(is_vertical) { avg_x <- mean(pontos_finitos$x); pontos_projetados <- data.frame(id = pontos_finitos$id, x = avg_x, y = pontos_finitos$y)
      } else {
        fit <- lm(y ~ x, data = pontos_finitos)
        pontos_projetados <- do.call(rbind, lapply(1:nrow(pontos_finitos), function(i) { p_proj <- projetar_ponto_na_linha(pontos_finitos[i,], fit); data.frame(id = pontos_finitos$id[i], x = p_proj$x, y = p_proj$y) }))
      }
      
      ref1_data_proj <- pontos_projetados[pontos_projetados$id == input$ponto_ref1,]; ref2_data_proj <- pontos_projetados[pontos_projetados$id == input$ponto_ref2,]
      dist_ref_img <- distancia_euclidiana(ref1_data_proj, ref2_data_proj)
      
      if(dist_ref_img < 1e-6) { rv$resultado_razao <- "<p style='color:red;'>Erro: Distância de referência na imagem é zero.</p>"; return() }
      
      escala <- input$distancia_referencia / dist_ref_img
      alvo1_data_proj <- pontos_projetados[pontos_projetados$id == input$ponto_alvo1,]; alvo2_data_proj <- pontos_projetados[pontos_projetados$id == input$ponto_alvo2,]
      dist_alvo_img <- distancia_euclidiana(alvo1_data_proj, alvo2_data_proj)
      dist_alvo_real <- dist_alvo_img * escala
      
      log_msg(sprintf("Cálculo de Distância por Escala (1 Ponto Infinito). Pontos: %s. Distância de referência D(%s, %s) = %.2f. Fator de escala calculado: %.4f. Distância calculada D(%s, %s) = %.4f.", 
                      paste(pontos_selecionados, collapse=", "), input$ponto_ref1, input$ponto_ref2, input$distancia_referencia, escala, input$ponto_alvo1, input$ponto_alvo2, dist_alvo_real))
      
      rv$resultado_razao <- paste0("<p><b>Método:</b> Escala Simples (1 Ponto Infinito)</p>", "<p><b>Fator de Escala:</b> ", sprintf("%.4f", escala), "</p>", "<p><b>Distância Alvo (Projetada):</b> ", sprintf("%.2f", dist_alvo_img), " pixels</p>", "<p><b>Distância Real Calculada:</b> <b style='color:blue; font-size:1.2em;'>", sprintf("%.4f", dist_alvo_real), "</b></p>")
      
    } else {
      req(input$dist_ref1, input$dist_ref2, input$alvo1, input$alvo2)
      pontos_ordenados_id <- input$pontos_razao_cruzada
      pontos_data <- pontos_data[match(pontos_ordenados_id, pontos_data$id),]
      
      is_vertical <- diff(range(pontos_data$x)) < 1e-6
      if(is_vertical) { avg_x <- mean(pontos_data$x); pontos_projetados_df <- data.frame(id = pontos_data$id, x = avg_x, y = pontos_data$y)
      } else {
        fit <- lm(y ~ x, data = pontos_data)
        pontos_projetados_df <- do.call(rbind, lapply(1:nrow(pontos_data), function(i) { p_proj <- projetar_ponto_na_linha(pontos_data[i,], fit); data.frame(id = pontos_data$id[i], x = p_proj$x, y = p_proj$y) }))
      }
      
      p_origem <- pontos_projetados_df[1,]
      pontos_projetados_df$d <- sapply(1:nrow(pontos_projetados_df), function(i) { distancia_euclidiana(p_origem, pontos_projetados_df[i,]) })
      
      a_prime <- 0; b_prime <- pontos_projetados_df$d[2]; c_prime <- pontos_projetados_df$d[3]; d_prime <- pontos_projetados_df$d[4]
      
      if (abs(c_prime - b_prime) < 1e-9 || abs(d_prime - a_prime) < 1e-9) { rv$resultado_razao <- "<p style='color:red;'>Erro: Configuração de pontos degenerada. Pontos colineares coincidem.</p>"; return() }
      K <- ((c_prime - a_prime) * (d_prime - b_prime)) / ((c_prime - b_prime) * (d_prime - a_prime))
      
      D_AB <- input$dist_ref1; D_CD <- input$dist_ref2
      a_quad <- K - 1; b_quad <- (K-1)*D_AB + (K-1)*D_CD; c_quad <- - D_AB * D_CD 
      
      if (abs(a_quad) < 1e-9) {
        if (abs(b_quad) < 1e-9) { rv$resultado_razao <- "<p style='color:red;'>Erro: Configuração degenerada (K=1 e referências inconsistentes).</p>"; return() }
        x <- -c_quad / b_quad
      } else {
        delta <- b_quad^2 - 4*a_quad*c_quad
        if (delta < 0) { rv$resultado_razao <- "<p style='color:red;'>Erro: Nenhuma solução real encontrada (delta < 0).</p>"; return() }
        x1 <- (-b_quad + sqrt(delta)) / (2*a_quad); x2 <- (-b_quad - sqrt(delta)) / (2*a_quad)
        x <- ifelse(x1 > 0, x1, x2)
      }
      D_BC <- x
      
      mapa_coords_reais <- list(0, D_AB, D_AB + D_BC, D_AB + D_BC + D_CD)
      names(mapa_coords_reais) <- pontos_ordenados_id
      dist_final <- abs(mapa_coords_reais[[input$alvo2]] - mapa_coords_reais[[input$alvo1]])
      
      log_msg(sprintf(paste("Cálculo por Razão Cruzada (4 Pontos Finitos):",
                            " Pontos (ordem A,B,C,D): %s;",
                            "Distâncias fornecidas: D(A,B)=%.2f, D(C,D)=%.2f;",
                            "Equação: K = (AC'*BD')/(BC'*AD') onde P' são as projeções na imagem;",
                            "Razão Cruzada calculada (K): %.4f;",
                            "Distância calculada D(%s, %s) = %.4f."),
                      paste(pontos_ordenados_id, collapse=", "), D_AB, D_CD, K, input$alvo1, input$alvo2, dist_final))
      
      rv$resultado_razao <- paste0("<p><b>Método:</b> Razão Cruzada (4 Pontos Finitos)</p>", "<p><b>Razão Cruzada (K):</b> ", sprintf("%.4f", K), "</p>", "<p><b>Distância Interna D(P2,P3) Estimada:</b> ", sprintf("%.4f", D_BC), "</p>", "<p><b>Distância Real D(",input$alvo1,", ",input$alvo2,") Calculada:</b> <b style='color:blue; font-size:1.2em;'>", sprintf("%.4f", dist_final), "</b></p>")
    }
  })
  
  observeEvent(input$btn_intersecao, {
    req(input$legenda_ponto)
    legenda_final <- trimws(input$legenda_ponto)
    if (legenda_final == "") {
      showNotification("A legenda é obrigatória.", type = "error")
      return()
    }
    if (!is_legenda_unica(legenda_final)) {
      showNotification("Essa legenda já existe. Escolha uma legenda única.", type = "error")
      return()
    }
    
    req(input$reta1_intersecao, input$reta2_intersecao, input$cor_ponto, input$tamanho_simbolo_ponto, input$tamanho_fonte_ponto, !is.null(input$add_ponto_mostrar_legenda))
    
    if (input$reta1_intersecao == input$reta2_intersecao) { log_msg("Erro: Retas devem ser diferentes."); return() }
    
    reta1 <- rv$retas[[input$reta1_intersecao]]; reta2 <- rv$retas[[input$reta2_intersecao]]
    m1 <- reta1$eq$m; c1 <- reta1$eq$c; m2 <- reta2$eq$m; c2 <- reta2$eq$c
    
    if (abs(m1 - m2) < 1e-9) { log_msg("As retas são paralelas e não se interceptam."); return() }
    
    if (is.infinite(m1)) { x_inter <- c1; y_inter <- m2 * x_inter + c2
    } else if (is.infinite(m2)) { x_inter <- c2; y_inter <- m1 * x_inter + c1
    } else { x_inter <- (c2 - c1) / (m1 - m2); y_inter <- m1 * x_inter + c1 }
    
    novo_id <- gerar_novo_id("P", rv$pontos$id)
    
    rv$pontos <- rbind(rv$pontos, data.frame(
      id = novo_id, type = "finito", x = x_inter, y = y_inter, 
      cor = input$cor_ponto, cex_simbolo = input$tamanho_simbolo_ponto, cex_fonte = input$tamanho_fonte_ponto, 
      origem = paste0("intersecao:", reta1$id, ",", reta2$id), mostrar_legenda = input$add_ponto_mostrar_legenda,
      mostrar_visualizacao = TRUE, legenda = legenda_final
    ))
    log_msg(sprintf("Ponto de interseção %s ('%s') criado entre as retas %s e %s em (x=%.2f, y=%.2f).", novo_id, legenda_final, reta1$id, reta2$id, x_inter, y_inter))
  })
  
  observeEvent(input$btn_apagar_elemento, {
    req(input$elemento_selecionado)
    id_apagar <- input$elemento_selecionado
    elementos_a_apagar <- list(pontos = c(), retas = c())
    
    encontrar_dependentes <- function(id) {
      if (grepl("^P", id) && !(id %in% elementos_a_apagar$pontos)) {
        elementos_a_apagar$pontos <<- c(elementos_a_apagar$pontos, id)
        for (reta_id in names(rv$retas)) {
          reta <- rv$retas[[reta_id]]
          if ((!is.null(reta$p1) && reta$p1 == id) || (!is.null(reta$p2) && reta$p2 == id)) { encontrar_dependentes(reta_id) }
        }
        for(i in seq_len(nrow(rv$pontos))) {
          if(rv$pontos$type[i] == "infinito" && grepl(paste0("\\b", id, "\\b"), rv$pontos$origem[i])) { encontrar_dependentes(rv$pontos$id[i]) }
        }
      } else if (grepl("^L", id) && !(id %in% elementos_a_apagar$retas)) {
        elementos_a_apagar$retas <<- c(elementos_a_apagar$retas, id)
        for (i in seq_len(nrow(rv$pontos))) {
          if (rv$pontos$type[i] == "finito" && grepl(paste0("\\b", id, "\\b"), rv$pontos$origem[i])) { encontrar_dependentes(rv$pontos$id[i]) }
        }
      }
    }
    encontrar_dependentes(id_apagar)
    
    if (length(elementos_a_apagar$pontos) > 0) { rv$pontos <- rv$pontos[!rv$pontos$id %in% elementos_a_apagar$pontos, ]; log_msg(paste("Pontos apagados (com dependentes):", paste(elementos_a_apagar$pontos, collapse=", "))) }
    if (length(elementos_a_apagar$retas) > 0) { rv$retas[elementos_a_apagar$retas] <- NULL; log_msg(paste("Retas apagadas (com dependentes):", paste(elementos_a_apagar$retas, collapse=", "))) }
  })
  
  observeEvent(input$btn_limpar_tudo, {
    rv$pontos <- data.frame(id=character(), type=character(), x=numeric(), y=numeric(), cor=character(), cex_simbolo=numeric(), cex_fonte=numeric(), origem=character(), mostrar_legenda=logical(), mostrar_visualizacao=logical(), legenda=character(), stringsAsFactors=FALSE)
    rv$retas <- list()
    log_msg("Todas as anotações foram limpas.")
  })
  
  observeEvent(input$btn_limpar_log, { rv$log <- character() })
  
  # --- LÓGICA PARA SALVAR E CARREGAR SESSÃO ---
  
  output$salvar_sessao_btn <- downloadHandler(
    filename = function() {
      paste0("sessao-analise-geometrica-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rds")
    },
    content = function(file) {
      estado_para_salvar <- list(
        pontos = rv$pontos,
        retas = rv$retas,
        log = rv$log
      )
      saveRDS(estado_para_salvar, file)
      log_msg("Sessão salva com sucesso.")
    }
  )
  
  observeEvent(input$carregar_sessao_input, {
    req(input$carregar_sessao_input$datapath)
    
    tryCatch({
      estado_carregado <- readRDS(input$carregar_sessao_input$datapath)
      
      if (is.list(estado_carregado) && all(c("pontos", "retas") %in% names(estado_carregado))) {
        rv$pontos <- estado_carregado$pontos
        rv$retas <- estado_carregado$retas
        if (!is.null(estado_carregado$log)) {
          rv$log <- estado_carregado$log
        } else {
          rv$log <- character()
        }
        log_msg(paste0("Sessão '", input$carregar_sessao_input$name, "' carregada com sucesso."))
        showNotification("Sessão carregada.", type = "message")
      } else {
        log_msg("Erro: O arquivo carregado não é um arquivo de sessão válido.")
        showNotification("Arquivo de sessão inválido.", type = "error")
      }
    }, error = function(e) {
      log_msg(paste("Erro ao carregar arquivo de sessão:", e$message))
      showNotification("Falha ao ler o arquivo. Pode estar corrompido.", type = "error")
    })
  })
  
  
  # --- Renderização da UI Dinâmica ---
  
  output$ui_margem_slider <- renderUI({
    max_dim <- 0
    if(!is.null(rv$imagem_original)) { info <- image_info(rv$imagem_original); max_dim <- max(info$width, info$height) }
    sliderInput("margem_canvas", "Tamanho da margem (pixels):", min = 0, max = max_dim, value = 0, step = 10)
  })
  
  # --- INÍCIO DA MODIFICAÇÃO ---
  output$ui_adicionar_elemento <- renderUI({
    req(input$acao_adicionar)
    # Estabelece dependência reativa para que a UI se atualize quando pontos/retas mudam
    rv$pontos
    rv$retas
    
    # Calcula a sugestão de legenda antes de criar os controles
    prefixo <- switch(input$acao_adicionar,
                      "Adicionar Ponto Finito" = "P",
                      "Adicionar Ponto no Infinito" = "Pinf",
                      "Adicionar Reta" = "L",
                      "Ponto de Interseção" = "P",
                      NULL
    )
    
    sugestao_legenda <- if (!is.null(prefixo)) {
      existentes <- if (prefixo == "L") names(rv$retas) else rv$pontos$id
      gerar_novo_id(prefixo, existentes)
    } else {
      ""
    }
    
    # Define os controles, inserindo a sugestão de legenda no 'value'
    controles_ponto <- tagList(
      textInput("legenda_ponto", "Legenda:", value = sugestao_legenda, placeholder = "Obrigatória e única"),
      colourInput("cor_ponto", "Cor:", value = "red"),
      sliderInput("tamanho_simbolo_ponto", "Tamanho do símbolo:", min = 1, max = 5, value = 1.5, step = 0.1),
      sliderInput("tamanho_fonte_ponto", "Tamanho da fonte:", min = 0.5, max = 3, value = 1, step = 0.1),
      checkboxInput("add_ponto_mostrar_legenda", "Mostrar legenda", value = TRUE)
    )
    
    controles_reta <- tagList(
      uiOutput("ui_para_reta"),
      textInput("legenda_reta", "Legenda:", value = sugestao_legenda, placeholder = "Obrigatória e única"),
      colourInput("cor_reta", "Cor:", value = "blue"),
      sliderInput("espessura_reta", "Espessura:", min = 1, max = 10, value = 1.5, step = 0.5),
      sliderInput("tamanho_fonte_reta", "Tamanho da fonte:", min = 0.5, max = 3, value = 1, step = 0.1),
      checkboxInput("prolongar_reta", "Prolongar reta nos extremos", value = FALSE),
      checkboxInput("add_reta_mostrar_legenda", "Mostrar legenda", value = TRUE),
      actionButton("btn_criar_reta", "Criar Reta", class = "btn-primary")
    )
    
    switch(input$acao_adicionar,
           "Adicionar Ponto Finito" = tagList(h5("Opções do Ponto"), p("Clique na imagem para adicionar."), controles_ponto),
           "Adicionar Ponto no Infinito" = tagList(h5("Definir Direção"), p("Selecione dois pontos."), uiOutput("ui_para_ponto_infinito"), h5("Propriedades"), controles_ponto, actionButton("btn_criar_ponto_infinito", "Criar Ponto no Infinito", class = "btn-primary")),
           "Adicionar Reta" = tagList(h5("Opções da Reta"), controles_reta),
           "Ponto de Interseção" = tagList(h5("Opções da Interseção"), uiOutput("selecao_reta_intersecao_ui"), h5("Propriedades do Ponto"), controles_ponto, actionButton("btn_intersecao", "Encontrar Interseção", class = "btn-primary"))
    )
  })
  # --- FIM DA MODIFICAÇÃO ---
  
  observe({ 
    opcoes_pontos <- rv$pontos$id
    names(opcoes_pontos) <- rv$pontos$legenda
    updateSelectizeInput(session, "pontos_razao_cruzada", choices = opcoes_pontos, server = TRUE) 
  })
  
  output$ui_razao_cruzada_controles <- renderUI({
    req(input$pontos_razao_cruzada)
    
    pontos_data_selecionados <- rv$pontos[rv$pontos$id %in% input$pontos_razao_cruzada, ]
    choices_com_legenda <- setNames(pontos_data_selecionados$id, pontos_data_selecionados$legenda)
    
    pontos_finitos_data <- pontos_data_selecionados[pontos_data_selecionados$type == "finito",]
    choices_finitos_com_legenda <- setNames(pontos_finitos_data$id, pontos_finitos_data$legenda)
    
    if(any(pontos_data_selecionados$type == "infinito")) {
      tagList(h5("Distância de Referência"), selectInput("ponto_ref1", "Ponto de Referência 1:", choices = choices_finitos_com_legenda), selectInput("ponto_ref2", "Ponto de Referência 2:", choices = choices_finitos_com_legenda, selected = if(length(choices_finitos_com_legenda)>1) choices_finitos_com_legenda[2] else NULL), numericInput("distancia_referencia", "Distância Real de Referência:", value = 1), hr(), h5("Distância a Calcular"), selectInput("ponto_alvo1", "Ponto Alvo 1:", choices = choices_finitos_com_legenda, selected = if(length(choices_finitos_com_legenda)>2) choices_finitos_com_legenda[3] else NULL), selectInput("ponto_alvo2", "Ponto Alvo 2:", choices = choices_finitos_com_legenda, selected = if(length(choices_finitos_com_legenda)>3) choices_finitos_com_legenda[4] else NULL))
    } else {
      req(length(input$pontos_razao_cruzada) == 4)
      
      pontos_ordenados <- rv$pontos[match(input$pontos_razao_cruzada, rv$pontos$id), ]
      legendas_ordenadas <- pontos_ordenados$legenda
      
      tagList(p("Importante: A ordem de seleção dos 4 pontos é (A, B, C, D)."), h5("Distâncias de Referência"), numericInput("dist_ref1", paste0("Distância Real (", legendas_ordenadas[1], " -> ", legendas_ordenadas[2], "):"), value = 1), numericInput("dist_ref2", paste0("Distância Real (", legendas_ordenadas[3], " -> ", legendas_ordenadas[4], "):"), value = 1), hr(), h5("Distância a Calcular"), selectInput("alvo1", "Ponto 1:", choices = choices_com_legenda), selectInput("alvo2", "Ponto 2:", choices = choices_com_legenda, selected = if(length(choices_com_legenda)>1) choices_com_legenda[2] else NULL))
    }
  })
  
  output$resultado_razao_cruzada <- renderUI({ HTML(rv$resultado_razao) })
  
  output$ui_para_ponto_infinito <- renderUI({
    pontos_finitos <- rv$pontos[rv$pontos$type == "finito", ]
    if (nrow(pontos_finitos) < 2) { p("São necessários pelo menos 2 pontos finitos.") }
    else { 
      choices_com_legenda <- setNames(pontos_finitos$id, pontos_finitos$legenda)
      tagList( selectInput("ponto_dir1", "Ponto de Origem:", choices = choices_com_legenda), selectInput("ponto_dir2", "Ponto de Destino:", choices = choices_com_legenda, selected = if(length(choices_com_legenda) > 1) choices_com_legenda[2] else NULL) )
    }
  })
  
  output$ui_para_reta <- renderUI({
    if (nrow(rv$pontos) < 2) { p("São necessários pelo menos 2 pontos.") }
    else { 
      choices_com_legenda <- setNames(rv$pontos$id, rv$pontos$legenda)
      tagList(selectInput("ponto1_reta", "Ponto 1:", choices = choices_com_legenda), selectInput("ponto2_reta", "Ponto 2:", choices = choices_com_legenda, selected = if(nrow(rv$pontos) > 1) choices_com_legenda[2] else NULL)) 
    }
  })
  
  output$selecao_reta_intersecao_ui <- renderUI({
    if (length(rv$retas) < 2) { p("São necessárias pelo menos 2 retas.") }
    else { 
      choices_com_legenda <- setNames(names(rv$retas), sapply(rv$retas, `[[`, "legenda"))
      tagList(selectInput("reta1_intersecao", "Reta 1:", choices = choices_com_legenda), selectInput("reta2_intersecao", "Reta 2:", choices = choices_com_legenda, selected = if(length(rv$retas) > 1) choices_com_legenda[2] else NULL)) 
    }
  })
  
  output$selecao_elemento_gerenciar_ui <- renderUI({
    choices_pontos <- setNames(rv$pontos$id, rv$pontos$legenda)
    
    choices_retas <- character(0) 
    if (length(rv$retas) > 0) {
      ids_retas <- names(rv$retas)
      nomes_retas <- sapply(rv$retas, `[[`, "legenda")
      choices_retas <- setNames(ids_retas, nomes_retas)
    }
    
    choices <- c(choices_pontos, choices_retas)
    
    selectInput("elemento_selecionado", "Elemento:", 
                choices = choices,
                selected = input$elemento_selecionado)
  })
  
  output$edicao_elemento_ui <- renderUI({
    req(input$elemento_selecionado)
    id <- input$elemento_selecionado
    
    if (grepl("^P", id)) {
      ponto <- rv$pontos[rv$pontos$id == id, ]; if(nrow(ponto) == 0) return()
      
      controles_comuns_ponto <- tagList(
        textInput(paste0("legenda_ponto_edit_", id), "Legenda:", value = ponto$legenda),
        colourInput(paste0("cor_ponto_edit_", id), "Cor:", value = ponto$cor),
        sliderInput(paste0("cex_simbolo_ponto_edit_", id), "Tamanho do símbolo:", min = 1, max = 5, value = ponto$cex_simbolo, step = 0.1),
        sliderInput(paste0("cex_fonte_ponto_edit_", id), "Tamanho da fonte:", min = 0.5, max = 3, value = ponto$cex_fonte, step = 0.1),
        checkboxInput(paste0("mostrar_legenda_ponto_edit_", id), "Mostrar legenda", value = ponto$mostrar_legenda)
      )
      
      if (ponto$type == "infinito") {
        tagList(
          controles_comuns_ponto,
          checkboxInput(paste0("visualizacao_pinf_edit_", id), "Mostrar indicador de direção", value = ponto$mostrar_visualizacao)
        )
      } else {
        controles_comuns_ponto
      }
      
    } else if (grepl("^L", id)) {
      reta <- rv$retas[[id]]; if(is.null(reta)) return()
      tagList(
        textInput(paste0("legenda_reta_edit_", id), "Legenda:", value = reta$legenda),
        colourInput(paste0("cor_reta_edit_", id), "Cor:", value = reta$cor),
        sliderInput(paste0("espessura_reta_edit_", id), "Espessura:", min = 1, max = 10, value = reta$espessura, step = 0.5),
        sliderInput(paste0("tamanho_fonte_edit_", id), "Tamanho da fonte:", min = 0.5, max = 3, value = reta$tamanho_fonte, step = 0.1),
        checkboxInput(paste0("prolongar_reta_edit_", id), "Prolongar reta", value = reta$prolongar),
        checkboxInput(paste0("mostrar_legenda_reta_edit_", id), "Mostrar legenda", value = reta$mostrar_legenda)
      )
    }
  })
  
  observe({
    req(input$elemento_selecionado)
    id <- input$elemento_selecionado
    
    if (grepl("^P", id)) {
      ponto_idx <- which(rv$pontos$id == id)
      if(length(ponto_idx) == 0) return()
      
      input_legenda_id <- paste0("legenda_ponto_edit_", id)
      if (!is.null(input[[input_legenda_id]])) {
        nova_legenda <- trimws(input[[input_legenda_id]])
        if (nova_legenda != "" && is_legenda_unica(nova_legenda, id_a_ignorar = id)) {
          rv$pontos$legenda[ponto_idx] <<- nova_legenda
        }
      }
      
      if (!is.null(input[[paste0("cor_ponto_edit_", id)]])) rv$pontos$cor[ponto_idx] <<- input[[paste0("cor_ponto_edit_", id)]]
      if (!is.null(input[[paste0("cex_simbolo_ponto_edit_", id)]])) rv$pontos$cex_simbolo[ponto_idx] <<- input[[paste0("cex_simbolo_ponto_edit_", id)]]
      if (!is.null(input[[paste0("cex_fonte_ponto_edit_", id)]])) rv$pontos$cex_fonte[ponto_idx] <<- input[[paste0("cex_fonte_ponto_edit_", id)]]
      if (!is.null(input[[paste0("mostrar_legenda_ponto_edit_", id)]])) rv$pontos$mostrar_legenda[ponto_idx] <<- input[[paste0("mostrar_legenda_ponto_edit_", id)]]
      
      if (rv$pontos$type[ponto_idx] == "infinito") {
        input_visualizacao_id <- paste0("visualizacao_pinf_edit_", id)
        if (!is.null(input[[input_visualizacao_id]])) {
          rv$pontos$mostrar_visualizacao[ponto_idx] <<- input[[input_visualizacao_id]]
        }
      }
      
    } else if (grepl("^L", id)) {
      if(is.null(rv$retas[[id]])) return()
      
      input_legenda_id <- paste0("legenda_reta_edit_", id)
      if (!is.null(input[[input_legenda_id]])) {
        nova_legenda <- trimws(input[[input_legenda_id]])
        if (nova_legenda != "" && is_legenda_unica(nova_legenda, id_a_ignorar = id)) {
          rv$retas[[id]]$legenda <<- nova_legenda
        }
      }
      
      if (!is.null(input[[paste0("cor_reta_edit_", id)]])) rv$retas[[id]]$cor <<- input[[paste0("cor_reta_edit_", id)]]
      if (!is.null(input[[paste0("espessura_reta_edit_", id)]])) rv$retas[[id]]$espessura <<- input[[paste0("espessura_reta_edit_", id)]]
      if (!is.null(input[[paste0("tamanho_fonte_edit_", id)]])) rv$retas[[id]]$tamanho_fonte <<- input[[paste0("tamanho_fonte_edit_", id)]]
      if (!is.null(input[[paste0("prolongar_reta_edit_", id)]])) rv$retas[[id]]$prolongar <<- input[[paste0("prolongar_reta_edit_", id)]]
      if (!is.null(input[[paste0("mostrar_legenda_reta_edit_", id)]])) rv$retas[[id]]$mostrar_legenda <<- input[[paste0("mostrar_legenda_reta_edit_", id)]]
    }
  })
  
  
  # --- Renderização Principal ---
  
  plot_generator <- function(margem) {
    req(rv$imagem_original)
    img_com_margem <- image_border(rv$imagem_original, "white", paste0(margem, "x", margem))
    info_canvas <- image_info(img_com_margem)
    
    par(mar = c(0, 0, 0, 0))
    plot(0, type = 'n', xlim = c(0, info_canvas$width), ylim = c(0, info_canvas$height), asp = 1, axes = FALSE, xlab = "", ylab = "")
    rasterImage(img_com_margem, 0, 0, info_canvas$width, info_canvas$height)
    
    if (nrow(rv$pontos) > 0) {
      for (i in seq_len(nrow(rv$pontos))) {
        p <- rv$pontos[i, ]
        if(p$type == "finito") {
          x_canvas <- p$x + margem; y_canvas <- p$y + margem
          points(x_canvas, y_canvas, col = p$cor, bg = p$cor, pch = 21, cex = p$cex_simbolo)
          if(p$mostrar_legenda) { text(x_canvas, y_canvas, labels = p$legenda, col = p$cor, pos = 4, offset = 0.5, cex = p$cex_fonte) }
        } else {
          if(p$mostrar_visualizacao) {
            center_x <- info_canvas$width / 2; center_y <- info_canvas$height / 2
            dir_x <- p$x; dir_y <- p$y
            t_x <- Inf; t_y <- Inf
            if(abs(dir_x) > 1e-6) t_x <- if(dir_x > 0) (info_canvas$width - center_x) / dir_x else (0 - center_x) / dir_x
            if(abs(dir_y) > 1e-6) t_y <- if(dir_y > 0) (info_canvas$height - center_y) / dir_y else (0 - center_y) / dir_y
            t_final <- min(t_x, t_y, na.rm = TRUE)
            
            x_borda <- center_x + t_final * dir_x; y_borda <- center_y + t_final * dir_y
            points(x_borda, y_borda, col = p$cor, pch = 8, cex = p$cex_simbolo)
            if(p$mostrar_legenda) { text(x_borda, y_borda, labels = p$legenda, col = p$cor, pos = 4, offset = 0.5, cex = p$cex_fonte) }
          }
        }
      }
    }
    
    if (length(rv$retas) > 0) {
      for (reta in rv$retas) {
        p1 <- rv$pontos[rv$pontos$id == reta$p1, ]; p2 <- rv$pontos[rv$pontos$id == reta$p2, ]; if(nrow(p1) == 0 || nrow(p2) == 0) next
        p1_canvas <- list(x = p1$x + margem, y = p1$y + margem); p2_canvas <- list(x = p2$x + margem, y = p2$y + margem)
        
        if (reta$prolongar) {
          if(is.infinite(reta$eq$m)) { abline(v = reta$eq$c + margem, col = reta$cor, lwd = reta$espessura, lty = "dashed")
          } else { c_canvas <- reta$eq$c + margem * (1 - reta$eq$m); abline(a = c_canvas, b = reta$eq$m, col = reta$cor, lwd = reta$espessura, lty = "dashed") }
        }
        if(p1$type == "finito" && p2$type == "finito") { segments(p1_canvas$x, p1_canvas$y, p2_canvas$x, p2_canvas$y, col = reta$cor, lwd = reta$espessura, lty = "solid") }
        
        if(reta$mostrar_legenda) {
          if(p1$type == "finito" && p2$type == "finito") { mid_x <- (p1_canvas$x + p2_canvas$x) / 2; mid_y <- (p1_canvas$y + p2_canvas$y) / 2; text(mid_x, mid_y, labels = reta$legenda, col = reta$cor, pos=3, cex=reta$tamanho_fonte)
          } else if (p1$type == "finito") { text(p1_canvas$x, p1_canvas$y, labels = reta$legenda, col = reta$cor, pos=3, cex=reta$tamanho_fonte) }
        }
      }
    }
  }
  
  output$zoomable_plot_ui <- renderUI({
    req(rv$imagem_original)
    margem <- if(is.null(input$margem_canvas)) 0 else input$margem_canvas
    info_canvas <- image_info(image_border(rv$imagem_original, "white", paste0(margem, "x", margem)))
    zoom_factor <- input$zoom_slider / 100
    plot_width <- info_canvas$width * zoom_factor; plot_height <- info_canvas$height * zoom_factor
    plotOutput("canvas_plot", width = paste0(plot_width, "px"), height = paste0(plot_height, "px"), click = "plot_click")
  })
  
  output$canvas_plot <- renderPlot({
    margem <- if(is.null(input$margem_canvas)) 0 else input$margem_canvas
    plot_generator(margem)
  })
  
  output$salvar_imagem_btn <- downloadHandler(
    filename = function() { paste("analise-geometrica-", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      req(rv$imagem_original)
      margem <- if(is.null(input$margem_canvas)) 0 else input$margem_canvas
      info_canvas <- image_info(image_border(rv$imagem_original, "white", paste0(margem, "x", margem)))
      png(file, width = info_canvas$width, height = info_canvas$height)
      plot_generator(margem)
      dev.off()
    }
  )
  
  output$log_panel_ui <- renderUI({
    log_text <- paste(rv$log, collapse = "\n")
    tagList(
      div(style="display: flex; justify-content: space-between; align-items: center;",
          h4("Log de Operações"),
          div(
            actionButton("btn_limpar_log", "Limpar Log", icon = icon("eraser"), style = "margin-left: 10px;"),
            rclipButton("clipbtn", "Copiar Log", log_text, icon = icon("clipboard"))
          )
      ),
      hr(),
      verbatimTextOutput("log_operacoes")
    )
  })
  
  output$log_operacoes <- renderText({ paste(rv$log, collapse = "\n") })
}

# --- Executar o Aplicativo ---
shinyApp(ui = ui, server = server)