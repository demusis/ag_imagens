# ##############################################################################
# Aplicativo Shiny para Análise Geométrica sobre Imagens
# ##############################################################################

# Carregamento das bibliotecas necessárias
library(shiny)
library(magick)
library(colourpicker)
library(shinythemes) # Adicionado para temas modernos

# Aumenta o limite de upload de arquivo para 30MB
options(shiny.maxRequestSize = 30*1024^2)

# --- Definição da Interface do Usuário (UI) ---
ui <- fluidPage(
  theme = shinytheme("flatly"), # Aplica o tema base "flatly"
  tags$head(
    tags$style(HTML("
            /* Paleta de Cores */
            :root {
                --royal-blue: #4169E1;
                --light-gray: #f5f5f5;
                --dark-text: #333;
                --success-green: #28a745;
                --danger-red: #dc3545;
            }

            /* Título Principal */
            .title-panel, h1 {
                color: var(--royal-blue);
                font-weight: bold;
            }

            /* Painel Lateral */
            .well {
                background-color: var(--light-gray);
                border: 1px solid #ddd;
                box-shadow: 0 2px 5px rgba(0,0,0,0.1);
                border-radius: 8px;
            }

            /* Abas */
            .nav-tabs > li > a {
                color: var(--dark-text);
            }
            .nav-tabs > li.active > a, 
            .nav-tabs > li.active > a:hover, 
            .nav-tabs > li.active > a:focus {
                color: white;
                background-color: var(--royal-blue);
                border-color: var(--royal-blue);
                font-weight: bold;
            }

            /* Botões */
            .btn-primary {
                background-color: var(--royal-blue);
                border-color: var(--royal-blue);
            }
            #btn_calcular_razao {
                background-color: var(--success-green);
                border-color: var(--success-green);
                color: white;
            }
            #btn_apagar_elemento {
                background-color: var(--danger-red);
                border-color: var(--danger-red);
                color: white;
            }

            /* Sliders */
            .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                background: var(--royal-blue);
                border-top: 1px solid var(--royal-blue);
                border-bottom: 1px solid var(--royal-blue);
            }
            .js-irs-0 .irs-from, .js-irs-0 .irs-to {
                 background: var(--royal-blue);
            }
            
            /* Contêiner do Plot */
            .plot-container {
                border: 1px solid #ddd;
                border-radius: 8px;
                box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            }
        "))
  ),
  titlePanel("Ferramenta de Análise Geométrica sobre Imagem"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
                 conditionalPanel(
                   condition = "input$acao_adicionar == 'Adicionar Ponto Finito'",
                   h5("Opções do Ponto"),
                   p("Clique na imagem para adicionar um novo ponto."),
                   colourInput("cor_ponto", "Cor:", value = "red"),
                   sliderInput("tamanho_simbolo_ponto", "Tamanho do símbolo:", min = 1, max = 5, value = 1.5, step = 0.1),
                   sliderInput("tamanho_fonte_ponto", "Tamanho da fonte:", min = 0.5, max = 3, value = 1, step = 0.1)
                 ),
                 conditionalPanel(
                   condition = "input$acao_adicionar == 'Adicionar Ponto no Infinito'",
                   h5("Definir Direção"),
                   p("Selecione dois pontos para definir a direção do ponto no infinito."),
                   uiOutput("ui_para_ponto_infinito"),
                   actionButton("btn_criar_ponto_infinito", "Criar Ponto no Infinito", class = "btn-primary")
                 ),
                 conditionalPanel(
                   condition = "input$acao_adicionar == 'Adicionar Reta'",
                   h5("Opções da Reta"),
                   uiOutput("ui_para_reta"),
                   colourInput("cor_reta", "Cor:", value = "blue"),
                   sliderInput("espessura_reta", "Espessura:", min = 1, max = 10, value = 1.5, step = 0.5),
                   sliderInput("tamanho_fonte_reta", "Tamanho da fonte:", min = 0.5, max = 3, value = 1, step = 0.1),
                   checkboxInput("prolongar_reta", "Prolongar reta nos extremos", value = FALSE),
                   actionButton("btn_criar_reta", "Criar Reta", class = "btn-primary")
                 ),
                 conditionalPanel(
                   condition = "input$acao_adicionar == 'Ponto de Interseção'",
                   h5("Opções da Interseção"),
                   uiOutput("selecao_reta_intersecao_ui"),
                   actionButton("btn_intersecao", "Encontrar Interseção", class = "btn-primary")
                 )
        ),
        tabPanel("Razão Cruzada",
                 br(),
                 h5("Medição por Escala em uma Linha"),
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
                 actionButton("btn_limpar_tudo", "Limpar Tudo", icon = icon("trash"))
        )
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "abas_main",
        tabPanel("Imagem", 
                 sliderInput("zoom_slider", "Zoom:", min = 25, max = 250, value = 100, step = 5, post = "%", width = "100%"),
                 div(class = "plot-container", style = "width: 100%; height: 600px; overflow: auto;",
                     uiOutput("zoomable_plot_ui")
                 )
        ),
        tabPanel("Log de Operações",
                 verbatimTextOutput("log_operacoes")
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
      id = character(), type = character(), # 'finito' ou 'infinito'
      x = numeric(), y = numeric(), 
      cor = character(), cex_simbolo = numeric(), cex_fonte = numeric(), 
      origem = character(),
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
  
  calcular_equacao_reta <- function(p1, p2) {
    if (abs(p2$x - p1$x) < 1e-9) { # Reta vertical
      return(list(m = Inf, c = p1$x))
    } else {
      m <- (p2$y - p1$y) / (p2$x - p1$x)
      c <- p1$y - m * p1$x
      return(list(m = m, c = c))
    }
  }
  
  distancia_euclidiana <- function(p1, p2) {
    sqrt((p1$x - p2$x)^2 + (p1$y - p2$y)^2)
  }
  
  gerar_novo_id <- function(prefixo, existentes) {
    ids_com_prefixo <- grep(paste0("^", prefixo), existentes, value = TRUE)
    if (length(ids_com_prefixo) == 0) {
      return(paste0(prefixo, 1))
    } else {
      numeros <- as.numeric(gsub(prefixo, "", ids_com_prefixo))
      return(paste0(prefixo, max(numeros, na.rm = TRUE) + 1))
    }
  }
  
  projetar_ponto_na_linha <- function(ponto, fit) {
    if(is.null(fit) || any(is.na(coef(fit)))) return(ponto)
    a <- coef(fit)[2] # slope
    b <- -1
    c <- coef(fit)[1] # intercept
    x0 <- ponto$x
    y0 <- ponto$y
    
    x_proj <- (b * (b * x0 - a * y0) - a * c) / (a^2 + b^2)
    y_proj <- (a * (-b * x0 + a * y0) - b * c) / (a^2 + b^2)
    
    return(data.frame(x = x_proj, y = y_proj))
  }
  
  # --- Observadores de Eventos ---
  
  observeEvent(input$upload_imagem, {
    req(input$upload_imagem$datapath)
    tryCatch({
      rv$imagem_original <- image_read(input$upload_imagem$datapath)
      rv$pontos <- data.frame(id=character(), type=character(), x=numeric(), y=numeric(), cor=character(), cex_simbolo=numeric(), cex_fonte=numeric(), origem=character(), stringsAsFactors=FALSE)
      rv$retas <- list()
      rv$log <- character()
      log_msg(paste0("Imagem '", input$upload_imagem$name, "' carregada."))
    }, error = function(e) { log_msg(paste("Erro:", e$message)) })
  })
  
  observeEvent(input$plot_click, {
    req(rv$imagem_original, input$acao_adicionar == "Adicionar Ponto Finito")
    margem <- input$margem_canvas
    info_img <- image_info(rv$imagem_original)
    
    x_canvas <- input$plot_click$x
    y_canvas <- input$plot_click$y
    
    x_img <- x_canvas - margem
    y_img <- y_canvas - margem
    
    if (x_img >= 0 && x_img <= info_img$width && y_img >= 0 && y_img <= info_img$height) {
      novo_id <- gerar_novo_id("P", rv$pontos$id)
      rv$pontos <- rbind(rv$pontos, data.frame(
        id = novo_id, type = "finito", x = x_img, y = y_img, 
        cor = input$cor_ponto, cex_simbolo = input$tamanho_simbolo_ponto, cex_fonte = input$tamanho_fonte_ponto, origem = "clique"
      ))
      log_msg(sprintf("Ponto Finito %s adicionado em (x=%.2f, y=%.2f).", novo_id, x_img, y_img))
    }
  })
  
  observeEvent(input$btn_criar_ponto_infinito, {
    req(input$ponto_dir1, input$ponto_dir2)
    if(input$ponto_dir1 == input$ponto_dir2) {
      log_msg("Erro: Pontos para definir a direção devem ser diferentes.")
      return()
    }
    
    p1 <- rv$pontos[rv$pontos$id == input$ponto_dir1, ]
    p2 <- rv$pontos[rv$pontos$id == input$ponto_dir2, ]
    
    vx <- p2$x - p1$x
    vy <- p2$y - p1$y
    magnitude <- sqrt(vx^2 + vy^2)
    
    if(magnitude < 1e-6) {
      log_msg("Erro: Pontos selecionados são coincidentes.")
      return()
    }
    
    vx_norm <- vx / magnitude
    vy_norm <- vy / magnitude
    
    novo_id <- gerar_novo_id("Pinf", rv$pontos$id)
    rv$pontos <- rbind(rv$pontos, data.frame(
      id = novo_id, type = "infinito", x = vx_norm, y = vy_norm,
      cor = input$cor_ponto, cex_simbolo = input$tamanho_simbolo_ponto, cex_fonte = input$tamanho_fonte_ponto,
      origem = paste0("direcao:", p1$id, ",", p2$id)
    ))
    log_msg(sprintf("Ponto no Infinito %s criado com direção (%.2f, %.2f).", novo_id, vx_norm, vy_norm))
  })
  
  observeEvent(input$btn_criar_reta, {
    req(input$ponto1_reta, input$ponto2_reta)
    if (input$ponto1_reta == input$ponto2_reta) { log_msg("Erro: Pontos para a reta devem ser diferentes."); return() }
    
    p1_data <- rv$pontos[rv$pontos$id == input$ponto1_reta, ]
    p2_data <- rv$pontos[rv$pontos$id == input$ponto2_reta, ]
    
    if(p1_data$type == "infinito" && p2_data$type == "infinito") {
      log_msg("Erro: Não é possível criar uma reta entre dois pontos no infinito."); return()
    }
    
    if(p1_data$type == "infinito") { # Garante que p1_data seja sempre o ponto finito
      temp <- p1_data; p1_data <- p2_data; p2_data <- temp
    }
    
    if (p2_data$type == "finito") {
      eq_nova <- calcular_equacao_reta(p1_data, p2_data)
    } else { # p2 é infinito
      dir_vec <- c(p2_data$x, p2_data$y)
      if(abs(dir_vec[1]) < 1e-9) { # Reta vertical
        eq_nova <- list(m = Inf, c = p1_data$x)
      } else {
        m <- dir_vec[2] / dir_vec[1]
        c <- p1_data$y - m * p1_data$x
        eq_nova <- list(m = m, c = c)
      }
    }
    
    id <- gerar_novo_id("L", names(rv$retas))
    rv$retas[[id]] <- list(id = id, p1 = p1_data$id, p2 = p2_data$id, eq = eq_nova, cor = input$cor_reta, espessura = input$espessura_reta, tamanho_fonte = input$tamanho_fonte_reta, prolongar = input$prolongar_reta)
    
    if (is.infinite(eq_nova$m)) {
      log_msg(sprintf("Reta %s criada. Equação: x = %.2f", id, eq_nova$c))
    } else {
      log_msg(sprintf("Reta %s criada. Equação: y = %.2fx + %.2f", id, eq_nova$m, eq_nova$c))
    }
  })
  
  observeEvent(input$btn_calcular_razao, {
    pontos_selecionados <- input$pontos_razao_cruzada
    if(length(pontos_selecionados) != 4) {
      rv$resultado_razao <- "<p style='color:red;'>Erro: Selecione exatamente 4 pontos.</p>"; return()
    }
    
    pontos_data <- rv$pontos[rv$pontos$id %in% pontos_selecionados, ]
    pontos_infinitos <- pontos_data[pontos_data$type == "infinito", ]
    pontos_finitos <- pontos_data[pontos_data$type == "finito", ]
    
    if(nrow(pontos_infinitos) > 1) {
      log_msg("Cálculo de Razão Cruzada: Falha. Mais de um ponto no infinito selecionado.")
      rv$resultado_razao <- "<p style='color:red;'>Erro: Selecione no máximo um ponto no infinito.</p>"; return()
    }
    
    # --- Lógica para 1 ponto no infinito (escala simples) ---
    if(nrow(pontos_infinitos) == 1) {
      req(input$ponto_ref1, input$ponto_ref2, input$ponto_alvo1, input$ponto_alvo2)
      if (input$ponto_ref1 == input$ponto_ref2 || input$ponto_alvo1 == input$ponto_alvo2) {
        rv$resultado_razao <- "<p style='color:red;'>Erro: Os pontos em um par devem ser diferentes.</p>"; return()
      }
      
      is_vertical <- diff(range(pontos_finitos$x)) < 1e-6
      if(is_vertical) {
        avg_x <- mean(pontos_finitos$x)
        pontos_projetados <- data.frame(id = pontos_finitos$id, x = avg_x, y = pontos_finitos$y)
      } else {
        fit <- lm(y ~ x, data = pontos_finitos)
        pontos_projetados <- do.call(rbind, lapply(1:nrow(pontos_finitos), function(i) {
          p_proj <- projetar_ponto_na_linha(pontos_finitos[i,], fit)
          data.frame(id = pontos_finitos$id[i], x = p_proj$x, y = p_proj$y)
        }))
      }
      
      ref1_data_proj <- pontos_projetados[pontos_projetados$id == input$ponto_ref1,]; ref2_data_proj <- pontos_projetados[pontos_projetados$id == input$ponto_ref2,]
      dist_ref_img <- distancia_euclidiana(ref1_data_proj, ref2_data_proj)
      
      if(dist_ref_img < 1e-6) { rv$resultado_razao <- "<p style='color:red;'>Erro: Distância de referência na imagem é zero.</p>"; return() }
      
      escala <- input$distancia_referencia / dist_ref_img
      log_msg(sprintf("Cálculo com 1 ponto infinito. Fator de escala: %.4f", escala))
      
      alvo1_data_proj <- pontos_projetados[pontos_projetados$id == input$ponto_alvo1,]; alvo2_data_proj <- pontos_projetados[pontos_projetados$id == input$ponto_alvo2,]
      dist_alvo_img <- distancia_euclidiana(alvo1_data_proj, alvo2_data_proj)
      dist_alvo_real <- dist_alvo_img * escala
      log_msg(sprintf("Distância alvo calculada: %.4f", dist_alvo_real))
      
      rv$resultado_razao <- paste0("<p><b>Método:</b> Escala Simples (1 Ponto Infinito)</p>", "<p><b>Fator de Escala:</b> ", sprintf("%.4f", escala), "</p>", "<p><b>Distância Alvo (Projetada):</b> ", sprintf("%.2f", dist_alvo_img), " pixels</p>", "<p><b>Distância Real Calculada:</b> <b style='color:blue; font-size:1.2em;'>", sprintf("%.4f", dist_alvo_real), "</b></p>")
      
      # --- Lógica para 0 pontos no infinito (razão cruzada completa) ---
    } else {
      req(input$dist_ref1, input$dist_ref2, input$alvo1, input$alvo2)
      
      pontos_ordenados_id <- input$pontos_razao_cruzada
      pontos_data <- pontos_data[match(pontos_ordenados_id, pontos_data$id),]
      
      is_vertical <- diff(range(pontos_data$x)) < 1e-6
      if(is_vertical) {
        avg_x <- mean(pontos_data$x)
        pontos_projetados_df <- data.frame(id = pontos_data$id, x = avg_x, y = pontos_data$y)
      } else {
        fit <- lm(y ~ x, data = pontos_data)
        pontos_projetados_df <- do.call(rbind, lapply(1:nrow(pontos_data), function(i) {
          p_proj <- projetar_ponto_na_linha(pontos_data[i,], fit)
          data.frame(id = pontos_data$id[i], x = p_proj$x, y = p_proj$y)
        }))
      }
      
      p_origem <- pontos_projetados_df[1,]
      pontos_projetados_df$d <- sapply(1:nrow(pontos_projetados_df), function(i) {
        distancia_euclidiana(p_origem, pontos_projetados_df[i,])
      })
      
      a_prime <- 0; b_prime <- pontos_projetados_df$d[2]; c_prime <- pontos_projetados_df$d[3]; d_prime <- pontos_projetados_df$d[4]
      
      K <- ((c_prime - a_prime) * (d_prime - b_prime)) / ((c_prime - b_prime) * (d_prime - a_prime))
      log_msg(sprintf("Cálculo com 4 pontos finitos. Razão Cruzada na Imagem (K) = %.4f", K))
      
      D_AB <- input$dist_ref1
      D_CD <- input$dist_ref2
      
      a_quad <- K - 1
      b_quad <- K*D_AB + K*D_CD - D_AB - D_CD
      c_quad <- - D_AB * D_CD 
      
      if (abs(a_quad) < 1e-9) { # Caso linear (K=1, sem perspectiva)
        if (abs(b_quad) < 1e-9) {
          rv$resultado_razao <- "<p style='color:red;'>Erro: Configuração degenerada (K=1 e referências inconsistentes).</p>"; return()
        }
        x <- -c_quad / b_quad
      } else { # Caso quadrático
        delta <- b_quad^2 - 4*a_quad*c_quad
        if (delta < 0) {
          rv$resultado_razao <- "<p style='color:red;'>Erro: Nenhuma solução real encontrada (delta < 0).</p>"; return()
        }
        x1 <- (-b_quad + sqrt(delta)) / (2*a_quad)
        x2 <- (-b_quad - sqrt(delta)) / (2*a_quad)
        x <- ifelse(x1 > 0, x1, x2) # Pega a raiz positiva
      }
      
      D_BC <- x
      
      mapa_coords_reais <- list(0, D_AB, D_AB + D_BC, D_AB + D_BC + D_CD)
      names(mapa_coords_reais) <- pontos_ordenados_id
      
      dist_final <- abs(mapa_coords_reais[[input$alvo2]] - mapa_coords_reais[[input$alvo1]])
      log_msg(sprintf("Distância real D(%s, %s) calculada: %.4f", input$alvo1, input$alvo2, dist_final))
      
      rv$resultado_razao <- paste0("<p><b>Método:</b> Razão Cruzada (4 Pontos Finitos)</p>", "<p><b>Razão Cruzada (K):</b> ", sprintf("%.4f", K), "</p>", "<p><b>Distância Interna D(P2,P3) Estimada:</b> ", sprintf("%.4f", D_BC), "</p>", "<p><b>Distância Real D(",input$alvo1,", ",input$alvo2,") Calculada:</b> <b style='color:blue; font-size:1.2em;'>", sprintf("%.4f", dist_final), "</b></p>")
    }
  })
  
  observeEvent(input$btn_intersecao, {
    req(input$reta1_intersecao, input$reta2_intersecao)
    if (input$reta1_intersecao == input$reta2_intersecao) { log_msg("Erro: Retas devem ser diferentes."); return() }
    
    reta1 <- rv$retas[[input$reta1_intersecao]]; reta2 <- rv$retas[[input$reta2_intersecao]]
    m1 <- reta1$eq$m; c1 <- reta1$eq$c; m2 <- reta2$eq$m; c2 <- reta2$eq$c
    
    if (abs(m1 - m2) < 1e-9) { log_msg("As retas são paralelas."); return() }
    
    if (is.infinite(m1)) { x_inter <- c1; y_inter <- m2 * x_inter + c2
    } else if (is.infinite(m2)) { x_inter <- c2; y_inter <- m1 * x_inter + c1
    } else { x_inter <- (c2 - c1) / (m1 - m2); y_inter <- m1 * x_inter + c1 }
    
    novo_id <- gerar_novo_id("P", rv$pontos$id)
    rv$pontos <- rbind(rv$pontos, data.frame(
      id = novo_id, type = "finito", x = x_inter, y = y_inter, 
      cor = input$cor_ponto, cex_simbolo = input$tamanho_simbolo_ponto, cex_fonte = input$tamanho_fonte_ponto, 
      origem = paste0("intersecao:", reta1$id, ",", reta2$id)
    ))
    log_msg(sprintf("Ponto de interseção %s criado em (x=%.2f, y=%.2f).", novo_id, x_inter, y_inter))
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
          if ((!is.null(reta$p1) && reta$p1 == id) || (!is.null(reta$p2) && reta$p2 == id)) {
            encontrar_dependentes(reta_id)
          }
        }
        for(i in seq_len(nrow(rv$pontos))) {
          if(rv$pontos$type[i] == "infinito" && grepl(paste0("\\b", id, "\\b"), rv$pontos$origem[i])) {
            encontrar_dependentes(rv$pontos$id[i])
          }
        }
      } else if (grepl("^L", id) && !(id %in% elementos_a_apagar$retas)) {
        elementos_a_apagar$retas <<- c(elementos_a_apagar$retas, id)
        for (i in seq_len(nrow(rv$pontos))) {
          if (rv$pontos$type[i] == "finito" && grepl(paste0("\\b", id, "\\b"), rv$pontos$origem[i])) {
            encontrar_dependentes(rv$pontos$id[i])
          }
        }
      }
    }
    
    encontrar_dependentes(id_apagar)
    
    if (length(elementos_a_apagar$pontos) > 0) {
      rv$pontos <- rv$pontos[!rv$pontos$id %in% elementos_a_apagar$pontos, ]
      log_msg(paste("Pontos apagados:", paste(elementos_a_apagar$pontos, collapse=", ")))
    }
    if (length(elementos_a_apagar$retas) > 0) {
      rv$retas[elementos_a_apagar$retas] <- NULL
      log_msg(paste("Retas apagadas:", paste(elementos_a_apagar$retas, collapse=", ")))
    }
  })
  
  observeEvent(input$btn_limpar_tudo, {
    rv$pontos <- data.frame(id=character(), type=character(), x=numeric(), y=numeric(), cor=character(), cex_simbolo=numeric(), cex_fonte=numeric(), origem=character(), stringsAsFactors=FALSE)
    rv$retas <- list()
    log_msg("Todas as anotações foram limpas.")
  })
  
  # --- Renderização da UI Dinâmica ---
  
  output$ui_margem_slider <- renderUI({
    max_dim <- 0
    if(!is.null(rv$imagem_original)) {
      info <- image_info(rv$imagem_original)
      max_dim <- max(info$width, info$height)
    }
    sliderInput("margem_canvas", "Tamanho da margem (pixels):", min = 0, max = max_dim, value = 0, step = 10)
  })
  
  observe({
    updateSelectizeInput(session, "pontos_razao_cruzada", choices = rv$pontos$id, server = TRUE)
  })
  
  output$ui_razao_cruzada_controles <- renderUI({
    req(input$pontos_razao_cruzada)
    pontos_data <- rv$pontos[rv$pontos$id %in% input$pontos_razao_cruzada, ]
    pontos_finitos_selecionados <- pontos_data$id[pontos_data$type == "finito"]
    
    if(any(pontos_data$type == "infinito")) { # Caso 1: 1 ponto infinito
      tagList(
        h5("Distância de Referência"),
        selectInput("ponto_ref1", "Ponto de Referência 1:", choices = pontos_finitos_selecionados),
        selectInput("ponto_ref2", "Ponto de Referência 2:", choices = pontos_finitos_selecionados, selected = if(length(pontos_finitos_selecionados)>1) pontos_finitos_selecionados[2] else NULL),
        numericInput("distancia_referencia", "Distância Real de Referência:", value = 1),
        hr(),
        h5("Distância a Calcular"),
        selectInput("ponto_alvo1", "Ponto Alvo 1:", choices = pontos_finitos_selecionados, selected = if(length(pontos_finitos_selecionados)>2) pontos_finitos_selecionados[3] else NULL),
        selectInput("ponto_alvo2", "Ponto Alvo 2:", choices = pontos_finitos_selecionados, selected = if(length(pontos_finitos_selecionados)>3) pontos_finitos_selecionados[4] else NULL)
      )
    } else { # Caso 2: 4 pontos finitos
      tagList(
        p("Selecione os 4 pontos na ordem em que aparecem na linha."),
        h5("Distâncias de Referência"),
        numericInput("dist_ref1", "Distância Real (Ponto 1 -> Ponto 2):", value = 1),
        numericInput("dist_ref2", "Distância Real (Ponto 3 -> Ponto 4):", value = 1),
        hr(),
        h5("Distância a Calcular"),
        selectInput("alvo1", "Ponto 1:", choices = input$pontos_razao_cruzada),
        selectInput("alvo2", "Ponto 2:", choices = input$pontos_razao_cruzada, selected = if(length(input$pontos_razao_cruzada)>1) input$pontos_razao_cruzada[2] else NULL)
      )
    }
  })
  
  output$resultado_razao_cruzada <- renderUI({ HTML(rv$resultado_razao) })
  
  output$ui_para_ponto_infinito <- renderUI({
    pontos_finitos <- rv$pontos$id[rv$pontos$type == "finito"]
    if (length(pontos_finitos) < 2) { p("São necessários pelo menos 2 pontos finitos.") }
    else { tagList(
      selectInput("ponto_dir1", "Ponto de Origem:", choices = pontos_finitos),
      selectInput("ponto_dir2", "Ponto de Destino:", choices = pontos_finitos, selected = if(length(pontos_finitos) > 1) pontos_finitos[2] else NULL)
    )}
  })
  
  output$ui_para_reta <- renderUI({
    if (nrow(rv$pontos) < 2) { p("São necessários pelo menos 2 pontos.") }
    else { tagList(selectInput("ponto1_reta", "Ponto 1:", choices = rv$pontos$id), selectInput("ponto2_reta", "Ponto 2:", choices = rv$pontos$id, selected = if(nrow(rv$pontos) > 1) rv$pontos$id[2] else NULL)) }
  })
  
  output$selecao_reta_intersecao_ui <- renderUI({
    if (length(rv$retas) < 2) { p("São necessárias pelo menos 2 retas.") }
    else { tagList(selectInput("reta1_intersecao", "Reta 1:", choices = names(rv$retas)), selectInput("reta2_intersecao", "Reta 2:", choices = names(rv$retas), selected = if(length(rv$retas) > 1) names(rv$retas)[2] else NULL)) }
  })
  
  output$selecao_elemento_gerenciar_ui <- renderUI({
    choices <- c(rv$pontos$id, names(rv$retas))
    selectInput("elemento_selecionado", "Elemento:", choices = choices)
  })
  
  output$edicao_elemento_ui <- renderUI({
    req(input$elemento_selecionado)
    id <- input$elemento_selecionado
    
    if (grepl("^P", id)) {
      ponto <- rv$pontos[rv$pontos$id == id, ]
      if(nrow(ponto) == 0) return()
      tagList(
        colourInput(paste0("cor_ponto_edit_", id), "Cor:", value = ponto$cor),
        sliderInput(paste0("cex_simbolo_ponto_edit_", id), "Tamanho do símbolo:", min = 1, max = 5, value = ponto$cex_simbolo, step = 0.1),
        sliderInput(paste0("cex_fonte_ponto_edit_", id), "Tamanho da fonte:", min = 0.5, max = 3, value = ponto$cex_fonte, step = 0.1)
      )
    } else if (grepl("^L", id)) {
      reta <- rv$retas[[id]]
      if(is.null(reta)) return()
      tagList(
        colourInput(paste0("cor_reta_edit_", id), "Cor:", value = reta$cor),
        sliderInput(paste0("espessura_reta_edit_", id), "Espessura:", min = 1, max = 10, value = reta$espessura, step = 0.5),
        sliderInput(paste0("tamanho_fonte_edit_", id), "Tamanho da fonte:", min = 0.5, max = 3, value = reta$tamanho_fonte, step = 0.1),
        checkboxInput(paste0("prolongar_reta_edit_", id), "Prolongar reta", value = reta$prolongar)
      )
    }
  })
  
  observe({
    req(input$elemento_selecionado)
    id <- input$elemento_selecionado
    
    if (grepl("^P", id)) {
      ponto_idx <- which(rv$pontos$id == id)
      if(length(ponto_idx) == 0) return()
      
      input_cor_id <- paste0("cor_ponto_edit_", id)
      if (!is.null(input[[input_cor_id]])) rv$pontos$cor[ponto_idx] <<- input[[input_cor_id]]
      input_cex_simbolo_id <- paste0("cex_simbolo_ponto_edit_", id)
      if (!is.null(input[[input_cex_simbolo_id]])) rv$pontos$cex_simbolo[ponto_idx] <<- input[[input_cex_simbolo_id]]
      input_cex_fonte_id <- paste0("cex_fonte_ponto_edit_", id)
      if (!is.null(input[[input_cex_fonte_id]])) rv$pontos$cex_fonte[ponto_idx] <<- input[[input_cex_fonte_id]]
      
    } else if (grepl("^L", id)) {
      if(is.null(rv$retas[[id]])) return()
      
      input_cor_id <- paste0("cor_reta_edit_", id)
      if (!is.null(input[[input_cor_id]])) rv$retas[[id]]$cor <<- input[[input_cor_id]]
      input_esp_id <- paste0("espessura_reta_edit_", id)
      if (!is.null(input[[input_esp_id]])) rv$retas[[id]]$espessura <<- input[[input_esp_id]]
      input_font_id <- paste0("tamanho_fonte_edit_", id)
      if (!is.null(input[[input_font_id]])) rv$retas[[id]]$tamanho_fonte <<- input[[input_font_id]]
      input_prolongar_id <- paste0("prolongar_reta_edit_", id)
      if (!is.null(input[[input_prolongar_id]])) rv$retas[[id]]$prolongar <<- input[[input_prolongar_id]]
    }
  })
  
  # --- Renderização Principal ---
  
  output$zoomable_plot_ui <- renderUI({
    req(rv$imagem_original)
    margem <- if(is.null(input$margem_canvas)) 0 else input$margem_canvas
    info_canvas <- image_info(image_border(rv$imagem_original, "white", paste0(margem, "x", margem)))
    
    zoom_factor <- input$zoom_slider / 100
    
    plot_width <- info_canvas$width * zoom_factor
    plot_height <- info_canvas$height * zoom_factor
    
    plotOutput("canvas_plot", width = paste0(plot_width, "px"), height = paste0(plot_height, "px"), click = "plot_click")
  })
  
  output$canvas_plot <- renderPlot({
    req(rv$imagem_original)
    margem <- if(is.null(input$margem_canvas)) 0 else input$margem_canvas
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
          text(x_canvas, y_canvas, labels = p$id, col = p$cor, pos = 4, offset = 0.5, cex = p$cex_fonte)
        } else { # Ponto no infinito
          center_x <- info_canvas$width / 2; center_y <- info_canvas$height / 2
          dir_x <- p$x; dir_y <- p$y
          t_x <- Inf; t_y <- Inf
          if(abs(dir_x) > 1e-6) t_x <- if(dir_x > 0) (info_canvas$width - center_x) / dir_x else (0 - center_x) / dir_x
          if(abs(dir_y) > 1e-6) t_y <- if(dir_y > 0) (info_canvas$height - center_y) / dir_y else (0 - center_y) / dir_y
          t_final <- min(t_x, t_y, na.rm = TRUE)
          
          x_borda <- center_x + t_final * dir_x; y_borda <- center_y + t_final * dir_y
          points(x_borda, y_borda, col = p$cor, pch = 8, cex = p$cex_simbolo)
          text(x_borda, y_borda, labels = p$id, col = p$cor, pos = 4, offset = 0.5, cex = p$cex_fonte)
        }
      }
    }
    
    if (length(rv$retas) > 0) {
      for (reta in rv$retas) {
        p1 <- rv$pontos[rv$pontos$id == reta$p1, ]; p2 <- rv$pontos[rv$pontos$id == reta$p2, ]
        if(nrow(p1) == 0 || nrow(p2) == 0) next
        
        p1_canvas <- list(x = p1$x + margem, y = p1$y + margem)
        p2_canvas <- list(x = p2$x + margem, y = p2$y + margem)
        
        if (reta$prolongar) {
          if(is.infinite(reta$eq$m)) {
            abline(v = reta$eq$c + margem, col = reta$cor, lwd = reta$espessura, lty = "dashed")
          } else {
            c_canvas <- reta$eq$c + margem * (1 - reta$eq$m)
            abline(a = c_canvas, b = reta$eq$m, col = reta$cor, lwd = reta$espessura, lty = "dashed")
          }
          if(p1$type == "finito" && p2$type == "finito") {
            segments(p1_canvas$x, p1_canvas$y, p2_canvas$x, p2_canvas$y, col = reta$cor, lwd = reta$espessura, lty = "solid")
          }
        } else {
          if(p1$type == "finito" && p2$type == "finito") {
            segments(p1_canvas$x, p1_canvas$y, p2_canvas$x, p2_canvas$y, col = reta$cor, lwd = reta$espessura, lty = "solid")
          }
        }
        
        if(p1$type == "finito" && p2$type == "finito") {
          mid_x <- (p1_canvas$x + p2_canvas$x) / 2; mid_y <- (p1_canvas$y + p2_canvas$y) / 2
          text(mid_x, mid_y, labels = reta$id, col = reta$cor, pos=3, cex=reta$tamanho_fonte)
        } else if (p1$type == "finito") {
          text(p1_canvas$x, p1_canvas$y, labels = reta$id, col = reta$cor, pos=3, cex=reta$tamanho_fonte)
        }
      }
    }
  })
  
  output$log_operacoes <- renderText({ paste(rv$log, collapse = "\n") })
}

shinyApp(ui = ui, server = server)
