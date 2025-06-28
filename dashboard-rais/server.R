# server.R

library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(plotly)
library(formattable)
library(geobr)
library(ggplot2)

# =================================================================
# Carregamento dos Dados e Listas Iniciais
# =================================================================
dados <- read.csv("../dados_sensiveis/dados_rais_limpos.csv")
traducao <- read.csv("../dados_sensiveis/traducao.csv")
cursos <- read.csv("../dados_sensiveis/cursos_empregos.csv")

all_states_info <- geobr::read_state(year = 2020) %>%
  as.data.frame() %>%
  dplyr::select(name_state, abbrev_state, code_state) %>%
  dplyr::arrange(name_state)

display_labels <- paste0(all_states_info$name_state, " (", all_states_info$abbrev_state, ")")
choices_estados <- setNames(all_states_info$code_state, display_labels)


server <- function(input, output, session) {
  
  # =================================================================
  # Inicialização e Atualização dos Inputs
  # =================================================================
  nomes_legiveis <- setNames(traducao$nome_real, traducao$nome_coluna)
  nomes_reais <- setNames(traducao$nome_coluna, traducao$nome_real)
  cursos_disponiveis <- sort(unique(cursos$curso_ies))
  
  observe({
    agrupaveis <- traducao %>% filter(tipo_coluna %in% c("texto", "fator"),
                                      !(nome_coluna %in% c('cbo', 'code_region_municipio', 'code_state_municipio', 'code_muni_municipio', 'name_state_municipio')))
    numericas  <- traducao %>% filter(tipo_coluna == "número")
    agrupaveis_curso <- traducao %>% filter(tipo_coluna %in% c("texto", "fator"),
                                            !(nome_coluna %in% c('cbo', 'cargo', 'code_region_municipio', 'code_state_municipio', 'code_muni_municipio', 'name_state_municipio')))
    
    updatePickerInput(session, "agrupadores", choices = agrupaveis$nome_real, selected = "Cargo")
    updatePickerInput(session, "variavel_numerica", choices = numericas$nome_real, selected = "Remuneração média nominal")
    updatePickerInput(session, "agrupadores1", choices = agrupaveis_curso$nome_real)
    updatePickerInput(session, "selecionar_curso", choices = cursos_disponiveis, selected = cursos_disponiveis[1:5])
    updatePickerInput(session, "graf_agrup", choices = agrupaveis$nome_real, selected = "Sexo")
    updatePickerInput(session, "boxplot_categorica", choices = agrupaveis$nome_real, selected = "Sexo")
    updatePickerInput(session, "boxplot_numerica", choices = numericas$nome_real, selected = "Remuneração média nominal")
    updatePickerInput(session, "hist_categorica", choices = agrupaveis$nome_real, selected = "Sexo")
    updatePickerInput(session, "estado_selecionado_municipio", choices = choices_estados, selected = 33)
  })
  
  # =================================================================
  # Lógica das Tabelas e Downloads
  # =================================================================
  
  output$tabela_dados <- DT::renderDataTable({
    req(input$variavel_numerica)
    col_agrup <- nomes_reais[input$agrupadores]
    col_num   <- nomes_reais[input$variavel_numerica]
    df <- dados
    if (length(col_agrup) > 0) {
      df <- df %>%
        group_by(across(all_of(col_agrup))) %>%
        summarise(Quantidade = n(), Média = mean(.data[[col_num]], na.rm = TRUE), Mediana = median(.data[[col_num]], na.rm = TRUE), .groups = "drop")
    } else {
      df <- df %>%
        summarise(Quantidade = n(), Mediana = median(.data[[col_num]], na.rm = TRUE), Média = mean(.data[[col_num]], na.rm = TRUE))
    }
    colnames(df) <- sapply(colnames(df), function(col) { if (col %in% names(nomes_legiveis)) nomes_legiveis[[col]] else col })
    num_cols <- intersect(c("Média", "Mediana"), colnames(df))
    df[num_cols] <- lapply(df[num_cols], function(x) format(round(as.numeric(x), 2), big.mark = ".", decimal.mark = ","))
    datatable(df, options = list(scrollX = TRUE, searching = TRUE, language = list(search = "Buscar:", paginate = list(previous = 'Anterior', `next` = 'Próxima'), info = "Mostrando _START_ a _END_ de _TOTAL_ registros", lengthMenu = "Mostrar _MENU_ registros")), class = 'stripe hover')
  })
  
  # Definição do df_tab1 reativo para o download
  df_tab1 <- reactive({
    req(input$variavel_numerica)
    col_agrup <- nomes_reais[input$agrupadores]
    col_num   <- nomes_reais[input$variavel_numerica]
    if (length(col_agrup) > 0) {
      dados %>% 
        group_by(across(all_of(col_agrup))) %>% 
        summarise(
          Quantidade = n(), 
          Media = mean(.data[[col_num]], na.rm = TRUE), 
          Mediana = median(.data[[col_num]], na.rm = TRUE), 
          .groups = "drop"
        )
    } else {
      dados %>% 
        summarise(
          Quantidade = n(), 
          Media = mean(.data[[col_num]], na.rm = TRUE), 
          Mediana = median(.data[[col_num]], na.rm = TRUE)
        )
    }
  })
  
  output$baixar_tab1 <- downloadHandler(
    filename = function() paste0("tabela_emprego_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(df_tab1(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  output$tabela_dados2 <- DT::renderDataTable({
    col_num <- "vl_remun_media_nom"
    col_agrup_user <- if (length(input$agrupadores1) == 0) character(0) else nomes_reais[input$agrupadores1]
    col_agrup <- unique(c("curso_ies", col_agrup_user))
    
    df <- dados %>%
      inner_join(cursos, by = "cbo") %>%
      filter(!is.na(.data[[col_num]]), !is.na(curso_ies), curso_ies %in% input$selecionar_curso) %>%
      group_by(across(all_of(col_agrup))) %>%
      summarise(Quantidade = n(), Média = mean(.data[[col_num]], na.rm = TRUE), Mediana = median(.data[[col_num]], na.rm=TRUE), .groups = "drop") %>%
      distinct()
    
    colnames(df) <- sapply(colnames(df), function(col) { if (col %in% names(nomes_legiveis)) nomes_legiveis[[col]] else col })
    num_cols <- intersect(c("Média", "Mediana"), colnames(df))
    df[num_cols] <- lapply(df[num_cols], function(x) format(round(as.numeric(x), 2), big.mark = ".", decimal.mark = ","))
    
    datatable(df, options = list(scrollX = TRUE, searching = TRUE, language = list(emptyTable = "Não há dados disponíveis", info = "Mostrando _START_ a _END_ de _TOTAL_ linhas", infoEmpty = "Mostrando 0 a 0 de 0 linhas", zeroRecords = "Sem resultados", search = "Buscar:", searchPlaceholder = "sistemas mulher", lengthMenu = "Mostrar _MENU_ linhas", paginate = list(first = "Primeira", last = "Última", `next` = "Próxima", previous = "Anterior"))), class = 'stripe hover')
  })
  
  # Definição do df_tab2 reativo para o download
  df_tab2 <- reactive({
    col_num <- "vl_remun_media_nom"
    col_agrup_user <- if (length(input$agrupadores1) == 0) character(0) else nomes_reais[input$agrupadores1]
    col_agrup <- unique(c("curso_ies", col_agrup_user))
    dados %>%
      inner_join(cursos, by = "cbo") %>%
      filter(!is.na(.data[[col_num]]), !is.na(curso_ies), curso_ies %in% input$selecionar_curso) %>%
      group_by(across(all_of(col_agrup))) %>%
      summarise(
        Quantidade = n(), 
        Media = mean(.data[[col_num]], na.rm = TRUE), 
        Mediana = median(.data[[col_num]], na.rm = TRUE), 
        .groups = "drop"
      ) %>%
      distinct()
  })
  ### FIM DA CORREÇÃO ###
  
  output$baixar_tab2 <- downloadHandler(
    filename = function() paste0("tabela_cursos_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(df_tab2(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  
  # =================================================================
  # Lógica dos Gráficos
  # =================================================================
  
  output$grafico_barras <- renderPlotly({
    req(input$graf_agrup)
    col_agrup <- nomes_reais[input$graf_agrup]
    col_salario <- "vl_remun_media_nom"
    plot_df <- dados %>%
      filter(!is.na(.data[[col_agrup]]), !is.na(.data[[col_salario]])) %>%
      group_by(.data[[col_agrup]]) %>%
      summarise(Media_Salario = mean(.data[[col_salario]], na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Media_Salario)) %>%
      mutate(!!sym(col_agrup) := factor(.data[[col_agrup]], levels = .data[[col_agrup]]))
    plot_ly(plot_df, x = ~.data[[col_agrup]], y = ~Media_Salario, type = "bar", text = ~paste0("R$ ", format(round(Media_Salario, 2), big.mark = ".", decimal.mark = ",")), textposition = "auto", marker = list(color = '#3c8dbc')) %>%
      layout(title = paste("Média de Salário por", input$graf_agrup), xaxis = list(title = input$graf_agrup), yaxis = list(title = "Média Salarial"), margin = list(b = 100))
  })
  
  output$grafico_boxplot <- renderPlotly({
    req(input$boxplot_categorica, input$boxplot_numerica)
    col_cat <- nomes_reais[input$boxplot_categorica]
    col_num <- nomes_reais[input$boxplot_numerica]
    plot_df <- dados %>% filter(!is.na(.data[[col_cat]]) & !is.na(.data[[col_num]]))
    plot_ly(plot_df, x = ~.data[[col_cat]], y = ~.data[[col_num]], type = "box", color = ~.data[[col_cat]]) %>%
      layout(title = paste("Distribuição de", input$boxplot_numerica, "por", input$boxplot_categorica), xaxis = list(title = input$boxplot_categorica), yaxis = list(title = input$boxplot_numerica), showlegend = FALSE)
  })
  
  output$grafico_histograma <- renderPlotly({
    req(input$hist_categorica)
    col_num <- "vl_remun_media_nom"
    col_cat <- nomes_reais[input$hist_categorica]
    plot_df <- dados %>% filter(!is.na(.data[[col_num]]) & !is.na(.data[[col_cat]]))
    plot_ly(plot_df, x = ~.data[[col_num]], color = ~.data[[col_cat]], type = "histogram", alpha = 0.6) %>%
      layout(barmode = "overlay", title = paste("Distribuição da Remuneração Média Nominal por", input$hist_categorica), xaxis = list(title = "Remuneração Média Nominal (R$)"), yaxis = list(title = "Frequência"), legend = list(title = list(text = paste("<b>", input$hist_categorica, "</b>"))))
  })
  
  output$mapa_choro_brasil <- renderPlotly({
    mapa_brasil_sf <- read_state(year = 2020)
    dados_estados <- dados %>% group_by(code_state_municipio) %>%
      summarise(media_remun_estado = mean(vl_remun_media_nom, na.rm = TRUE),
                qtd = n(),
                .groups = 'drop')
    mapa_com_dados_estados <- left_join(mapa_brasil_sf, dados_estados, by = c("code_state" = "code_state_municipio"))
    mapa_com_dados_estados <- mapa_com_dados_estados %>%
      mutate(texto_hover = paste("Estado:", name_state, "<br>",
                                 "Quantidade de vínculos:", qtd, "<br>",
                                 "Remuneração Média: R$", format(round(media_remun_estado, 2), nsmall = 2, big.mark = ".", decimal.mark = ",")))
    p <- ggplot(mapa_com_dados_estados) + geom_sf(aes(fill = media_remun_estado, text = texto_hover), color = "white", size = 0.1) + scale_fill_viridis_c(option = "A", name = "Remuneração Média (R$)") + labs(title = "Remuneração Média Nominal por Estado") + theme_void()
    ggplotly(p, tooltip = "text") %>% layout(title = list(x = 0.5))
  })
  
  output$grafico_salario_municipios <- renderPlotly({
    req(input$estado_selecionado_municipio)
    codigo_estado <- as.numeric(input$estado_selecionado_municipio)
    
    nome_estado_selecionado <- names(choices_estados[choices_estados == codigo_estado])
    
    dados_agregados_muni <- dados %>%
      filter(code_state_municipio == codigo_estado) %>%
      group_by(code_muni_municipio) %>%
      summarise(media_remun_muni = mean(vl_remun_media_nom, na.rm = TRUE),
                qtd = n(),
                .groups = 'drop') %>%
      filter(!is.na(media_remun_muni))
    
    municipios_sf <- read_municipality(code_muni = codigo_estado, year = 2020)
    
    plot_data <- left_join(
      municipios_sf,
      dados_agregados_muni,
      by = c("code_muni" = "code_muni_municipio")
    ) %>%
      filter(!is.na(media_remun_muni)) %>%
      mutate(
        texto_hover = paste(
          "Município:", name_muni, "<br>",
          "Quantidade de vínculos:", qtd,"<br>",
          "Salário Médio: R$", format(round(media_remun_muni, 2), nsmall = 2, big.mark = ".", decimal.mark = ",")
        )
      )
    
    p <- ggplot(plot_data) +
      geom_sf(aes(fill = media_remun_muni, text = texto_hover), 
              color = "white",
              size = 0.1) +
      scale_fill_viridis_c(option = "A", name = "Salário Médio (R$)") +
      labs(
        title = paste("Salário Médio por Município em", nome_estado_selecionado)
      ) +
      theme_void()
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        title = list(x = 0.5)
      )
  })
  
}