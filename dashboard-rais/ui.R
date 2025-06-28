# ui.R

library(shiny)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)

ui <- navbarPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      .shiny-input-container {
        margin: 0;
      }"))
  ),
  title = "Vínculos RAIS",
  
  # =================================================================
  # Aba TABELA
  # =================================================================
  tabPanel("Tabela",
           fluidPage(
             class = "container",
             fluidRow(
               column(12,
                      h3("Médias por variáveis do emprego"),
                      # A mudança é a adição do argumento 'style' nesta linha
                      wellPanel(
                        fluidRow(style = "display: flex; align-items: flex-end;",
                                 column(5,
                                        pickerInput("agrupadores", "Variáveis de agrupamento:",
                                                    choices = NULL, multiple = TRUE,
                                                    selected = "Cargo",
                                                    options = list(actionsBox = TRUE,
                                                                   `live-search` = TRUE,
                                                                   `none-selected-text` = "Selecione uma variável",
                                                                   `deselect-all-text` = "Remover todos",
                                                                   `select-all-text` = "Selecionar todos"))
                                 ),
                                 column(5,
                                        pickerInput("variavel_numerica", "Variável numérica:",
                                                    choices = NULL, multiple = FALSE)
                                 ),
                                 column(2, 
                                        downloadButton("baixar_tab1", "Baixar CSV")
                                 )
                        )
                      )
               )
             ),
             fluidRow(
               DTOutput("tabela_dados") %>%
                 withSpinner(type = 6, color = "#3c8dbc")
             ),
             hr(),
             fluidRow(
               column(12,
                      h3("Médias de remuneração por curso"),
                      wellPanel(
                        fluidRow(
                          style = "display: flex; align-items: flex-end;",
                          column(5,
                                 pickerInput("agrupadores1", "Variáveis de agrupamento:",
                                             choices = NULL, multiple = TRUE,
                                             selected = "Cargo",
                                             options = list(actionsBox = TRUE,
                                                            `live-search` = TRUE,
                                                            `none-selected-text` = "Selecione uma variável",
                                                            `deselect-all-text` = "Remover todos",
                                                            `select-all-text` = "Selecionar todos"))
                          ),
                          column(5,
                                 pickerInput("selecionar_curso", "Selecione o curso:",
                                             choices = NULL, multiple = TRUE)
                          ),
                          column(2, downloadButton("baixar_tab2", "Baixar CSV"))
                        )
                      )
               )
             ),
             fluidRow(
               DTOutput("tabela_dados2") %>%
                 withSpinner(type = 6, color = "#3c8dbc")
             )
           )
  ),
  
  # =================================================================
  # Aba GRÁFICOS
  # =================================================================
  tabPanel("Gráficos",
           fluidPage(
             class = "container",
             # --- Seções dos gráficos anteriores (Barras, Boxplot, Histograma) ---
             fluidRow(
               column(12, h3("Média Salarial por Categoria (Gráfico de Barras)"),
                      wellPanel(fluidRow(column(4, pickerInput("graf_agrup", "Agrupar por:", choices = NULL, multiple = FALSE, options = list(`live-search` = TRUE))))))
             ),
             fluidRow(column(12, plotlyOutput("grafico_barras") %>% withSpinner(type = 6, color = "#3c8dbc"))),
             hr(),
             fluidRow(
               column(12, h3("Distribuição por Categoria (Box Plot)"),
                      wellPanel(fluidRow(
                        column(4, pickerInput("boxplot_categorica", "Variável Categórica (Eixo X):", choices = NULL, multiple = FALSE, options = list(`live-search` = TRUE))),
                        column(4, pickerInput("boxplot_numerica", "Variável Numérica (Eixo Y):", choices = NULL, multiple = FALSE, options = list(`live-search` = TRUE)))
                      )))
             ),
             fluidRow(column(12, plotlyOutput("grafico_boxplot") %>% withSpinner(type = 6, color = "#3c8dbc"))),
             hr(),
             fluidRow(
               column(12, h3("Histograma de Remuneração por Categoria"),
                      wellPanel(fluidRow(column(6, pickerInput("hist_categorica", "Dividir por variável categórica:", choices = NULL, multiple = FALSE, options = list(`live-search` = TRUE))))))
             ),
             fluidRow(column(12, plotlyOutput("grafico_histograma") %>% withSpinner(type = 6, color = "#3c8dbc"))),
             hr(),
             fluidRow(
               column(12, h3("Mapa de Remuneração Média por Estado"),
                      wellPanel(p("O mapa abaixo exibe a média da remuneração nominal para cada estado do Brasil.")))
             ),
             fluidRow(column(12, plotlyOutput("mapa_choro_brasil", height = "600px") %>% withSpinner(type = 6, color = "#3c8dbc"))),
             
             hr(),
             
             fluidRow(
               column(12,
                      h3("Mapa de Salário Médio por Município"),
                      wellPanel(
                        fluidRow(
                          column(6,
                                 pickerInput(
                                   inputId = "estado_selecionado_municipio",
                                   label = "Selecione um Estado para visualizar o mapa:",
                                   choices = NULL,
                                   options = list(`live-search` = TRUE)
                                 )
                          )
                        )
                      )
               )
             ),
             fluidRow(
               column(12,
                      plotlyOutput("grafico_salario_municipios", height = "600px") %>%
                        withSpinner(type = 6, color = "#3c8dbc")
               )
             )
           )
  )
)