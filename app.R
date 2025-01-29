library(kableExtra)
library(fontawesome)
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(dplyr)


#Lâmina (mm/m²)
lamina1 = function(ET0, kc){
  return(ET0*kc)
}


#Lâmina (m³/ha)
lamina2 = function(ET0, kc){
  return(ET0*kc*10)
}





#Horas de irrigação (h)
irriga = function(ET0, dist_linha, dist_gotej, vazao_gotej, CE, kc) {
  return(((((kc*ET0)*10)/((10000/dist_linha)*(1/dist_gotej)*(vazao_gotej/1000)))*(1+(CE*0.05))))
}





sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("🍈 Melão", tabName = "melao"),
    menuItem("🍉 Melancia", tabName = "melancia")
  )
)


body <- dashboardBody(
  
  tags$head(
    tags$style(HTML("
    .skin-blue .main-header .navbar {
      background-color: #548238;
    }
    .skin-blue .main-header .logo {
      background-color: #385723;
    }
    .skin-blue .main-header .logo:hover {
      background-color: #548238;
    }
    .skin-blue .main-header li.user-header {
      background-color: #548238;
    }
    .skin-blue .main-sidebar {
      border-right: 1px solid #548238;
    }
    
    .skin-blue .main-header .navbar .nav > li > a {
      border-left: 1px solid #548238;
    }
    
    .skin-blue .wrapper {
      border: 1px solid #548238;
    }
    
    .skin-blue .content-wrapper {
      border: 1px solid #548238;
    }
  ")),
  skin = "blue"
  ),
  
  
  
  tabItems(
    tabItem(tabName = "melao",
            h2(
              fluidPage(
                
                
                #Botões de irrigação do melão----
                # box(
                #   style="font-size: 50%; background-color: #548238; color: white;",
                #   width = 2,
                #   title = "Insira os detalhes da irrigação",
                #   solidHeader = TRUE, 
                #   status = "primary", 
                #   
                #   tags$style(HTML(".radio-inline {margin-right: 42px;}")),
                #   
                #   div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
                #       numericInput(inputId = "n_et0_me",label = "ET0 (mm)",value = 5, width = '100px')),
                #   
                #   div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
                #       numericInput(inputId = "n_CE_me",label = "Condutividade Elétrica (dS/m)",value = 2, width = '100px')),
                #   
                #   div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
                #       numericInput(inputId = "n_dist_linha_me",label = "Distância entre linhas (m)",value = 2, width = '100px')),
                #   
                #   div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
                #       numericInput(inputId = "n_dist_gotej_me",label = "Distância entre gotejadores (m)",value = 0.5, width = '100px')),
                #   
                #   div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
                #       numericInput(inputId = "n_vazao_gotej_me",label = "Vazão do gotejador (L/h)",value = 1.5, width = '100px'))),
                
                
                
                tags$head(tags$style(HTML("
  .box.box-primary {
    border-top-color: #548238 !important;
    border-color: #548238 !important;
  }
  .box-primary>.box-header {
    color: #fff;
    background: #548238 !important;
  }
  .box.box-success {
    border-top-color: #B33634 !important;
    border-color: #B33634 !important;
  }
  .box-success>.box-header {
    color: #fff;
    background: #B33634 !important;
  }
"))),


box(
  style = "font-size: 50%; background-color: #548238; color: white;",
  width = 2,
  title = tags$div("Insira os detalhes da irrigação", style = "background-color: #548238; color: white;"),
  solidHeader = TRUE, 
  status = "primary",
  
  tags$style(HTML(".radio-inline {margin-right: 42px;}")),
  
  div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
      numericInput(inputId = "n_et0_me",label = "ET0 (mm)",value = 5, width = '100px')),
  
  div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
      numericInput(inputId = "n_CE_me",label = "Condutividade Elétrica (dS/m)",value = 2, width = '100px')),
  
  div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
      numericInput(inputId = "n_dist_linha_me",label = "Distância entre linhas (m)",value = 2, width = '100px')),
  
  div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
      numericInput(inputId = "n_dist_gotej_me",label = "Distância entre gotejadores (m)",value = 0.5, width = '100px')),
  
  div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
      numericInput(inputId = "n_vazao_gotej_me",label = "Vazão do gotejador (L/h)",value = 1.5, width = '100px'))
),

                
                #Kc da cultura melao----
                box(
                  style="font-size': 50%",
                  width = 10,
                  title = "Insira o kc para cada fase fenológica do meloeiro", 
                  status = "primary", 
                  solidHeader = FALSE,
                  
                  tags$style(HTML(".radio-inline {margin-right: 42px;}")),
                  
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc1_me",label = "Fase 1", value = 0.3, width = '100px')),
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc2_me",label = "Fase 2", value = 0.7, width = '100px')),
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc3_me",label = "Fase 3", value = 1.2, width = '100px')),
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc4_me",label = "Fase 4", value = 1.2, width = '100px')),
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc5_me",label = "Fase 5", value = 0.9, width = '100px')),
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc6_me",label = "Fase 6", value = 0.6, width = '100px')),
                ),
                
                #Tabela de irrigação do melão----
                box(
                  style="font-size: 50%",
                  width = 10,
                  title = "Sugestão de irrigação baseada no dia anterior", 
                  status = "primary", 
                  solidHeader = FALSE,
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      tableOutput(outputId = "tab_melao")))
)
              
            )),
    tabItem(tabName = "melancia",
            h2(
              fluidPage(
                
              
                
                
                #Botões de irrigação da melancia----
                box(
                  style="font-size: 50%; background-color: #B33634; color: white;",
                  width = 2,
                  # title = "Insira os detalhes da irrigação", 
                  title = tags$div("Insira os detalhes da irrigação", style = "background-color: #B33634; color: white;"),
                  
                  solidHeader = TRUE, 
                  status = "success",
                  
                  tags$style(HTML(".radio-inline {margin-right: 42px;}")),
                  
                  div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
                      numericInput(inputId = "n_et0_wme",label = "ET0 (mm)",value = 5, width = '100px')),
                  
                  div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
                      numericInput(inputId = "n_CE_wme",label = "Condutividade Elétrica (dS/m)",value = 2, width = '100px')),
                  
                  div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
                      numericInput(inputId = "n_dist_linha_wme",label = "Distância entre linhas (m)",value = 2, width = '100px')),
                  
                  div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
                      numericInput(inputId = "n_dist_gotej_wme",label = "Distância entre gotejadores (m)",value = 0.5, width = '100px')),
                  
                  div(style="display:inline-block;vertical-align:bottom;font-size: 14px;",
                      numericInput(inputId = "n_vazao_gotej_wme",label = "Vazão do gotejador (L/h)",value = 1.5, width = '100px'))),
                
                
                #Kc da cultura melancia----
                box(
                  style="font-size': 50%",
                  width = 10,
                  title = "Insira o kc para cada fase fenológica da melancia", 
                  status = "success", 
                  solidHeader = FALSE,
                  
                  tags$style(HTML(".radio-inline {margin-right: 42px;}")),
                  
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc1_wme",label = "Fase 1", value = 0.3, width = '100px')),
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc2_wme",label = "Fase 2", value = 0.6, width = '100px')),
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc3_wme",label = "Fase 3", value = 1.2, width = '100px')),
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc4_wme",label = "Fase 4", value = 1.2, width = '100px')),
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc5_wme",label = "Fase 5", value = 1.0, width = '100px')),
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                      numericInput(inputId = "n_kc6_wme",label = "Fase 6", value = 0.8, width = '100px')),
                ),
                
                box(
                  style="font-size: 50%",
                  width = 10,
                  title = "Sugestão de irrigação baseada no dia anterior", 
                  status = "success", 
                  solidHeader = FALSE,
                  div(style="display:inline-block;vertical-align:top;font-size: 14px;",
                  tableOutput("tab_melancia"))))
            ))
  )
)

ui <- dashboardPage(
  
  dashboardHeader(title = "MelonMundi: Irrigação"),
  sidebar,
  body
)

server <- function(input, output) {
  
  
  
  output$tab_melao <-  renderTable({
    
    tribble(
      ~"Fase",
      ~"Kc",
      # ~"Lâmina \n (mm/m²)",
      ~"Lâmina (m³/ha)",
      # ~"Irrigação (h)",
      ~"h",
      ~"min",
      
      "S1 - Até retirada da manta (±23 dias)",
      input$n_kc1_me,
      # lamina1(kc = input$n_kc1_me,ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc1_me, ET0 = input$n_et0_me), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc1_me, 
      #        ET0 = input$n_et0_me,
      #        dist_linha = input$n_dist_linha_me,
      #        dist_gotej = input$n_dist_gotej_me,
      #        vazao_gotej = input$n_vazao_gotej_me,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc1_me, ET0 = input$n_et0_me,
                        dist_linha = input$n_dist_linha_me,
                        dist_gotej = input$n_dist_gotej_me,
                        vazao_gotej = input$n_vazao_gotej_me,
                        CE = input$n_CE_me)),
      as.integer(((irriga(kc = input$n_kc1_me, 
                          ET0 = input$n_et0_me,
                          dist_linha = input$n_dist_linha_me,
                          dist_gotej = input$n_dist_gotej_me,
                          vazao_gotej = input$n_vazao_gotej_me,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc1_me, ET0 = input$n_et0_me,
                                                                  dist_linha = input$n_dist_linha_me,
                                                                  dist_gotej = input$n_dist_gotej_me,
                                                                  vazao_gotej = input$n_vazao_gotej_me,
                                                                  CE = input$n_CE_me))))*60),
      
      
      
      
      "S2 - Até finalizar o pegamento (±7 dias)",
      input$n_kc2_me,
      # lamina1(kc = input$n_kc2_me,ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc2_me, ET0 = input$n_et0_me), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc2_me, 
      #        ET0 = input$n_et0_me,
      #        dist_linha = input$n_dist_linha_me,
      #        dist_gotej = input$n_dist_gotej_me,
      #        vazao_gotej = input$n_vazao_gotej_me,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc2_me, ET0 = input$n_et0_me,
                        dist_linha = input$n_dist_linha_me,
                        dist_gotej = input$n_dist_gotej_me,
                        vazao_gotej = input$n_vazao_gotej_me,
                        CE = input$n_CE_me)),
      as.integer(((irriga(kc = input$n_kc2_me, 
                          ET0 = input$n_et0_me,
                          dist_linha = input$n_dist_linha_me,
                          dist_gotej = input$n_dist_gotej_me,
                          vazao_gotej = input$n_vazao_gotej_me,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc2_me, ET0 = input$n_et0_me,
                                                                  dist_linha = input$n_dist_linha_me,
                                                                  dist_gotej = input$n_dist_gotej_me,
                                                                  vazao_gotej = input$n_vazao_gotej_me,
                                                                  CE = input$n_CE_me))))*60),
      
      "S3 - Crescimento longitudinal dos frutos (±10 dias)",
      input$n_kc3_me,
      # lamina1(kc = input$n_kc3_me,ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc3_me, ET0 = input$n_et0_me), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc3_me, 
      #        ET0 = input$n_et0_me,
      #        dist_linha = input$n_dist_linha_me,
      #        dist_gotej = input$n_dist_gotej_me,
      #        vazao_gotej = input$n_vazao_gotej_me,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc3_me, ET0 = input$n_et0_me,
                        dist_linha = input$n_dist_linha_me,
                        dist_gotej = input$n_dist_gotej_me,
                        vazao_gotej = input$n_vazao_gotej_me,
                        CE = input$n_CE_me)),
      as.integer(((irriga(kc = input$n_kc3_me, 
                          ET0 = input$n_et0_me,
                          dist_linha = input$n_dist_linha_me,
                          dist_gotej = input$n_dist_gotej_me,
                          vazao_gotej = input$n_vazao_gotej_me,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc3_me, ET0 = input$n_et0_me,
                                                                  dist_linha = input$n_dist_linha_me,
                                                                  dist_gotej = input$n_dist_gotej_me,
                                                                  vazao_gotej = input$n_vazao_gotej_me,
                                                                  CE = input$n_CE_me))))*60),
      
      
      "S4 - Crescimento equatorial dos frutos (±10 dias)",
      input$n_kc4_me,
      # lamina1(kc = input$n_kc4_me,ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc4_me, ET0 = input$n_et0_me), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc4_me, 
      #        ET0 = input$n_et0_me,
      #        dist_linha = input$n_dist_linha_me,
      #        dist_gotej = input$n_dist_gotej_me,
      #        vazao_gotej = input$n_vazao_gotej_me,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc4_me, ET0 = input$n_et0_me,
                        dist_linha = input$n_dist_linha_me,
                        dist_gotej = input$n_dist_gotej_me,
                        vazao_gotej = input$n_vazao_gotej_me,
                        CE = input$n_CE_me)),
      as.integer(((irriga(kc = input$n_kc4_me, 
                          ET0 = input$n_et0_me,
                          dist_linha = input$n_dist_linha_me,
                          dist_gotej = input$n_dist_gotej_me,
                          vazao_gotej = input$n_vazao_gotej_me,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc4_me, ET0 = input$n_et0_me,
                                                                  dist_linha = input$n_dist_linha_me,
                                                                  dist_gotej = input$n_dist_gotej_me,
                                                                  vazao_gotej = input$n_vazao_gotej_me,
                                                                  CE = input$n_CE_me))))*60),
      
      
      "S5 - Maturação dos frutos (±7 dias)",
      input$n_kc5_me,
      # lamina1(kc = input$n_kc5_me,ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc5_me, ET0 = input$n_et0_me), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc5_me, 
      #        ET0 = input$n_et0_me,
      #        dist_linha = input$n_dist_linha_me,
      #        dist_gotej = input$n_dist_gotej_me,
      #        vazao_gotej = input$n_vazao_gotej_me,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc5_me, ET0 = input$n_et0_me,
                        dist_linha = input$n_dist_linha_me,
                        dist_gotej = input$n_dist_gotej_me,
                        vazao_gotej = input$n_vazao_gotej_me,
                        CE = input$n_CE_me)),
      as.integer(((irriga(kc = input$n_kc5_me, 
                          ET0 = input$n_et0_me,
                          dist_linha = input$n_dist_linha_me,
                          dist_gotej = input$n_dist_gotej_me,
                          vazao_gotej = input$n_vazao_gotej_me,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc5_me, ET0 = input$n_et0_me,
                                                                  dist_linha = input$n_dist_linha_me,
                                                                  dist_gotej = input$n_dist_gotej_me,
                                                                  vazao_gotej = input$n_vazao_gotej_me,
                                                                  CE = input$n_CE_me))))*60),
      
      
      "S6 - Após a colheita",
      input$n_kc6_me,
      # lamina1(kc = input$n_kc6_me,ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc6_me, ET0 = input$n_et0_me), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc6_me, 
      #        ET0 = input$n_et0_me,
      #        dist_linha = input$n_dist_linha_me,
      #        dist_gotej = input$n_dist_gotej_me,
      #        vazao_gotej = input$n_vazao_gotej_me,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc6_me, ET0 = input$n_et0_me,
                        dist_linha = input$n_dist_linha_me,
                        dist_gotej = input$n_dist_gotej_me,
                        vazao_gotej = input$n_vazao_gotej_me,
                        CE = input$n_CE_me)),
      as.integer(((irriga(kc = input$n_kc6_me, 
                          ET0 = input$n_et0_me,
                          dist_linha = input$n_dist_linha_me,
                          dist_gotej = input$n_dist_gotej_me,
                          vazao_gotej = input$n_vazao_gotej_me,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc6_me, ET0 = input$n_et0_me,
                                                                  dist_linha = input$n_dist_linha_me,
                                                                  dist_gotej = input$n_dist_gotej_me,
                                                                  vazao_gotej = input$n_vazao_gotej_me,
                                                                  CE = input$n_CE_me))))*60),
      
    )})
  
  render_dt = function(data, editable = 'cell', server = TRUE, ...) {
    DT::renderDT(data, selection = 'none', server = server, editable = editable, ...)
    
  }
  

  
  
  output$tab_melancia <-  renderTable({
    
    tribble(
      ~"Fase",
      ~"Kc",
      # ~"Lâmina (mm/m²)",
      ~"Lâmina (m³/ha)",
      # ~"Irrigação (h)",
      ~"h",
      ~"min",
      
      "S1 - Até retirada da manta (±23 dias)",
      input$n_kc1_wme,
      # lamina1(kc = input$n_kc1_wme,ET0 = input$n_et0_wme), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc1_wme, ET0 = input$n_et0_wme), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc1_wme, 
      #        ET0 = input$n_et0_wme,
      #        dist_linha = input$n_dist_linha_wme,
      #        dist_gotej = input$n_dist_gotej_wme,
      #        vazao_gotej = input$n_vazao_gotej_wme,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc1_wme, ET0 = input$n_et0_wme,
                        dist_linha = input$n_dist_linha_wme,
                        dist_gotej = input$n_dist_gotej_wme,
                        vazao_gotej = input$n_vazao_gotej_wme,
                        CE = input$n_CE_wme)),
      as.integer(((irriga(kc = input$n_kc1_wme, 
                          ET0 = input$n_et0_wme,
                          dist_linha = input$n_dist_linha_wme,
                          dist_gotej = input$n_dist_gotej_wme,
                          vazao_gotej = input$n_vazao_gotej_wme,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc1_wme, ET0 = input$n_et0_wme,
                                                                  dist_linha = input$n_dist_linha_wme,
                                                                  dist_gotej = input$n_dist_gotej_wme,
                                                                  vazao_gotej = input$n_vazao_gotej_wme,
                                                                  CE = input$n_CE_wme))))*60),
      
      
      "S2 - Até finalizar o pegamento (±7 dias)",
      input$n_kc2_wme,
      # lamina1(kc = input$n_kc2_wme,ET0 = input$n_et0_wme), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc2_wme, ET0 = input$n_et0_wme), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc2_wme, 
      #        ET0 = input$n_et0_wme,
      #        dist_linha = input$n_dist_linha_wme,
      #        dist_gotej = input$n_dist_gotej_wme,
      #        vazao_gotej = input$n_vazao_gotej_wme,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc2_wme, ET0 = input$n_et0_wme,
                        dist_linha = input$n_dist_linha_wme,
                        dist_gotej = input$n_dist_gotej_wme,
                        vazao_gotej = input$n_vazao_gotej_wme,
                        CE = input$n_CE_wme)),
      as.integer(((irriga(kc = input$n_kc2_wme, 
                          ET0 = input$n_et0_wme,
                          dist_linha = input$n_dist_linha_wme,
                          dist_gotej = input$n_dist_gotej_wme,
                          vazao_gotej = input$n_vazao_gotej_wme,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc2_wme, ET0 = input$n_et0_wme,
                                                                  dist_linha = input$n_dist_linha_wme,
                                                                  dist_gotej = input$n_dist_gotej_wme,
                                                                  vazao_gotej = input$n_vazao_gotej_wme,
                                                                  CE = input$n_CE_wme))))*60),
      
      "S3 - Últimas vingas tamanho laranja (±5 dias)",
      input$n_kc3_wme,
      # lamina1(kc = input$n_kc3_wme,ET0 = input$n_et0_wme), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc3_wme, ET0 = input$n_et0_wme), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc3_wme, 
      #        ET0 = input$n_et0_wme,
      #        dist_linha = input$n_dist_linha_wme,
      #        dist_gotej = input$n_dist_gotej_wme,
      #        vazao_gotej = input$n_vazao_gotej_wme,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc3_wme, ET0 = input$n_et0_wme,
                        dist_linha = input$n_dist_linha_wme,
                        dist_gotej = input$n_dist_gotej_wme,
                        vazao_gotej = input$n_vazao_gotej_wme,
                        CE = input$n_CE_wme)),
      as.integer(((irriga(kc = input$n_kc3_wme, 
                          ET0 = input$n_et0_wme,
                          dist_linha = input$n_dist_linha_wme,
                          dist_gotej = input$n_dist_gotej_wme,
                          vazao_gotej = input$n_vazao_gotej_wme,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc3_wme, ET0 = input$n_et0_wme,
                                                                  dist_linha = input$n_dist_linha_wme,
                                                                  dist_gotej = input$n_dist_gotej_wme,
                                                                  vazao_gotej = input$n_vazao_gotej_wme,
                                                                  CE = input$n_CE_wme))))*60),
      
      
      "S4 - Finalização do tamanho dos frutos (±15 dias)",
      input$n_kc4_wme,
      # lamina1(kc = input$n_kc4_wme,ET0 = input$n_et0_wme), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc4_wme, ET0 = input$n_et0_wme), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc4_wme, 
      #        ET0 = input$n_et0_wme,
      #        dist_linha = input$n_dist_linha_wme,
      #        dist_gotej = input$n_dist_gotej_wme,
      #        vazao_gotej = input$n_vazao_gotej_wme,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc4_wme, ET0 = input$n_et0_wme,
                        dist_linha = input$n_dist_linha_wme,
                        dist_gotej = input$n_dist_gotej_wme,
                        vazao_gotej = input$n_vazao_gotej_wme,
                        CE = input$n_CE_wme)),
      as.integer(((irriga(kc = input$n_kc4_wme, 
                          ET0 = input$n_et0_wme,
                          dist_linha = input$n_dist_linha_wme,
                          dist_gotej = input$n_dist_gotej_wme,
                          vazao_gotej = input$n_vazao_gotej_wme,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc4_wme, ET0 = input$n_et0_wme,
                                                                  dist_linha = input$n_dist_linha_wme,
                                                                  dist_gotej = input$n_dist_gotej_wme,
                                                                  vazao_gotej = input$n_vazao_gotej_wme,
                                                                  CE = input$n_CE_wme))))*60),
      
      
      "S5 - Maturação dos frutos (±5 dias)",
      input$n_kc5_wme,
      # lamina1(kc = input$n_kc5_wme,ET0 = input$n_et0_wme), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc5_wme, ET0 = input$n_et0_wme), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc5_wme, 
      #        ET0 = input$n_et0_wme,
      #        dist_linha = input$n_dist_linha_wme,
      #        dist_gotej = input$n_dist_gotej_wme,
      #        vazao_gotej = input$n_vazao_gotej_wme,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc5_wme, ET0 = input$n_et0_wme,
                        dist_linha = input$n_dist_linha_wme,
                        dist_gotej = input$n_dist_gotej_wme,
                        vazao_gotej = input$n_vazao_gotej_wme,
                        CE = input$n_CE_wme)),
      as.integer(((irriga(kc = input$n_kc5_wme, 
                          ET0 = input$n_et0_wme,
                          dist_linha = input$n_dist_linha_wme,
                          dist_gotej = input$n_dist_gotej_wme,
                          vazao_gotej = input$n_vazao_gotej_wme,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc5_wme, ET0 = input$n_et0_wme,
                                                                  dist_linha = input$n_dist_linha_wme,
                                                                  dist_gotej = input$n_dist_gotej_wme,
                                                                  vazao_gotej = input$n_vazao_gotej_wme,
                                                                  CE = input$n_CE_wme))))*60),
      
      
      "S6 - Após a primeira colheita",
      input$n_kc6_wme,
      # lamina1(kc = input$n_kc6_wme,ET0 = input$n_et0_wme), #Lâmina (mm/m²)
      lamina2(kc = input$n_kc6_wme, ET0 = input$n_et0_wme), #Lâmina (m³/ha)
      # irriga(kc = input$n_kc6_wme, 
      #        ET0 = input$n_et0_wme,
      #        dist_linha = input$n_dist_linha_wme,
      #        dist_gotej = input$n_dist_gotej_wme,
      #        vazao_gotej = input$n_vazao_gotej_wme,
      #        CE = input$n_CE_me),
      as.integer(irriga(kc = input$n_kc6_wme, ET0 = input$n_et0_wme,
                        dist_linha = input$n_dist_linha_wme,
                        dist_gotej = input$n_dist_gotej_wme,
                        vazao_gotej = input$n_vazao_gotej_wme,
                        CE = input$n_CE_wme)),
      as.integer(((irriga(kc = input$n_kc6_wme, 
                          ET0 = input$n_et0_wme,
                          dist_linha = input$n_dist_linha_wme,
                          dist_gotej = input$n_dist_gotej_wme,
                          vazao_gotej = input$n_vazao_gotej_wme,
                          CE = input$n_CE_me))-(as.integer(irriga(kc = input$n_kc6_wme, ET0 = input$n_et0_wme,
                                                                  dist_linha = input$n_dist_linha_wme,
                                                                  dist_gotej = input$n_dist_gotej_wme,
                                                                  vazao_gotej = input$n_vazao_gotej_wme,
                                                                  CE = input$n_CE_wme))))*60)
      
    )})
  
  render_dt = function(data, editable = 'cell', server = TRUE, ...) {
    DT::renderDT(data, selection = 'none', server = server, editable = editable, ...)
    
  }
  
}





#Tabela melancia


shinyApp(ui, server)