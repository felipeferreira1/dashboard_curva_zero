#################################################################################################################
# Shiny app com dados da estrutura a termo da taxa de juros
# Felipe Simplicio Ferreira
# Data: 24-06-2021
#################################################################################################################



# Pacotes
library(shiny)
library(tidyverse)
library(rio)
library(openxlsx)
library(ggrepel)
library(scales)



# Importando planilha
# Definindo as datas
datas <- import("curva_zero.xlsx", col_names = F)
datas <- datas[1,]
seleciona_datas <- seq(2,length(datas), 8)
datas_tratadas <- NA
for (i in 1:length(seleciona_datas)){
    datas_tratadas[i] <- datas[1,seleciona_datas[i]]
}
datas_tratadas <- as.vector(as.character((convertToDate(datas_tratadas))))



# Coletando o resto dos dados
dados <- import("curva_zero.xlsx", skip = 5, col_names = F)
seleciona_dados <- seq(1,length(dados), 8)
for (i in 1:length(datas_tratadas)){
    dados_dia <- dados[0:22,seleciona_dados[i]:(seleciona_dados[i]+4)]
    names(dados_dia) <- dados_dia[1,]
    dados_dia <- dados_dia[-1,] %>% mutate_all(function(x) as.numeric(as.character(x))) %>%
        as.data.frame() %>% drop_na()
    names(dados_dia) <- c("anos", "vertices", "real", "nominal", "implicita")
    dados_dia["data"] <- datas_tratadas[i]
    nome_arquivo <- as.character(datas_tratadas[i])
    assign(nome_arquivo, dados_dia) #Nomeando arquivos
    print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso
}

dados_real <- data.frame()
for (i in 1:length(datas_tratadas)){
    dados_a_incorporar <- select(get(datas_tratadas[i]), data, anos, real)
    dados_real <- rbind(dados_real, dados_a_incorporar)
    print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso
}

dados_nominal <- data.frame()
for (i in 1:length(datas_tratadas)){
    dados_a_incorporar <- select(get(datas_tratadas[i]), data, anos, nominal)
    dados_nominal <- rbind(dados_nominal, dados_a_incorporar)
    print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso
}

dados_implicita <- data.frame()
for (i in 1:length(datas_tratadas)){
    dados_a_incorporar <- select(get(datas_tratadas[i]), data, anos, implicita)
    dados_implicita <- rbind(dados_implicita, dados_a_incorporar)
    print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso
}


dados_lista <- list(implicita = dados_implicita, nominal = dados_nominal, real = dados_real)



# CÃ³digo do servidor
server <- function(input, output, session){

    resposta_data <- reactive({
        as.character(input$datas)
    })
    
    resposta_dados <- reactive({
        as.character(input$dados)
    }) 
    
    dados_graf <- reactive({
        req(as.data.frame(dados_lista[[resposta_dados()]])[as.data.frame(dados_lista[[resposta_dados()]])$data %in% resposta_data(),])
    })
    
    tabela <- reactive({
        arrange(pivot_wider(dados_graf(), names_from = data, values_from = !!sym(resposta_dados())), anos)
    })
    
    opcao_dados <- reactive({
        if (resposta_dados() == "real")
            return("Real")
        if (resposta_dados() == "nominal")
            return("Nominal")
        if (resposta_dados() == "implicita")
            return("InflaÃ§Ã£o implÃ­cita")
        })
    
    limites <- reactive({
        dados_graf() %>% select(-anos, -data) %>% replace(is.na(.), 0)
    })
    
    
    output$plot <- renderPlot({
        dados_graf() %>% 
            ggplot(aes(x = anos, y = !!sym(resposta_dados()), color = data, label = sprintf("%0.2f", round(!!sym(resposta_dados()),2)))) + 
            geom_line() + geom_label_repel() + 
            theme(axis.text.x=element_text(angle=90, hjust=1)) + 
            labs(title = "Curvas", subtitle = opcao_dados(), 
                 caption = "Fonte: Anbima") + ylab("%") + xlab("Anos") + 
            scale_x_continuous(breaks = seq(0.5, 10.5, 0.5)) + 
            scale_y_continuous(breaks = round(seq(min(limites()), max(limites()), by = 0.5),1)) + 
            scale_color_discrete(name = "Datas")
            })
    
    output$table <- renderTable({
        # arrange(pivot_wider(dados_graf(), names_from = data, values_from = !!sym(resposta_dados())), anos)
        tabela()
        })
    
    output$download <- downloadHandler(
        filename = function(){"dados.xlsx"}, 
        content = function(fname){
            export(tabela(), fname, row.names = F)
        })
}



# CÃ³digo da "user interface"

ui <- fluidPage(
    h1("Estrutura a termo da taxa de juros"),
    selectInput(inputId = "datas",
                label = "Escolha alguma(s) data(s):",
                choices = datas_tratadas,
                multiple = T,
                selected = head(datas_tratadas,1)),
    selectInput(inputId = "dados",
                label = "Escolha um tipo de dado:",
                choices = c("Nominal" = "nominal",
                            "Real" = "real",
                            "InflaÃ§Ã£o implÃ­cita" = "implicita")),
    mainPanel(tabsetPanel(type = "tabs",
              tabPanel("Gráfico", plotOutput("plot")),
              tabPanel("Tabela", tableOutput("table"))),
    downloadButton('download',"Download dos dados")))

# ui <- fluidPage(
#     h1("Estrutura a termo da taxa de juros"),
#     selectInput(inputId = "datas",
#                 label = "Escolha alguma(s) data(s):",
#                 choices = datas_tratadas,
#                 multiple = T,
#                 selected = head(datas_tratadas,1)),
#     selectInput(inputId = "dados",
#                 label = "Escolha um tipo de dado:",
#                 choices = c("Nominal" = "nominal",
#                             "Real" = "real",
#                             "InflaÃ§Ã£o implÃ­cita" = "implicita")),
#     h2("GrÃ¡fico"),
#     plotOutput("plot"),
#     h2("Tabela com dados"),
#     tableOutput("table"),
#     downloadButton('download',"Download da tabela")
# )



# Run the application 
shinyApp(ui = ui, server = server)