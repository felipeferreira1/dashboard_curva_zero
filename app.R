#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rio)
library(openxlsx)
library(ggrepel)
library(scales)

#Importando planilha
#Definindo as datas
datas <- import("curva_zero.xlsx", col_names = F)
datas <- datas[1,]
seleciona_datas <- seq(2,length(datas), 8)
datas_tratadas <- NA
for (i in 1:length(seleciona_datas)){
    datas_tratadas[i] <- datas[1,seleciona_datas[i]]
}
datas_tratadas <- as.vector(as.character((convertToDate(datas_tratadas))))

#Coletando o resto dos dados
dados <- import("curva_zero.xlsx", skip = 5, col_names = F)
# dados_graf <- data.frame()
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

# dados_juntos = merge(dados_implicita, dados_nominal, by = c("anos", "data"), all = T)
# dados_juntos = merge(dados_juntos, dados_real, by = c("anos", "data"), all = T)

dados_lista <- list(implicita = dados_implicita, nominal = dados_nominal, real = dados_real)

server <- function(input, output, session){

    resposta_data <- reactive({
        as.character(input$datas)
    })
    
    resposta_dados <- reactive({
        as.character(input$dados)
    }) 
    
    dados_graf <- reactive({
        as.data.frame(dados_lista[[resposta_dados()]]) %>% 
            dplyr::filter(data == resposta_data())
    })
    
    # output$table <- renderDataTable({
    #     dados_graf()}
    # )
    
    # output$text <-renderText({
    #     class(resposta_data())
    # })
    
    output$plot <- renderPlot({
        dados_graf() %>% 
            ggplot(aes(x = anos, y = !!sym(resposta_dados()), color = data, label = !!sym(resposta_dados()))) + 
            geom_line() + geom_label_repel() + 
            theme(axis.text.x=element_text(angle=90, hjust=1)) + 
            labs(title = paste("Curva zero", resposta_data(), sep = " "), subtitle = resposta_dados(), 
                 caption = "Fonte: Ambima") + ylab("%") + xlab("Anos")
            }
    )
}

# Define UI for application
ui <- fluidPage(
    h1("Curva Zero"),
    selectInput(inputId = "datas",
                label = "escolha uma data",
                choices = datas_tratadas,
                multiple = T),
    
    selectInput(inputId = "dados",
                label = "escolha um tipo de dado",
                choices = c("nominal" = "nominal",
                            "real" = "real",
                            "implicita" = "implicita")),
    
    plotOutput("plot")
    # dataTableOutput("table")
    # textOutput("text")
)


# Run the application 
shinyApp(ui = ui, server = server)


