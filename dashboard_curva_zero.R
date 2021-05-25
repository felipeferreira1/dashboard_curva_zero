#Script para dados da curva zero
#Feito por: Felipe Simplício Ferreira
#última atualização:25/05/2021

##Carregando pacotes que serão utilizados
library(tidyverse)
library(rio)
library(openxlsx)
library(lubridate)
#library(shiny)


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
seleciona_dados <- seq(1,length(dados), 8)
#seleciona_coluna_nominal <- seq(4,length(dados), 8)
#seleciona_coluna_inflacao <- seq(5,length(dados), 8)
for (i in 1:length(datas_tratadas)){
  dados_dia <- dados[0:22,seleciona_dados[i]:(seleciona_dados[i]+4)]
  names(dados_dia) <- dados_dia[1,]
  dados_dia <- dados_dia[-1,]
  dados_dia <- mutate_all(dados_dia, function(x) as.numeric(as.character(x)))
  dados_dia <- as.data.frame(dados_dia)
  dados_dia <- drop_na(dados_dia)
  names(dados_dia) <- c("Anos", "Vertices", "real", "nominal", "implícita")
  nome_arquivo <- as.character(datas_tratadas[i])
  assign(nome_arquivo, dados_dia) #Nomeando arquivos
  print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso da repetição
}


#Selecionando data
usuario_data <- function(){
  resposta_data <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")

    while(is.element(resposta_data, datas_tratadas) == F){
      print(datas_tratadas)
      resposta_data <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
    }

  resposta_dado <- readline(prompt = "Escolha um dado (nominal, real ou implícita): ")
  opcoes_dado <- c("nominal", "real", "implícita")
  while(is.element(resposta_dado, opcoes_dado) == F){
    resposta_dado <- readline(prompt = "Escolha um dado (nominal, real ou implícita): ")
  }

  graf <- ggplot() + 
    geom_line(data = get(resposta_data), aes(x=Anos, y = get(resposta_dado), colour = resposta_data), size=.8)
  show(graf)
  
  adicionar_linha <- readline(prompt = "Deseja adicionar mais datas (responda com sim ou não): ")
  while(adicionar_linha == "sim"){
    resposta_data_nova <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
    while(is.element(resposta_data_nova, datas_tratadas) == F){
      print(datas_tratadas)
      resposta_data_nova <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
    }
    
    graf <- graf + 
      geom_line(data = get(resposta_data_nova), aes(x=Anos, y = get(resposta_dado), colour = resposta_data_nova), size=.8)
    show(graf)
    
  }
}

usuario_data()
