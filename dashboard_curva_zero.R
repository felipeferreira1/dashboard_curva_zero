#Script para dados da curva zero
#Feito por: Felipe Simplício Ferreira
#última atualização: 25/05/2021

##Carregando pacotes que serão utilizados
library(tidyverse)
library
library(openxlsx)
#library(shiny)
#devtools::install_github('bbc/bbplot')
#library(bbplot)



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
for (i in 1:length(datas_tratadas)){
  dados_dia <- dados[0:22,seleciona_dados[i]:(seleciona_dados[i]+4)]
  names(dados_dia) <- dados_dia[1,]
  dados_dia <- dados_dia[-1,] %>% mutate_all(function(x) as.numeric(as.character(x))) %>%
    as.data.frame() %>% drop_na()
  names(dados_dia) <- c("anos", "vertices", "real", "nominal", "implícita")
  nome_arquivo <- as.character(datas_tratadas[i])
  assign(nome_arquivo, dados_dia) #Nomeando arquivos
  print(paste(i, length(datas_tratadas), sep = '/')) #Printa o progresso da repetição
}


#Selecionando data
usuario_data <- function(){
  resposta_dados <- readline(prompt = "Escolha um dado (nominal, real ou implícita): ")
  opcoes_dados <- c("nominal", "real", "implícita")
  while(is.element(resposta_dados, opcoes_dados) == F){
    resposta_dados <- readline(prompt = "Escolha um dado (nominal, real ou implícita): ")
  }
  
  resposta_data <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
  while(is.element(resposta_data, datas_tratadas) == F){
    print(datas_tratadas)
    resposta_data <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
  }
  
  dado_graf <- get(resposta_data) %>% select(anos, resposta_dados) %>% rename(!!resposta_data := resposta_dados)

  graf <- ggplot() + 
    geom_line(data = dado_graf, aes(x = anos, y = get(!!resposta_data)))
  show(graf)
  
  for (i in 1:length(datas_tratadas)){
    resposta_data_nova <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
    while(is.element(resposta_data_nova, datas_tratadas) == F){
      print(datas_tratadas)
      resposta_data_nova <- readline(prompt = "Escolha uma data no formato YYYY-MM-DD: ")
    }
    
    dado_graf <- merge(dado_graf, select(get(resposta_data_nova), anos, resposta_dados), by = "anos", all = T) %>%
      rename(!!resposta_data_nova := resposta_dados)
    
    dado_graf_mult <- pivot_longer(dado_graf, -1)
    
    graf <- graf + 
      geom_line(data = dado_graf_mult, aes(x = anos, y = value, colour = name))
    show(graf)
  }
}

usuario_data()
