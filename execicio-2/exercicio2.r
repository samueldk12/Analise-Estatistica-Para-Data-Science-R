### Primeira coisa ao iniciar um projeto -- Definir um workspace
setwd("C:\\Users\\samuel.arao\\projects\\estatistica\\git_exercicios\\Analise-Estatistica-Para-Data-Science-R\\execicio-2")

getwd()

## Carregar o dataset

data <- read.csv("dataset.csv")

str(data)


## Tratamento dos dados

### Transformando char em factor
data$ano <- as.factor(data$ano)
data$modelo <- as.factor(data$modelo)
data$cor <- as.factor(data$cor)
data$transmissao <- as.factor(data$transmissao)

## 1 - Quais as 3 cores dos veículos mais vendidos? [OPÇÃO 1]
tabcor <- table(data$cor)
tabcor

#Resposta 
# - Preto
# - Prata
# - Vermelho

## 1 - Quais as 3 cores dos veículos mais vendidos? [OPÇÃO 2]
install.packages("tidyverse")
library(tidyverse)

data %>% 
  count(cor) %>%
  top_n(3,n) %>%
  arrange(desc(n))

#Resposta 
# - Preto
# - Prata
# - Vermelho

## 2- De qual ano são os veículos mais vendidos? [OPÇÃO 1]
tabano <- table(data$ano)
tabano

#Resposta 
# - 2010

## 2- De qual ano são os veículos mais vendidos? [OPÇÃO 1]
data %>%
  count(ano) %>%
  top_n(3,n) %>%
  arrange(desc(n))

#Resposta 
# - 2010

# 3- Crie um barplot para apresentar sua resposta no item 2.
barplot(tabcor,main="Counts")

# 4- Qual o percentual de vendas de veículos com transmissão automática
tabcambio <- table(data$transmissao)
tabcambio

relfreq <- prop.table(tabcambio) * 100
relfreq

# 5- Crie um Pie Chart para representar sua resposta no item 4.
lbls <- c("Auto", "Manual")
lbls <- paste(lbls,"%", sep="")
pie(relfreq, labels=lbls, col= rainbow(length(lbls)),main="Pie chart de Veículos Vendidos por Tipo")

# 6- Qual o percentual de venda de veículos por modelo?
install.packages("janitor")
library(janitor)
tabyl(data$modelo, sort=TRUE)

# 7- Calcule o percentual de vendas por preço do veículo e o percentual acumulado
tabpreco <- table(data$preco)
tabpreco
relfreq <- prop.table(tabpreco) *100
relfreq
cumsum(relfreq)

# 8- Liste o total de veículos vendidos por ano e por tipo de transmissão
tab <- table(data$ano, data$transmissao)
tab

# 9- Imprima o resumo estatístico com o teste do qui-quadrado, graus de liberdade e valor p
summary(tab)
chisq.test(tab)

# 10- Crie um barplot a partir do resultado do item 8
barplot(tab, beside= TRUE, legend.text = rownames(tab), ylab= "Frequencia Absoluta")
