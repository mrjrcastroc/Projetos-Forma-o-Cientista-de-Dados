# Definindo  o working directory 
setwd("C:/FCD/BigDataAnalytics-R-Azure/Projeto2")

# Carregando o pacote para manipulação de dados
library(tidyverse)
library(rmarkdown)

# Carregando os dados
dataset<- read_csv("dataset/train.csv", n_max = 15000000)

#Visualizando os dados
head(dataset)
str(dataset)
summary(dataset)

#Verificando se há valores NA
colSums(is.na(dataset))

#Verificando os dias de semana
table(dataset$Semana)


#Visualizando a relação de vendas por semana 
ggplot(data = dataset %>% sample_frac(0.005)) +
  geom_bar(mapping = aes(x =Semana), alpha = 0.75, color = "black", fill = "green", width = 0.75)+
  scale_x_continuous(breaks = 2:5) + 
  scale_y_continuous("Vendas por Semana") + 
  theme_minimal()


#Visualizando a correlação entre as variáveis
library(corrplot)

corrplot(cor(dataset))


#Criando um subset com as variáveis que possuem correlação com a variável target
dataset_treino <- dataset %>%
  select(Canal_ID, Ruta_SAK, Venta_hoy, Venta_uni_hoy, Dev_proxima,Dev_uni_proxima, Demanda_uni_equil)

rm(dataset)

#Verificando a correlação entre as variáveis do subset
corrplot(cor(dataset_treino))


#Separando os dados em treino e teste
library(caret)
set.seed(5000)

split <- createDataPartition(y = dataset_treino$Canal_ID, p = 0.70, list = F)

treino <- dataset_treino[split,]
teste <- dataset_treino[-split,]

#Verificando a proporção dos dados
nrow(teste) + nrow(treino) == nrow(dataset_treino)

rm(dataset_treino)
rm(split)

#Criando o modelo
set.seed(1234)
modelo <- lm(Demanda_uni_equil ~ ., data=treino)

#Verificando o desempenho do modelo
summary(modelo)


previsoes <- predict(modelo, teste)

score<- data.frame(valor_teste = teste$Demanda_uni_equil, 
                   valor_previsto = previsoes)


#Relação entre os valores previstos e o valor de teste

ggplot(data = score %>% sample_frac(0.005), aes(x = valor_teste, y= valor_previsto))+
  geom_point(stroke = 1.5, color = 'gold3', alpha = 0.9)+
  geom_smooth(method = 'lm', linetype = 3)+
  annotate("text", x=135, y=1000, label= "Taxa de acerto: 99.5%") + 
  labs(x = 'Valores de Teste', y = 'Valores Previstos', main = 'Valores de Teste x Valores Previstos')+
  theme_minimal()


#
summary(str)
