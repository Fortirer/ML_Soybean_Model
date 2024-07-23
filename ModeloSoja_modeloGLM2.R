# para este script consultei o material disponivel em
# https://analises-ecologicas.com/cap8


# USANDO ESSE CODIGO PARA RODAR OS MODELOS

#   scrip que aparentemente deu certo interacao 

#########################################################
#      modelo por tratamento 
#########################################################
df <- read.csv("DadosModeloDuplo.csv", sep=";")
df <- read.csv("DadosModeloMultivariadoSoja.csv", sep=";")

df <- read.csv("Amb2.csv", sep=";")

df$Trataments <- as.factor(df$Trataments)
str(df)


## pela analise de correleacao realizada na analise exploratoria
# optou por realizar o modelo com as variaveis com uma relacao mais linear com a resposta do grao
# ajuste modelo resposta grao por total biomassa

# Se precisar excluir uma parte dos dados
#df_dummies <- df_dummies[df$Grain != 0.000001, ]

# criando interacao termos para  Elev/Drought and Elev/Temp
df$Temp_Drought_Interaction <- as.integer(df$Trataments %in% c('Elev'))

df$Dro <- as.integer(df$Trataments %in% c('AmbDrought'))


# Converte categorica coluna para numericaca
for (column in names(df)[sapply(df, is.factor)]) {
  df[[column]] <- as.integer(as.numeric(df[[column]]))
}

modelo <- glm(Grain ~ NumberLeaves:Temp_Drought_Interaction, data = df, family='Gamma'(link='inverse'))
modelo <- glm(Grain ~ NumberLeaves:Dro, data = df, family='Gamma'(link='inverse'))

summary(modelo)
modelo <- glm(Grain ~ NumberLeaves, data = df, family='Gamma'(link='inverse'))
modelo <- glm(Grain ~ NumberLeaves, data = df, family='Gamma'(link='inverse'))


summary(modelo)

valores_preditos <- predict(modelo, type = "response")

# Calcular os valores observados
valores_observados <- df$Grain
valores_observados

df$Grain

valores_preditos

# Calcular o R²
residuos <- valores_observados - valores_preditos
ss_total <- sum((valores_observados - mean(valores_observados))^2)
ss_residual <- sum(residuos^2)
r_squared <- 1 - (ss_residual / ss_total)

# Visualizar o R²
print(r_squared)

# VALORES OBSERVADOR
# Calcular a média
media <- mean(valores_observados)
media

# Calcular o desvio padrão
desvio_padrao <- sd(valores_observados)
desvio_padrao

# Calcular o tamanho da amostra
tamanho_amostra <- length(valores_observados)

# Calcular o erro padrão da média
erro_media <- desvio_padrao / sqrt(tamanho_amostra)
erro_media



# VALORES PREDITOS
# Calcular a média
media <- mean(valores_preditos)
media

# Calcular o desvio padrão
desvio_padrao <- sd(valores_preditos)
desvio_padrao

# Calcular o tamanho da amostra
tamanho_amostra <- length(valores_preditos)

# Calcular o erro padrão da média
erro_media <- desvio_padrao / sqrt(tamanho_amostra)
erro_media

##########################################################
## MODELO por tratamento
##########################################################

# Carregar o conjunto de dados
setwd("C:/Users/LAFIECO/Downloads")
dados <- read.csv("DadosModeloMultivariadoSoja.csv", sep=";")

# Verificar a estrutura dos dados
str(dados)

#?glm
# Ajustar o modelo glm
modelo <- glm(Grain ~ NumberLeaves:Trataments, data = dados, family='Gamma'(link='inverse'))
summary(modelo) # resumo modelo
plot(modelo) 

######
# calculando valores preditos e observados por tratamento
# Ajustar o modelo GLM
library(dplyr)

modelo <- glm(Grain ~ TotalBiomass:Trataments, data = df_dummies, family = Gamma(link = 'inverse'))
modelo <- glm(Grain ~ TotalBiomass:Trataments, data = df, family = Gamma(link = 'inverse'))


# Prever os valores
valores_preditos <- predict(modelo, type = "response")  # "response" para obter os valores preditos na escala original
mean(valores_preditos)
# Adicionar os valores preditos ao conjunto de dados original
df_dummies$Valores_Preditos <- valores_preditos

# Visualizar valores preditos e observados para cada tratamento

df_dummies %>%
  select(Trataments, Grain, Valores_Preditos) %>%
  arrange(Trataments)


# modelo com interacao
modelo2 <- glm(Grain ~ dados$Trataments=="Amb"*dados$Trataments=="AmbDrought", data = dados, family='Gamma'(link='inverse'))
summary(modelo2) # resumo modelo
plot(modelo) 


#
modelo <- glm(Grain ~ TotalBiomass:Trataments, data = dados, family='Gamma'(link='inverse'))
summary(modelo)
str(dados)

# nao deu certo pq deu NA

# criando interacao termos para  Elev/Drought and Elev/Temp
dados$Elev_Interaction <- as.integer(dados$Trataments %in% c('AmbTemp'))


# Converte categorica coluna para numericaca
for (column in names(dados)[sapply(dados, is.factor)]) {
  dados[[column]] <- as.integer(as.numeric(dados[[column]]))
}

modelo <- glm(Grain ~ Elev_Interaction, data = dados, family='Gamma'(link='inverse'))
summary(modelo)

str(dados)

###############################   fim
######
# fazendo outra forma
# Create an interaction term for Elev/Drought and Elev/Temp
dados$Elev_Interaction <- as.integer(dados$Trataments %in% c("Elev", "Amb/Temp", "Amb/Drought"))
dados

# Convert categorical columns to numeric
categorical_columns <- sapply(dados, is.factor)
dados[categorical_columns] <- lapply(dados[categorical_columns], as.numeric)

# Fit a linear model with the interaction term
X <- cbind(1, dados$Trataments)

# X <- cbind(1, combined_data$Leaves, combined_data$Stem, combined_data$Root, combined_data$Grain)

y <- dados$Grain
# y <- combined_data$Trataments

model <- lm(y ~ X)

# Print the model summary
print(summary(model))



###################outra opcao
# Criar a interação entre Biomassa e Tratamentos
dados$Interacao <- dados$TotalBiomass * as.numeric(dados$Trataments == "AmbTemp") # deu certo
dados$Interacao <- as.numeric(dados$Trataments == "Temp") + as.numeric(dados$Trataments == "AmbTemp")


# Criar a interação entre Elev, Drought e Temp de TotalBiomass
dados$Interacao <- dados$Trataments == "Elev" & dados$Trataments == "AmbDrought" & dados$Trataments == "AmbTemp"
# ou 



# Ajustar o modelo glm com a interação
modelo <- glm(Grain ~ Trataments + TotalBiomass + Interacao, data = dados, family = gaussian)

# Verificar o resumo do modelo
summary(modelo)

# Verificar o resumo do modelo
summary(modelo)


#   outra forma
# Criar a interação entre Trataments e Temp
dados$Interacao_Trataments_Temp <- dados$Trataments * dados$Temp

# Ajustar o modelo glm com a nova interação
modelo <- glm(Grain ~ TotalBiomass:TratamentsElev, 
              family = Gamma(link = "inverse"), data = dados)

# Verificar o resumo do modelo
summary(modelo)
