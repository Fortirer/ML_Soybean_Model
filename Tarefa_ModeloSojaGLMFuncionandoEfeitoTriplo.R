# autoria: Janaina da Silva Fortirer
# email: janainawx@gmail.com

# para este script consultei o material disponivel em
# https://analises-ecologicas.com/cap8

# USANDO ESSE CODIGO PARA RODAR OS MODELOS
#   scrip que aparentemente deu certo interacao 
#########################################################
#      modelo por tratamento 
#########################################################

library(dplyr)

df <- read.csv("DadosModeloMultivariadoSoja.csv", sep=";")
dfAmb <- read.csv("dfAmb.csv", sep=";")


# Verificar a estrutura dos dados
str(df)

df$Trataments <- as.factor(df$Trataments)

df$Triple_Interaction <- as.integer(df$Trataments %in% c('AmbTemp', 'AmbDrought', 'Elev'))

modelo <- glm(Grain ~ TotalBiomass:Triple_Interaction, data = df, family='Gamma'(link='inverse'))
summary(modelo)

valores_preditos <- predict(modelo, type = "response")
valores_preditos

valores_observados <- df$Grain
valores_observados



#CALCULANDO MEDIA, DESVIO, ERROR
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

######################################
# Do to Amb treatment
df <- read.csv("dfElevDrought.csv", sep=";")
modelo <- glm(Grain ~ TotalBiomass, data = df, family='Gamma'(link='inverse'))
modelo <- glm(Grain ~ NumberLeaves, data = df, family='Gamma'(link='inverse'))

summary(modelo)
valores_preditos <- predict(modelo, type = "response")
valores_preditos
valores_observados <- df$Grain
valores_observados


#CALCULANDO MEDIA, DESVIO, ERROR
# VALORES OBSERVADOS
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


