getwd()
setwd("C:/Users/LAFIECO/Downloads")


# vai mudando o nome do arquivo limpar a cash e apagar as memórias dos objetos
# os arquivos estao somento com um tratamento, por exemplo so com AMb ou Elev, etc
# fazer para cada tratamento a funcao modelo <- glm(Grain ~ TotalBiomass, data = df, family='Gamma'(link='log'))
# depois na interecao rodar com outro script antes
# e ver a interacao 

df <- read.csv("DadosModeloTriploGLM.csv", sep=";")

df$Trataments <- as.factor(df$Trataments)
str(df)

modelo <- glm(grain ~ TotalBiomass, data = df, family='Gamma'(link='log')) # ver link log
summary(modelo)


valores_preditos <- predict(modelo, type = "response")
# Calcular os valores observados
valores_observados <- df$Grain

# Calcular o tamanho da amostra
tamanho_amostra <- length(valores_observados)

# VALORES OBSERVADOR
# Calcular a média
media <- mean(valores_observados)
media

# Calcular o desvio padrão
desvio_padrao <- sd(valores_observados)
desvio_padrao

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

# Calcular o erro padrão da média
erro_media <- desvio_padrao / sqrt(tamanho_amostra)
erro_media


###########
# MODELO COM INTERACAO

df$Temp_Drought_Interaction <- as.integer(df$Trataments %in% c('Elev', 'AmbDrought','AmbTemp'))

df$Dro <- as.integer(df$Trataments %in% c('AmbDrought'))


# Converte categorica coluna para numericaca
for (column in names(df)[sapply(df, is.factor)]) {
  df[[column]] <- as.integer(as.numeric(df[[column]]))
}

modelo <- glm(Grain ~ NumberLeaves:Temp_Drought_Interaction, data = df, family='Gamma'(link='inverse'))


