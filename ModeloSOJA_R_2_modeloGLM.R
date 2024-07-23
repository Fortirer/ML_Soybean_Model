# CITACAO
#Para elaborar esta analise foi consultada o material:
#http://labtrop.ib.usp.br/doku.php?id=cursos:planeco:roteiro:09-lm02b

'''
Analise para o artigo Soja Efeito Triplo

Contextualização: A base de dados contém dados sobre experimento que simularam as alterções climáticas,
com elevado CO2 controle (400ppm) e elevado (800ppm), temperatura ambiente e temperatura (+5°C), 
bem regado e com estresse hídrico (regado a cada 3 dias com 100ml).

    Parte I: análise exploratória dos dados.

    Parte II: análise de correlações.

    Parte III: Modelo multivariado

Obs. todos os dados X são referentes a coleta de dados de 60 dias de experimento Já a variavel resposta grão Y é referente 
ao fim do experimento de 125 dias.

Objetivo do trabalho: Analisar o efeito das mudanças climáticas na produção de soja. 
Em 6 condições (Controle = Amb, Elev, Temp, ElevTemp, Drought, ElevDrought)

Os dados são referentes a 2 experimentos independentes Com N amostral N = 10

Experimento #1 Amb Elev Temp ElevTemp

Experimento #2 Amb Elev Drought ElevDrought

Como tem a repetição das condições Amb e Elev nos dois experimentos resultando em um N = 20 para essas condições. 
Fiz um merge de 5 amostras (random) de cada tratamento Amb e Elev
'''

library(caret)

getwd()
setwd("C:/Users/LAFIECO/Downloads")

df <- read.csv("DadosModeloMultivariadoSoja.csv", sep=";")

df$Trataments <- as.factor(df$Trataments)
str(df)


## pela analise de correleacao realizada na analise exploratoria
# optou por realizar o modelo com as variaveis com uma relacao mais linear com a resposta do grao
# ajuste modelo resposta grao por total biomassa

# Se precisar excluir uma parte dos dados
#df_dummies <- df_dummies[df$Grain != 0.000001, ]

# criando interacao termos para  Elev/Drought and Elev/Temp
df$Amb_Interaction <- as.integer(df$Trataments %in% c('Elev'))


# Converte categorica coluna para numericaca
for (column in names(df)[sapply(df, is.factor)]) {
  df[[column]] <- as.integer(as.numeric(df[[column]]))
}


modelo <- glm(Grain ~ SLA:Amb_Interaction, data = df, family='Gamma'(link='log'))
summary(modelo)

valores_preditos <- predict(modelo, type = "response")

# Calcular os valores observados
valores_observados <- df$Grain
valores_observados
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
tamanho_amostra

# Calcular o erro padrão da média
erro_media <- desvio_padrao / sqrt(tamanho_amostra)
erro_media


########
# modelo logistico

pontodecorte = 2 
df$Grain_not0 = 0
df$Grain_not0[ which(df$Grain > pontodecorte) ] = 1 
modelo <- glm(Grain_not0 ~ TotalBiomass:Elev_Interaction, data = df, family='binomial'(link='logit'))  # coloca -1 para o intercepto ser zero e ficar a mesma baseline para todos os tratamentos
summary(modelo)
plot(modelo)  # olhando os residuos do modelo

###### fim script

str(dados)


modelo <- glm(Grain ~ TotalBiomass:Trataments, data = df_dummies, family='Gamma'(link='log'))  
summary(modelo)
plot(modelo)  # olhando os residuos do modelo


# apareceu warning fiz com familia inversa
modelo_gamma <- glm(Grain ~ TotalBiomass:Trataments, data = df_dummies, family='Gamma'(link='inverse'))  
summary(modelo_gamma)
plot(modelo_gamma)  # olhando os residuos do modelo

# Explorando um pouco as possibilidades para modelo com resposta binária
pontodecorte = 2 
df_dummies$Grain_not0 = 0
df_dummies$Grain_not0[ which(df$Grain > pontodecorte) ] = 1 
modelo_binomial <- glm(Grain_not0 ~ TotalBiomass:Trataments, data = df_dummies, family='binomial'(link='logit'))  # coloca -1 para o intercepto ser zero e ficar a mesma baseline para todos os tratamentos
summary(modelo_binomial)
plot(modelo_binomial)  # olhando os residuos do modelo


# testando interação
modelo <- glm(Grain ~  TotalBiomass*Trataments.Elev * TotalBiomass*Trataments.Amb, data = df_dummies, family='Gamma'(link='inverse'))  
summary(modelo)
plot(modelo) 

str(df_dummies)
modelo_gamma <- glm(Grain ~ Trataments, data = df_dummies, family='Gamma'(link='inverse'))  
summary(modelo_gamma)
plot(modelo_gamma)

df_dummies$Trataments.Amb <- as.factor(df_dummies$Trataments.Amb)
df_dummies$Trataments.Elev <- as.factor(df_dummies$Trataments.Elev)


library(plyr)
revalue(df_dummies$Trataments.Amb, c("0"="-1"))
revalue(df_dummies$Trataments.Elev, c("0"="-1"))

# verificando a distribuicao dos Resíduos
residuos <- residuals(modelo_linear)
shapiro.test(residuos)  # teste deu < 0.05 entao rejeita a hipotese nula de normalidade. os dados nao segeum distribuicao norma -> 
                        # proximo passo transformar dados para ver


# deu dados infinitos pq os graos em alta temperatura deram ZEro graos
# opcao remover ? NA  ou ajustar para nao dar infito
# optei por colocar 0.00001 onde a producao de grao foi ZERO
# df_dummies <- df_dummies[is.finite(df_dummies$log_Grain), ]
#  dados transformados
df_dummies$log_Grain <- log(df_dummies$Grain)
df_dummies$log_TotalBiomass <- log(df_dummies$TotalBiomass)

# Ajustar o modelo linear
modelo <- lm(log_Grain ~ TotalBiomass:Trataments-1, data = df_dummies)
summary(modelo)

# verificando residuos depois da transformacao
# verificando a distribuicao dos Resíduos
residuos <- residuals(modelo)
shapiro.test(residuos)  # teste deu < 0.05 entao rejeita a hipotese nula de normalidade. os dados nao segeum distribuicao norma 
plot(modelo) # piorou a distribuicao dos dados

# transformação Box-Cox
library(MASS)

modelo <- lm(Grain ~ TotalBiomass:Trataments - 1, data = df_dummies)
summary(modelo)

# transformação Box-Cox - 
modelo_boxcox <- boxcox(modelo)
summary(modelo)

residuos <- residuals(modelo)
shapiro.test(residuos)  # nao resolveu fazer transformacao box-cox

# Exibir resumo do modelo
summary(modelo)

# apenas olhando predito e observado, só para verificar ate pq os residuos nao deram normal
# predito versus observado
# Adicionar preditos e observados a um novo dataframe
resultado <- data.frame(
  Observado = df$Grain,
  Predito = predict(modelo)
)

print(resultado)
plot(resultado)

