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

install.packages("DHARMa")


library(caret)
library(performance)
library(DHARMa)

getwd()
setwd("C:/Users/LAFIECO/Downloads")

df <- read.csv("DadosModeloMultivariadoSoja.csv", sep=";")
df
str(df)
df$Trataments <- as.factor(df$Trataments)
str(df)

# drop coluna
df <- subset(df, select = -Tratament_numeric)
df

# Criar variáveis dummy usando dummyVars
dummy_var <- dummyVars(~ Trataments, data = df)

# Transformar os dados originais usando predict
df_dummies <- predict(dummy_var, newdata = df)

# Adicionar as dummies ao conjunto de dados original
df_dummies <- cbind(df, df_dummies)

# Exibir o resultado
print(df_dummies)

str(df_dummies)

## pela analise de correleacao realizada na analise exploratoria
# optou por realizar o modelo com as variaveis com uma relacao mais linear com a resposta do grao
# ajuste modelo resposta grao por total biomassa

modelo <- lm(Grain ~ TotalBiomass:Trataments -1, data = df_dummies)  # coloca -1 para o intercepto ser zero e ficar a mesma baseline para todos os tratamentos
summary(modelo)
plot(modelo)  # olhando os residuos do modelo

# Se precisar excluir uma parte dos dados
#df_dummies <- df_dummies[df$Grain != 0.000001, ]

######################
# MODELO GLM
# para este script consultei o material disponivel em
# https://analises-ecologicas.com/cap8

modelo <- glm(Grain ~ TotalBiomass:Trataments, data = df_dummies, family='Gamma'(link='log'))  # coloca -1 para o intercepto ser zero e ficar a mesma baseline para todos os tratamentos
summary(modelo)
plot(modelo)  # olhando os residuos do modelo


# modelo link inverse
modelo <- glm(Grain ~ TotalBiomass:Trataments, data = df_dummies, family='Gamma'(link='inverse'))  # coloca -1 para o intercepto ser zero e ficar a mesma baseline para todos os tratamentos
summary(modelo)
plot(modelo)

## Diagnose básica
par(mfrow = c(2, 2))
plot(modelo)

## Diagnose avançada
simulationOutput <- simulateResiduals(fittedModel = modelo, plot = TRUE)

# Criar a interação do modelo

# Criar variáveis indicadoras para os tratamentos
dados$Amb <- as.numeric(dados$Trataments == "Amb")
dados$Elev <- as.numeric(dados$Trataments == "Elev")
dados$AmbTemp <- as.numeric(dados$Trataments == "AmbTemp")
dados$AmbDrought <- as.numeric(dados$Trataments == "AmbDrought")
dados$ElevDrought <- as.numeric(dados$Trataments == "ElevDrought")
dados$ElevTemp <- as.numeric(dados$Trataments == "ElevTemp")


dados
# Criar a interação entre Amb e Elev
dados$Interacao <- dados$Amb * dados$Elev

# Ajustar o modelo glm com a interação
modelo <- glm(Grain ~ Amb + Elev + Interacao, data = dados, family = gaussian)

# Verificar o resumo do modelo
summary(modelo)














# Explorando um pouco as possibilidades para modelo com resposta binária
pontodecorte = 2 # revisar
df_dummies$Grain_not0 = 0
df_dummies$Grain_not0[which(df$Grain > pontodecorte)] = 1 
modelo <- glm(Grain_not0 ~ TotalBiomass:Trataments -1, data = df_dummies, family='binomial'(link='logit'))  # coloca -1 para o intercepto ser zero e ficar a mesma baseline para todos os tratamentos
summary(modelo)
plot(modelo)  # olhando os residuos do modelo

# verificando a distribuicao dos Resíduos
residuos <- residuals(modelo)
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

#verificando residuos depois da transformacao
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

