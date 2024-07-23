# Carregar a biblioteca necessária
install.packages("glmnet")
library(glmnet)

# Criar um dataframe com os dados fornecidos
dados <- data.frame(
  Tratamentos = rep(c("Amb", "AmbTemp", "Elev", "ElevTemp", "AmbDrought", "ElevDrought"), each = 10),
  TotalBiomass = c(8.1, 10.4, 8.7, 12.1, 11.5, 6.3, 7, 9.1, 10.2, 11.1, 9.2, 9.2, 8.5, 6.7, 8.4, 8.9, 8.6, 9.4, 9.3, 9.3, 18.9, 18.6, 20.5, 19, 15.4, 17.2, 24.6, 20.1, 21.8, 15.1, 18.2, 16.9, 19.8, 15.7, 21.9, 12.1, 17.6, 18.6, 23, 15.7, 7.2, 8.6, 7.2, 7.5, 7.5, 8.3, 8.7, 6.3, 8.7, 7.9, 10.2, 7, 8, 10.3, 12.3, 10.9, 8.4, 19.7, 12.3, 15.4),
  Grain = c(8.7, 13.3, 7.6, 9.6, 10.8, 4.7, 9, 5.9, 4.2, 5, 0.1, 0.000001, 0.8, 0.000001, 0.2, 0.000001, 0.000001, 1.1, 1.5, 0.4, 12.2, 12.7, 21, 11.5, 15.1, 10.4, 24.6, 21.8, 12, 18.1, 10.2, 2.8, 16.1, 25.2, 19, 15.1, 22.4, 16.8, 10.1, 17.5, 2.7, 2.6, 1.5, 3.6, 5.7, 4, 2, 2.7, 3.6, 2.3, 5.1, 4, 2.1, 4.1, 4.8, 6.1, 1.8, 4.7, 0.8, 3.7)
)

# Ajustar o modelo GLM com interação
modelo <- glm(Grain ~ ElevDrought:Elev, data = dados, family = gaussian)

# Visualizar o sumário do modelo
summary(modelo)

modelo <- glm(Grain ~ Tratamentos:"Amb", data = dados, family = gaussian)

# Visualizar o sumário do modelo
summary(modelo)

str(dados)

dados$Tratamentos <- as.factor(dados$Tratamentos)
str(dados)

# Acessar os fatores dentro de 'Tratamentos'
levels_tratamentos <- levels(dados$Tratamentos)

# Exibir os níveis dos fatores
print(levels_tratamentos)

# Acessar um fator específico
amb_indices <- which(dados$Tratamentos == "Amb")
amb_dados <- dados[amb_indices, ]
print(amb_dados)

# Se você quiser acessar todos os níveis de uma vez, pode iterar sobre os níveis
for (nivel in levels_tratamentos) {
  dados_nivel <- dados[dados$Tratamentos == nivel, ]
  print(paste("Dados para o nível", nivel, ":"))
  print(dados_nivel)
}

