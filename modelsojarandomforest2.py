# -*- coding: utf-8 -*-
"""ModelSojaRandomForest2.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1brhRpZPIeSxd7XONBYNK65SWfI9U33_V
"""

import pandas as pd
import numpy as np  # Adicione esta linha
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score

# Carregando o conjunto de dados
data = pd.read_csv('/content/SojaModeloRF.csv', sep=";")  # Substitua 'seu_arquivo.csv' pelo nome do seu arquivo

print(data)

# Selecionando as variáveis preditoras e a variável alvo
features = data[['NumberLeaves', 'NumberFlowers', 'SLA', 'SoilMoisture', 'CO2Assimilation',
                  'StomatalConductance', 'Ci', 'Transpiration', 'L_Starch', 'L_Glucose',
                  'L_Frutose', 'L_Sucrose', 'L_Raffinose']]

target = data['TotalBiomass']

# Dividindo o conjunto de dados em treino e teste
X_train, X_test, y_train, y_test = train_test_split(features, target, test_size=0.2, random_state=42)

# Criando e treinando o modelo Random Forest
rf_model = RandomForestRegressor(random_state=42)
rf_model.fit(X_train, y_train)

# Realizando previsões no conjunto de teste
predictions = rf_model.predict(X_test)

# Avaliando o desempenho do modelo
print('RMSE (teste):', np.sqrt(mean_squared_error(y_test, predictions)))
print('R² (teste):', r2_score(y_test, predictions))

# Criando e treinando o modelo RandomForestRegressor
rf = RandomForestRegressor(random_state=42)
rf.fit(X_train, y_train)

# Fazendo previsões no conjunto de teste
predictions = rf.predict(X_test)

# Mostrando os valores previstos e observados
resultados = pd.DataFrame({'Observado': y_test, 'Predito': predictions})
print(resultados)

import matplotlib.pyplot as plt

# Fazendo previsões no conjunto de teste
predictions = rf.predict(X_test)

# Criando um gráfico de dispersão para comparar os valores observados e previstos
plt.scatter(y_test, predictions)
plt.xlabel('Observated')
plt.ylabel('Predict')
#plt.title('Valores Observados vs. Valores Previstos')
plt.show()

import matplotlib.pyplot as plt

# Fazendo previsões no conjunto de teste
predictions = rf.predict(X_test)

# Criando um gráfico de dispersão para comparar os valores observados e previstos
plt.scatter(y_test, predictions, label='Observed vs. Predict')
plt.plot([min(y_test), max(y_test)], [min(y_test), max(y_test)], '-', color='black', label='Perfect Match Line')
plt.xlabel('Observated')
plt.ylabel('Predict')
#plt.title('Observated value vs. Predict value')
plt.legend()
plt.show()