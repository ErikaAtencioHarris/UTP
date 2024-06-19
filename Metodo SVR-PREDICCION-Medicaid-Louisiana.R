# Cargar las librerías necesarias
library(e1071)  # Para SVR
library(caret)  # Para el escalado de datos y la métrica R2

# Datos del problema
data <- data.frame(
  Periodo = c("2018-1", "2018-2", "2018-3", "2018-4", "2019-1", "2019-2", "2019-3", "2019-4",
              "2020-1", "2020-2", "2020-3", "2020-4", "2021-1", "2021-2", "2021-3", "2021-4",
              "2022-1", "2022-2", "2022-3", "2022-4"),
  Trimestre = 1:20,
  Valor = c(275748351.68, 287274070.11, 297470366.84, 310620099.66, 315796559.79, 321865625.32,
            382327101.16, 407439063.85, 436722534.70, 427530420.35, 457000290.99, 481744263.99,
            497351375.65, 513400724.61, 511541532.09, 545833799.13, 575920786.73, 605946785.78,
            632551614.54, 647146675.48)
)

# Escalar los datos
data_scaled <- data
data_scaled$Trimestre <- scale(data$Trimestre)
data_scaled$Valor <- scale(data$Valor)

# Ajustar el modelo SVR
set.seed(123)  # Para reproducibilidad
tuneResult <- tune(svm, Valor ~ Trimestre, data = data_scaled, 
                   ranges = list(cost = 10^(-1:2), gamma = c(0.1, 1, 10)))
best_model <- tuneResult$best.model

# Predecir en el conjunto de entrenamiento usando el modelo SVR
train_scaled <- data.frame(Trimestre = data_scaled$Trimestre)
train_predictions <- predict(best_model, train_scaled)

# Desescalar las predicciones en el conjunto de entrenamiento
train_predictions <- train_predictions * attr(scale(data$Valor), "scaled:scale") + attr(scale(data$Valor), "scaled:center")

# Calcular el error de las predicciones en el conjunto de entrenamiento
residuals <- data$Valor - train_predictions

# Calcular las métricas de evaluación
# Mean Absolute Deviation (MAD)
MAD <- mean(abs(residuals))

# Mean Absolute Percentage Error (MAPE)
MAPE <- mean(abs(residuals / data$Valor)) * 100

# Desviación estándar de los errores
std_dev <- sd(residuals)

# Coeficiente de Determinación (R2)
R2 <- caret::R2(train_predictions, data$Valor)

# Calcular la Media del Error
mean_error <- mean(residuals)

# Calcular el Rango TS para los errores
z_value <- 1.96  # Para un intervalo de confianza del 95%
n <- length(residuals)  # Número de observaciones

rango_ts_inferior_error <- mean_error - z_value * (std_dev / sqrt(n))
rango_ts_superior_error <- mean_error + z_value * (std_dev / sqrt(n))

cat("\nRango TS Inferior del Error: ", rango_ts_inferior_error, "\n")
cat("\nRango TS Superior del Error: ", rango_ts_superior_error, "\n")

# Predecir los próximos trimestres (21 a 24)
new_trimestres <- data.frame(Trimestre = scale(21:24, center = attr(scale(data$Trimestre), "scaled:center"),
                                               scale = attr(scale(data$Trimestre), "scaled:scale")))
predictions <- predict(best_model, new_trimestres)

# Desescalar las predicciones
predictions <- predictions * attr(scale(data$Valor), "scaled:scale") + attr(scale(data$Valor), "scaled:center")

# Calcular el intervalo de confianza (Rango TS) para las predicciones
confidence_interval <- 1.96 * std_dev
lower_bound <- predictions - confidence_interval
upper_bound <- predictions + confidence_interval

# Imprimir los resultados
resultados <- data.frame(
  Trimestre = 21:24,
  Prediccion = predictions,
  Rango_TS_Inferior = lower_bound,
  Rango_TS_Superior = upper_bound
)

print("Resultados de las predicciones y rangos de confianza:")
print(resultados)

cat("\n")
cat(sprintf("MAD: %.2f\n", MAD))
cat(sprintf("MAPE: %.2f%%\n", MAPE))
cat(sprintf("Desviación Estándar de los residuos: %.2f\n", std_dev))
cat(sprintf("R2: %.2f\n", R2))

# Graficar los resultados
plot(data$Trimestre, data$Valor, type = "b", col = "blue", pch = 16, 
     xlab = "Trimestre", ylab = "Valor", main = "Predicciones SVR y Rangos de Confianza-Desembolso Medicaid-Louisiana",
     xlim = c(1, 24), ylim = c(min(c(data$Valor, lower_bound)), max(c(data$Valor, upper_bound))))
lines(21:24, predictions, type = "b", col = "red", pch = 16)
polygon(c(21:24, rev(21:24)), c(lower_bound, rev(upper_bound)), col = rgb(0.8, 0.8, 0.8, 0.5), border = NA)
legend("topleft", legend = c("Datos Reales", "Predicciones", "Intervalo de Confianza"), 
       col = c("blue", "red", rgb(0.8, 0.8, 0.8, 0.5)), lty = 1, pch = 16, fill = c(NA, NA, rgb(0.8, 0.8, 0.8, 0.5)))

# Mostrar los mejores parámetros encontrados
cat(sprintf("\nMejores parámetros encontrados por GridSearchCV: Cost = %f, Gamma = %f\n", 
            best_model$cost, best_model$gamma))

