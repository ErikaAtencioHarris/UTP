# Cargar librerías necesarias
#install.packages(c("dplyr", "ggplot2", "caret", "e1071", "readr"))
library(dplyr)
library(ggplot2)
library(caret)
library(e1071)
library(readr)

# Supongamos que el archivo CSV está guardado como "data.csv"
# data <- read_csv("path/to/data.csv")

# Crear datos simulados basados en la imagen proporcionada (modificar con el archivo original si es necesario)
data <- data.frame(
  AñoTrimestre = c("2018-1", "2018-2", "2018-3", "2018-4", "2019-1", "2019-2", "2019-3", "2019-4", 
                   "2020-1", "2020-2", "2020-3", "2020-4", "2021-1", "2021-2", "2021-3", "2021-4", 
                   "2022-1", "2022-2", "2022-3", "2022-4"),
  Año = c(2018, 2018, 2018, 2018, 2019, 2019, 2019, 2019, 2020, 2020, 2020, 2020, 2021, 2021, 2021, 2021, 2022, 2022, 2022, 2022),
  Trimestre = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4),
  MontoTotalPagadoAProveedor = c(282700754.36, 295015097.33, 304125395.51, 317851057.48, 323452665.09, 
                                 329637010.24, 390004045.89, 414299181.19, 437968483.20, 429272414.09, 
                                 458074823.22, 482561949.56, 498316010.19, 514615029.54, 512881529.87, 
                                 547017166.37, 577027348.90, 606964930.68, 633558070.62, 647946636.10),
  MontoCubiertoSeguroMedicaid = c(275748351.68, 287274070.11, 297470366.84, 310620099.66, 315796559.79, 
                                  321865625.32, 382327101.16, 407439063.85, 436722534.70, 427530420.35, 
                                  457000290.99, 481744263.99, 497351375.65, 513400724.61, 511541532.09, 
                                  545833799.13, 575920786.73, 605946785.78, 632551614.54, 647146675.48),
  UnidadesMedicamentosReembolsadas = c(254172254.58, 250901299.10, 250116266.41, 257888796.52, 266952665.57, 
                                       238102136.77, 244949556.08, 254965370.45, 257516074.14, 220628505.23, 
                                       237634704.24, 245840162.99, 243712369.36, 258791157.25, 252010386.07, 
                                       259720836.79, 258235857.45, 261428871.00, 268430721.24, 278642943.28),
  ProporcionPrecioUnidad = c(1.11, 1.18, 1.22, 1.23, 1.21, 1.38, 1.59, 1.62, 1.70, 1.95, 1.93, 1.96, 
                             2.04, 1.99, 2.04, 2.11, 2.23, 2.32, 2.36, 2.33),
  CantidadProveedores = c(550, 556, 567, 564, 576, 594, 594, 589, 602, 593, 598, 596, 602, 605, 603, 
                          607, 608, 604, 621, 629),
  Cuarentena = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
)

# Crear la variable de precio por unidad
data$PrecioPorUnidad <- data$MontoTotalPagadoAProveedor / data$UnidadesMedicamentosReembolsadas

# Filtrar los datos para el entrenamiento (antes de cuarentena y no en cuarentena)
train_data <- data %>% filter(Cuarentena == 0)

# Ajustar un modelo de regresión lineal
model <- lm(PrecioPorUnidad ~ Año + Trimestre + UnidadesMedicamentosReembolsadas + CantidadProveedores, data = train_data)

# Resumen del modelo para ver los coeficientes
summary(model)

# Predecir para los datos en cuarentena
predict_data <- data %>% filter(Cuarentena == 1)
predict_data$PrecioPorUnidad_Pred <- predict(model, newdata = predict_data)

# Crear una tabla comparativa
comparison <- predict_data %>%
  select(AñoTrimestre, Año, Trimestre, PrecioPorUnidad, PrecioPorUnidad_Pred)

# Imprimir la tabla
print(comparison)

# Visualizar los resultados en una gráfica con la estética `group` correcta
ggplot(comparison, aes(x = AñoTrimestre)) +
  geom_line(aes(y = PrecioPorUnidad, color = "Real", group = 1)) +
  geom_line(aes(y = PrecioPorUnidad_Pred, color = "Predicho", group = 1)) +
  labs(title = "Comparación de precios reales vs estimados en Louisiana", x = "Trimestre", y = "Precio por Unidad") +
  theme_minimal() +
  scale_color_manual(values = c("Real" = "blue", "Predicho" = "red"))
