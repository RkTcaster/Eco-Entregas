# # Limpio la memoria
rm(list = ls()) # remuevo todos los objetos
gc() # garbage collection

require("data.table")
require("lightgbm")
require(ggplot2)

#-----------------------------------CARGO DATOS DE GANANCIAS----------------------------------#
setwd("C:\\Users\\lrktl\\OneDrive\\Escritorio\\Maestria\\Primero\\Economia\\dmeyf2023 - Copy\\src\\Colaborativos")

datos_baseline <- fread("./exp/ES_01/ES_01resultados_ganancia_sem10.csv")
datos_ensembles <- fread("./exp/ES_02/ES_02resultados_ganancia_cv3f50.csv")
datos_cv5 <- fread("./exp/ES_02/ES_02resultados_ganancia_cv5f.csv")




# Scatterplot
scatterplot <- ggplot() +
  geom_point(data = datos_baseline, aes(x = as.numeric(factor(semilla)), y = ganancia, color = "Baseline"), size = 3) +
  geom_point(data = datos_ensembles, aes(x = ganancia, y = ganancia, color = "CV3"), size = 3) +
  geom_point(data = datos_cv5, aes(x = ganancia, y = ganancia, color = "CV5"), size = 3) +
  labs(title = "Comparación de Ganancias entre modelos: semillas sueltas & ensembles",
       x = "",  # Oculta las etiquetas del eje x
       y = "Ganancia") +
  theme_minimal() +
  scale_x_continuous(breaks = NULL) +
  scale_color_manual(values = c("Baseline" = "blue", "CV3" = "green", "CV5" = "red"))

ggsave("ganancias_modelos_vs.png", scatterplot, width = 8, height = 5, units = "in", bg = "white")
print(scatterplot)
# Gráfico de densidad
density_plot <- ggplot() +
  geom_density(data = datos_baseline, aes(x = ganancia, fill = "Baseline"), alpha = 0.5) +
  geom_density(data = datos_ensembles, aes(x = ganancia, fill = "CV3"), alpha = 0.5) +
  geom_density(data = datos_cv5, aes(x = ganancia, fill = "CV5"), alpha = 0.5) +
  labs(title = "Distribución de Ganancias",
       x = "Ganancia",
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(values = c("Baseline" = "blue", "CV3" = "green","CV5"="red"))

ggsave("distribucion_densidad_vs.png", density_plot, width = 8, height = 5, units = "in", bg = "white")
print(density_plot)
