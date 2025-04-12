# Cargar librerías necesarias
library(tidyverse)
library(vegan) # Para análisis multivariado
library(corrplot) # Para matrices de correlación
library(ggplot2)
library(ggpubr)

# Cargar los datos
datos <- read.csv("C:/Users/kevin/Downloads/datos_estuario_simulados.csv")

# Ver estructura de los datos
str(datos)
summary(datos)

# Resumen estadístico
summary_stats <- datos %>% 
  select(-matches("ind|CPUE")) %>% 
  psych::describe() %>% 
  round(3)

# Guardar en archivo
write.csv(summary_stats, "resumen_estadistico.csv")

# Matriz de correlación
cor_matrix <- cor(datos %>% select(Salinidad, Temperatura, Oxígeno.Disuelto, Hg, Pb, Cd, HAPs, CPUE))
corrplot(cor_matrix, method = "circle", type = "upper")

# Gráficos de pares
pairs(datos[,1:7], pch = 19, lower.panel = NULL)

#################################### mercurio
# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Hg, y = Ostras)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Mercurio y Abundancia de Ostras",
       x = "Concentración de Hg (ppm)", y = "ostras (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Hg, y = Cangrejos)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Mercurio y Abundancia de Cangrejos",
       x = "Concentración de Hg (ppm)", y = "cangrejos (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Hg, y = Moluscos)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Mercurio y Abundancia de Moluscos",
       x = "Concentración de Hg (ppm)", y = "Moluscos (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Hg, y = Camarones)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Mercurio y Abundancia de Camarones",
       x = "Concentración de Hg (ppm)", y = "camarones (ind/m²)")

#############################################################plomo
# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Pb, y = Ostras)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Plomo y Abundancia de Ostras",
       x = "Concentración de Pb (ppm)", y = "ostras (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Pb, y = Cangrejos)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Plomo y Abundancia de Cangrejos",
       x = "Concentración de Pb (ppm)", y = "cangrejos (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Pb, y = Moluscos)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Plomo y Abundancia de Moluscos",
       x = "Concentración de Pb (ppm)", y = "Moluscos (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Pb, y = Camarones)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Plomo y Abundancia de Camarones",
       x = "Concentración de Pb (ppm)", y = "camarones (ind/m²)")

################################################# cadmio 
# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Cd, y = Ostras)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Cadmio y Abundancia de Ostras",
       x = "Concentración de Cd (ppm)", y = "ostras (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Cd, y = Cangrejos)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Cadmio y Abundancia de Cangrejos",
       x = "Concentración de Cd (ppm)", y = "cangrejos (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Cd, y = Moluscos)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Cadmio y Abundancia de Moluscos",
       x = "Concentración de Cd (ppm)", y = "Moluscos (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = Cd, y = Camarones)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Cadmio y Abundancia de Camarones",
       x = "Concentración de Cd (ppm)", y = "camarones (ind/m²)")

################################################# Hidrocarburos Aromáticos Policíclicos
# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = HAPs, y = Ostras)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Hidrocarburos Aromáticos y Abundancia de Ostras",
       x = "Concentración de HPAs (ppm)", y = "ostras (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = HAPs, y = Cangrejos)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Hidrocarburos Aromáticos Policíclicos y Abundancia de Cangrejos",
       x = "Concentración de HPAs (ppm)", y = "cangrejos (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = HAPs, y = Moluscos)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Hidrocarburos Aromáticos Policíclicos y Abundancia de Moluscos",
       x = "Concentración de HPAs (ppm)", y = "Moluscos (ind/m²)")

# Relación entre contaminantes y abundancia de especies
ggplot(datos, aes(x = HAPs, y = Camarones)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Hidrocarburos Aromáticos Policíclicos y Abundancia de Camarones",
       x = "Concentración de HPAs (ppm)", y = "camarones (ind/m²)")
########################################################

# Boxplots por variables
datos_long <- datos %>% 
  pivot_longer(cols = c(Ostras, Cangrejos, Camarones, Moluscos),
               names_to = "Especie", values_to = "Abundancia")

ggplot(datos_long, aes(x = Especie, y = Abundancia, fill = Especie)) +
  geom_boxplot() +
  theme_minimal()

# Preparar datos: variables ambientales vs comunidad biológica
env_vars <- datos %>% select(Salinidad, Temperatura, Oxígeno.Disuelto, Hg, Pb, Cd, HAPs)
bio_vars <- datos %>% select(Ostras, Cangrejos, Camarones, Moluscos)

# Estandarizar datos
env_vars_std <- decostand(env_vars, method = "standardize")
bio_vars_std <- decostand(bio_vars, method = "hellinger")

# Realizar CCA
cca_result <- cca(bio_vars_std ~ ., data = env_vars_std)

# Resumen del análisis
summary(cca_result)

# Visualización
plot(cca_result, display = c("sp", "cn"), main = "Análisis de Correspondencia Canónica")

# Modelo para CPUE en función de variables ambientales
modelo_cpue <- lm(CPUE ~ Salinidad + Temperatura + Oxígeno.Disuelto + Hg + Pb + Cd + HAPs, 
                  data = datos)
summary(modelo_cpue)

# Modelo para CPUE en función de abundancia de especies
modelo_cpue_especies <- lm(CPUE ~ Ostras + Cangrejos + Camarones + Moluscos, 
                           data = datos)
summary(modelo_cpue_especies)

###### mapa de calor 
# Cargar librerías adicionales necesarias
library(reshape2) # Para manipulación de datos
library(viridis) # Para paleta de colores atractiva
library(dplyr) # Para el operador %>%
library(tidyr) # Para pivot_longer
library(ggplot2)

# Seleccionar y estandarizar las variables para el mapa de calor
datos_heatmap <- datos %>%
  select(Salinidad, Temperatura, Oxígeno.Disuelto, 
         Hg, Pb, Cd, HAPs, CPUE,
         Ostras, Cangrejos, Camarones, Moluscos) %>%
  scale() %>% # Estandarizar los datos (z-scores)
  as.data.frame()

# Calcular matriz de correlación
cor_matrix <- cor(datos_heatmap, use = "complete.obs")

# Crear versión "derretida" para ggplot
melted_cormat <- melt(cor_matrix)

# Crear el mapa de calor
ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Correlación", 
                     limits = c(-1, 1), 
                     option = "magma") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Mapa de Calor: Correlaciones entre Variables del Estuario",
       x = "", y = "") +
  coord_fixed()
