library(ggplot2)
library(ggthemes)
library(sf)
library(dplyr)
library(RColorBrewer)



datos <- read.csv("completos_datos/completos_datos.csv")

custom_palette <- c("#fc8d62", "#66c2a5", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3")



ggplot(datos, aes(x = datos$PUNT_GLOBAL)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +  # Histograma en azul con bordes negros
  labs(title = "Histograma Puntaje Global", x = "Puntaje Global", y = "Frecuencia") + 
  theme_minimal() +  # Tema minimalista
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Título centrado y en negrita
    axis.title = element_text(size = 12),  # Tamaño de las etiquetas de los ejes
    panel.grid.major = element_line(size = 0.5),  # Líneas de cuadrícula más finas
    panel.grid.minor = element_blank()  # Ocultar las líneas menores de la cuadrícula
  )

## Edad
ggplot(datos, aes(x = EDAD_2023)) +
  geom_bar(fill = "#fc8d62", color = "black") +
  scale_x_continuous(breaks = seq(min(datos$EDAD_2023, na.rm = TRUE), 
                                  max(datos$EDAD_2023, na.rm = TRUE), 
                                  by = 5)) +
  labs(title = "Gráfico de barras Edad",
       x = "Edad",
       y = "Frecuencia") +
  theme_minimal() +
  coord_flip()

ggplot(datos, aes(y = EDAD_2023)) + 
  geom_boxplot(fill = "#fc8d62", color = "black") + 
  labs(title = "Distribución de Edad",
       y = "Edad") +
  theme_minimal()

## Estrato
ggplot(datos, aes(x = FAMI_ESTRATOVIVIENDA, fill = FAMI_ESTRATOVIVIENDA)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = custom_palette) + 
  labs(title = "Estratos",
       x = "Categoría",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none")

## Naturaleza del Colegio
ggplot(datos, aes(x = COLE_NATURALEZA, fill = COLE_NATURALEZA)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = custom_palette) +  
  labs(title = "Naturaleza del Colegio",
       x = "Categoría",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none")

## Ubicación del Colegio
ggplot(datos, aes(x = COLE_AREA_UBICACION, fill = COLE_AREA_UBICACION)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = custom_palette) + 
  labs(title = "Ubicación del Colegio",
       x = "Categoría",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none")

## Horas de trabajo a la semana
ggplot(datos, aes(x = ESTU_HORASSEMANATRABAJA, fill = ESTU_HORASSEMANATRABAJA)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = custom_palette) + 
  labs(title = "¿Cuántas horas a la semana trabaja?",
       x = "Categoría",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none")

## Tiene Internet
ggplot(datos, aes(x = FAMI_TIENEINTERNET, fill = FAMI_TIENEINTERNET)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = custom_palette) +  
  labs(title = "Tiene Internet",
       x = "Categoría",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none")

## Tiene Computador
ggplot(datos, aes(x = FAMI_TIENECOMPUTADOR, fill = FAMI_TIENECOMPUTADOR)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = custom_palette) +  
  labs(title = "Tiene Computador",
       x = "Categoría",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none")

## Puntaje según colegio

ggplot(datos, aes(x = factor(COLE_NATURALEZA), y = PUNT_GLOBAL, fill = COLE_NATURALEZA)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_palette) +  
  labs(title = "Puntaje según el colegio",
       x = "Categoría",
       y = "Valor") +
  theme_minimal() +
  theme(legend.position = "none")


## Gráfico de violin puntaje global

ggplot(datos, aes(x = factor(COLE_NATURALEZA), y = PUNT_GLOBAL, fill = COLE_NATURALEZA)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.5, color = "black", alpha = 0.3) +  
  scale_fill_manual(values = custom_palette) + 
  labs(title = "Gráfico de Violin Punatje Global y Naturaleza del Colegio",
       x = "Categoría",
       y = "Puntaje Global") +
  theme_minimal() +
  theme(legend.position = "none")


##Puntaje según el estrato

ggplot(datos, aes(x = factor(FAMI_ESTRATOVIVIENDA), y = PUNT_GLOBAL, fill = FAMI_ESTRATOVIVIENDA)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_palette) +  
  labs(title = "Distribución de valor por categoría",
       x = "Categoría",
       y = "Valor") +
  theme_minimal() +
  theme(legend.position = "none")


##Regresión lineal simple ajustada para cada nivel de los estratos

ggplot(datos, aes(x = PUNT_MATEMATICAS, y = PUNT_GLOBAL, color = FAMI_ESTRATOVIVIENDA)) +
  geom_point(size = 3) +  
  scale_color_manual(values = custom_palette) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Scatterplot Puntaje Matemáticas y Puntaje Global",
       x = "Puntaje Matemáticas",
       y = "Puntaje Global",
       color = "Estrato") + 
  theme_minimal()




##Clusters-------------------------------------------------------------------------

##Boxplot Puntaje por Cluster
custom_palette <- c("#E0F7FA", "#B2EBF2", "#80DEEA", "#4DD0E1", "#26C6DA", 
                    "#00BCD4", "#00ACC1", "#0097A7", "#00838F") 


ggplot(datos, aes(x = factor(ClusterMuni), y = PUNT_GLOBAL, fill = factor(ClusterMuni))) +
  geom_boxplot() +
  scale_fill_manual(values = custom_palette) +  
  labs(title = "Boxplot por Cluster Puntaje Global",
       x = "Cluster",
       y = "Puntaje Global") +
  theme_minimal() +
  theme(legend.position = "none")


##Cantidad de cada estrato por cluster

ggplot(datos, aes(x = FAMI_ESTRATOVIVIENDA, fill = FAMI_ESTRATOVIVIENDA)) +
  geom_bar() +
  facet_wrap(~ ClusterMuni, labeller = labeller(ClusterMuni = function(x) paste("Cluster", x)), ncol = 4) + 
  labs(title = "Cantidad de Estratos por Cluster",
       x = "Cantidad",  # Etiqueta del eje x (inversión por coord_flip)
       y = "Estrato") +  # Etiqueta del eje y (inversión por coord_flip)
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()  # Invertir los ejes para barras horizontales

##Edad por cada cluster

ggplot(datos, aes(x = EDAD_2023)) +
  geom_histogram(binwidth = 4) +
  facet_wrap(~ ClusterMuni, labeller = labeller(ClusterMuni = function(x) paste("Cluster", x)), ncol = 4) + 
  labs(title = "Edad por Cluster",
       x = "Estrato",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


##Cantidad de tipo de colegio por cluster
library(stringr)  # Cargar stringr para usar str_to_title

datos$COLE_NATURALEZA <- str_to_title(datos$COLE_NATURALEZA)

# Crear el gráfico
ggplot(datos, aes(x = COLE_NATURALEZA, fill = COLE_NATURALEZA)) +
  geom_bar() +
  facet_wrap(~ ClusterMuni, labeller = labeller(ClusterMuni = function(x) paste("Cluster", x)), ncol = 4) + 
  labs(title = "Colegio Público o Privado por Cluster",
       x = "Naturaleza del Colegio",  # Ajusta la etiqueta del eje x
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

##Tiene Internet o No por Cluster

datos$COLE_NATURALEZA <- str_to_title(datos$FAMI_TIENEINTERNET)

ggplot(datos, aes(x = FAMI_TIENEINTERNET, fill = FAMI_TIENEINTERNET)) +
  geom_bar() +
  facet_wrap(~ ClusterMuni, labeller = labeller(ClusterMuni = function(x) paste("Cluster", x)), ncol = 4) + 
  labs(title = "Presencia de internet por cluster",
       x = "Estrato",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


##Cantidad de horas trabajadas

horas <- datos %>%
  mutate(ESTU_HORASSEMANATRABAJA = recode(ESTU_HORASSEMANATRABAJA, "0" = "0 horas"))

ggplot(horas, aes(x = factor(ESTU_HORASSEMANATRABAJA), fill = factor(ESTU_HORASSEMANATRABAJA))) +
  geom_bar() +
  facet_wrap(~ ClusterMuni, labeller = labeller(ClusterMuni = function(x) paste("Cluster", x)), ncol = 4) + 
  labs(title = "Distribución de Horas de Trabajo por Cluster",
       x = "Horas de Trabajo",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datos, aes(x = COLE_AREA_UBICACION, fill = COLE_AREA_UBICACION)) +
  geom_bar() +
  facet_wrap(~ ClusterMuni, labeller = labeller(ClusterMuni = function(x) paste("Cluster", x)), ncol = 4) + 
  labs(title = "Tipo de Colegio por Cluster",
       x = "Estrato",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(datos, aes(x = PUNT_MATEMATICAS, y = PUNT_GLOBAL, color = factor(ClusterMuni))) +
  geom_point(size = 3) +  
  geom_smooth(method = "lm", se = FALSE, aes(group = ClusterMuni)) + 
  scale_color_manual(values = custom_palette) + 
  labs(title = "Scatterplot Puntaje Matemáticas y Puntaje Global",
       x = "Puntaje Matemáticas",
       y = "Puntaje Global",
       color = "Estrato") + 
  theme_minimal()




##Graficos Mapas

##Antioquia--------------------------------------------------------------------------------------



df_promedios <- datos %>%
  group_by(datos$COLE_MCPIO_UBICACION) %>%
  summarise(mean_puntaje = mean(PUNT_GLOBAL, na.rm = TRUE))

df_promedios <- datos %>%
  group_by(COLE_MCPIO_UBICACION) %>%
  summarise(
    mean_puntaje = mean(PUNT_GLOBAL, na.rm = TRUE),
    cluster_muni = first(ClusterMuni)  # o puedes usar unique(CLUSTER_MUNI)
  )

df_promedios[df_promedios$`datos$COLE_MCPIO_UBICACION` == "GÓMEZ PLATA", "datos$COLE_MCPIO_UBICACION"] <- "GOMEZ PLATA"


mapa_antioquia <- st_read("Municipios Antioquia/ESE_Capacidad_Instalada.shp")

municipios_comunes <- intersect(df_promedios$`datos$COLE_MCPIO_UBICACION`, mapa_antioquia$MPIO_NOMBR)

mapa_antioquia <- mapa_antioquia %>%
  filter(MPIO_NOMBR %in% municipios_comunes)

mapa_antioquia <- mapa_antioquia %>%
  mutate(new_column = if_else(MPIO_NOMBR == df_promedios$`datos$COLE_MCPIO_UBICACION`[match(MPIO_NOMBR, df_promedios$`datos$COLE_MCPIO_UBICACION`)], MPIO_NOMBR, NA_character_))



df_combinado <- merge(mapa_antioquia, df_promedios, by.x = "MPIO_NOMBR", by.y = "datos$COLE_MCPIO_UBICACION", all.x = TRUE)


ggplot(data = df_combinado) +
  geom_sf(aes(fill = mean_puntaje)) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),  
                       name = "Puntaje") +  
  theme_minimal() + 
  labs(title = "Mapa de Intensidad por Puntaje Antiqouia")

##Algunos municipios no aparecen ya que no los tiene la base

df_filtrado <- subset(datos, COLE_DEPTO_UBICACION == "ANTIOQUIA")
municipios <- unique(df_filtrado$COLE_MCPIO_UBICACION)

diferencia <- setdiff(mapa_antioquia$MPIO_NOMBR, municipios)


##Antioquia y otras Zonas----------------------------------------------------------------------------------------------
##Se puede ver que municipios fuera de Antioquia se tienen muy pocos por lo que no es tan
##util este grafico


mapa_Colombia <- st_read("Municipios Colombia/Piloto_Desigualdades.shp")
mapa_Colombia$MpNombre <- toupper(mapa_Colombia$MpNombre)

mapa_Colombia <- mapa_Colombia %>%
                filter(mapa_Colombia$Depto == "Antioquia")

municipios_comunes <- intersect(datos$COLE_MCPIO_UBICACION, mapa_Colombia$MpNombre)

mapa_Colombia <- mapa_Colombia %>%
  filter(MpNombre %in% municipios_comunes)


mapa_Colombia <- mapa_Colombia %>%
  mutate(new_column = if_else(MpNombre == df_promedios$`datos$COLE_MCPIO_UBICACION`[match(MpNombre, df_promedios$`datos$COLE_MCPIO_UBICACION`)], MpNombre, NA_character_))

df_combinado_Colombia <- merge(mapa_Colombia, df_promedios, by.x = "MpNombre", by.y = "datos$COLE_MCPIO_UBICACION", all.x = TRUE)

ggplot(data = df_combinado_Colombia) +
  geom_sf(aes(fill = mean_puntaje)) + 
  scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"),  
                       name = "Puntaje") +  
  theme_minimal() + 
  labs(title = "Mapa de Intensidad por Puntaje Antiqouia")