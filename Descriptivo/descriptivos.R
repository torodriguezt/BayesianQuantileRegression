library(viridis)
library(stringr)
library(RColorBrewer)

datos <- read.csv("completos_datos/completos_datos.csv")


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

mapa_Colombia <- st_read("Municipios Colombia/Piloto_Desigualdades.shp")
mapa_Colombia$MpNombre <- toupper(mapa_Colombia$MpNombre)

mapa_Colombia <- mapa_Colombia %>%
  filter(mapa_Colombia$Depto == "Antioquia")

df_promedios <- datos %>%
  group_by(datos$COLE_MCPIO_UBICACION) %>%
  summarise(mean_puntaje = mean(PUNT_GLOBAL, na.rm = TRUE),
            cluster_muni = first(ClusterMuni))

df_promedios[df_promedios$`datos$COLE_MCPIO_UBICACION` == "SANTA FÉ DE ANTIOQUIA", "datos$COLE_MCPIO_UBICACION"] <- "SANTA FE DE ANTIOQUIA"
df_promedios[df_promedios$`datos$COLE_MCPIO_UBICACION` == "VALPARAÍSO", "datos$COLE_MCPIO_UBICACION"] <- "VALPARAISO"
df_promedios[df_promedios$`datos$COLE_MCPIO_UBICACION` == "RETIRO", "datos$COLE_MCPIO_UBICACION"] <- "EL RETIRO"
df_promedios[df_promedios$`datos$COLE_MCPIO_UBICACION` == "EL CARMEN DE VIBORAL", "datos$COLE_MCPIO_UBICACION"] <- "CARMEN DE VIBORAL"
df_promedios[df_promedios$`datos$COLE_MCPIO_UBICACION` == "EL SANTUARIO", "datos$COLE_MCPIO_UBICACION"] <- "SANTUARIO"
df_promedios[df_promedios$`datos$COLE_MCPIO_UBICACION` == "ENTRERRÍOS", "datos$COLE_MCPIO_UBICACION"] <- "ENTRERRIOS"
df_promedios[df_promedios$`datos$COLE_MCPIO_UBICACION` == "SAN ANDRÉS DE CUERQUÍA", "datos$COLE_MCPIO_UBICACION"] <- "SAN ANDRÉS DE CUERQUIA"
df_promedios[df_promedios$`datos$COLE_MCPIO_UBICACION` == "SAN VICENTE FERRER", "datos$COLE_MCPIO_UBICACION"] <- "SAN VICENTE"

mapa_Colombia <- mapa_Colombia[-124, ]
mapa_Colombia <- mapa_Colombia[-71, ]


df_combinado <- merge(mapa_Colombia, df_promedios, by.x = "MpNombre", by.y = "datos$COLE_MCPIO_UBICACION", all.x = TRUE)


# Modificar la escala de colores a una paleta de azules

ggplot(data = df_combinado) +
  geom_sf(aes(fill = mean_puntaje)) + 
  scale_fill_viridis(option = "D", name = "Puntaje", direction = -1) +  # Usar viridis en su forma continua suave
  theme_minimal() + 
  labs(title = "Puntaje global promedio por municipio en Antioquia") +
  theme(legend.position = "right")  # Posición de la leyenda



ggplot(data = df_combinado) +
  geom_sf(aes(fill = factor(cluster_muni))) +  # Convertir cluster_muni a factor
  scale_fill_viridis_d(option = "D", name = "Cluster") +  # Paleta discreta similar a viridis D
  theme_minimal() + 
  labs(title = "Clusterización de cada municipio según el puntaje promedio") +
  theme(legend.position = "right")  # Posición de la leyenda


custom_palette_green <- c("#55C667FF", "#238A8DFF")  # Seleccionados de la gama verde de viridis

# Gráfico 1: Colegio Público o Privado por Cluster
datos$COLE_NATURALEZA <- str_to_title(datos$COLE_NATURALEZA)

ggplot(datos, aes(x = COLE_NATURALEZA, fill = COLE_NATURALEZA)) +
  geom_bar() +
  facet_wrap(~ ClusterMuni, labeller = labeller(ClusterMuni = function(x) paste("Cluster", x)), ncol = 4) + 
  scale_fill_manual(values = custom_palette_green) +  # Aplicar la paleta personalizada verde
  labs(title = "Colegio Público o Privado por Cluster",
       x = "Naturaleza del Colegio",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 2: Presencia de Internet por Cluster
datos$FAMI_TIENEINTERNET <- str_to_title(datos$FAMI_TIENEINTERNET)

ggplot(datos, aes(x = FAMI_TIENEINTERNET, fill = FAMI_TIENEINTERNET)) +
  geom_bar() +
  facet_wrap(~ ClusterMuni, labeller = labeller(ClusterMuni = function(x) paste("Cluster", x)), ncol = 4) + 
  scale_fill_manual(values = custom_palette_green) +  # Aplicar la paleta personalizada verde
  labs(title = "Presencia de Internet por Cluster",
       x = "Tiene Internet",
       y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 3: Cantidad de Estratos por Cluster
ggplot(datos, aes(x = FAMI_ESTRATOVIVIENDA, fill = FAMI_ESTRATOVIVIENDA)) +
  geom_bar() +
  facet_wrap(~ ClusterMuni, labeller = labeller(ClusterMuni = function(x) paste("Cluster", x)), ncol = 4) + 
  scale_fill_viridis_d(option = "D") +  # Aplicar viridis discreta
  labs(title = "Cantidad de Estratos por Cluster",
       x = "Estrato",  # Etiqueta del eje x (inversión por coord_flip)
       y = "Cantidad") +  # Etiqueta del eje y
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()  # Invertir los ejes para barras horizontales