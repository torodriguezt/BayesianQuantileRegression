library(rstan)


# Leer el archivo CSV
datos_sub <- read.csv("completos_datos.csv", header = TRUE, sep = ",")[1:35000,]

# Recodificar las variables
datos_sub$FAMI_TIENEINTERNET <- ifelse(datos_sub$FAMI_TIENEINTERNET == "Si", "1",
                                       ifelse(datos_sub$FAMI_TIENEINTERNET == "No", "2", NA))

datos_sub$FAMI_ESTRATOVIVIENDA <- ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 1", "1",
                                         ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 2", "2",
                                                ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 3", "3",
                                                       ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 4", "4",
                                                              ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 5", "5",
                                                                     ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Estrato 6", "6",
                                                                            ifelse(datos_sub$FAMI_ESTRATOVIVIENDA == "Sin Estrato", "7", NA)))))))

datos_sub$ESTU_HORASSEMANATRABAJA <- ifelse(datos_sub$ESTU_HORASSEMANATRABAJA == "0", "1",
                                            ifelse(datos_sub$ESTU_HORASSEMANATRABAJA == "Menos de 10 horas", "2",
                                                   ifelse(datos_sub$ESTU_HORASSEMANATRABAJA == "Entre 11 y 20 horas", "3",
                                                          ifelse(datos_sub$ESTU_HORASSEMANATRABAJA == "Entre 21 y 30 horas", "4",
                                                                 ifelse(datos_sub$ESTU_HORASSEMANATRABAJA == "Más de 30 horas", "5", NA)))))

# Convertir a factor y luego a entero
datos_sub$FAMI_TIENEINTERNET <- as.integer(as.factor(datos_sub$FAMI_TIENEINTERNET))
datos_sub$FAMI_ESTRATOVIVIENDA <- as.integer(as.factor(datos_sub$FAMI_ESTRATOVIVIENDA))
datos_sub$ESTU_HORASSEMANATRABAJA <- as.integer(as.factor(datos_sub$ESTU_HORASSEMANATRABAJA))

# Eliminar filas con NA
datos_sub <- datos_sub[!is.na(datos_sub$FAMI_TIENEINTERNET) & 
                         !is.na(datos_sub$FAMI_ESTRATOVIVIENDA) & 
                         !is.na(datos_sub$ESTU_HORASSEMANATRABAJA) & 
                         !is.na(datos_sub$EDAD_2023), ]




N <- nrow(datos_sub) 


K <- length(unique(datos_sub$ClusterMuni))

datos_unicos <- datos_sub %>% distinct(ClusterMuni, colegio, .keep_all = TRUE)



conteo_colegios_por_cluster <- aggregate(colegio ~ ClusterMuni, data = datos_unicos, FUN = length)
colnames(conteo_colegios_por_cluster)[2] <- "NumColegios"


conteo_colegios_por_cluster 


total_colegios <- sum(conteo_colegios_por_cluster$NumColegios)


J <- total_colegios
N2 <- total_colegios



datos_sub_distinct <- datos_sub[!duplicated(datos_sub[c("ClusterMuni", "colegio")]), ]


datos_sub_distinct$Colegio <- as.integer(datos_sub_distinct$colegio)
datos_sub$Colegio <- as.integer(datos_sub$colegio)

#unique(datos_sub_distinct$Colegio)


stan_data <- list(
  N = N,
  J = J,
  K = K,
  #H = 6,
  E = 7,
  I = 2,
  N2 = N2,
  cole = datos_sub$Colegio,
  muni = datos_sub$ClusterMuni,
  #horas = datos_sub$ESTU_HORASSEMANATRABAJA,
  estrato = datos_sub$FAMI_ESTRATOVIVIENDA,
  internet = datos_sub$FAMI_TIENEINTERNET,
  y = datos_sub$PUNT_GLOBAL,
  tau = 0.25,  
  col2 = datos_sub_distinct$Colegio,
  muni2 = datos_sub_distinct$ClusterMuni,
  x = as.numeric(datos_sub$EDAD_2023)
)



#str(Colegio)
fit <- stan(data = stan_data, 
            file = "C:/Users/57320/Documents/Proyectos/Regresion Cuantilica/Regresión Cuantilica ICFES/STAN_SIMULACION.stan",
            chains = 4,
            iter= 6000,
            warmup = 1000,
            cores = 12
)




options(max.print=1050)  
print(fit)
