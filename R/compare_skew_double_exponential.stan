data {
  vector[3] x;   // Valores de x
  real location; // Parámetro de ubicación (location)
  real scale;    // Parámetro de escala (scale)
  real kappa;    // Parámetro de sesgo (kappa)
}

parameters {
  // No parameters needed, this is just for testing the function
}

model {
  vector[3] log_density;
  
  // Calcular la densidad para cada valor de x
  for (i in 1:3) {
    log_density[i] = skew_double_exponential_lpdf(x[i] | location, scale, kappa);
  }
  
  // Imprimir la densidad
  print("Log-Density calculado en Stan: ", log_density);
}