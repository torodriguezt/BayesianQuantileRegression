data {
  int<lower=0> N;      // número de observaciones
  real mu;             // parámetro de ubicación
  real sigma;          // parámetro de escala
  real tau;            // parámetro de asimetría
  real y[N];           // datos observados
}

parameters {
  real theta;  // parámetro a ajustar
}

model {
  // Uso de la distribución skew_double_exponential en Stan
  y ~ skew_double_exponential(mu, sigma, tau);
}

generated quantities {
  real y_rep[N];
  for (n in 1:N) {
    y_rep[n] = skew_double_exponential_rng(mu, sigma, tau);
  }
}