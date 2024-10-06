data {
  int<lower=1> N;             // Número total de estudiantes
  int<lower=1> J;             // Número de colegios
  int<lower=1> K;             // Número de municipios
  int<lower=1> E;             // Número de niveles para estrato
  int<lower=1> N2;            // Número de niveles para estrato
  int<lower=1> I;             // Número de niveles para tiene internet

  array[N] int<lower=1, upper=J> cole; // Índice de colegio para cada estudiante
  array[N] int<lower=1, upper=K> muni; // Índice de municipio para cada estudiante
  array[N] int<lower=1, upper=E> estrato; // Índice de estrato para cada estudiante
  array[N] int<lower=1, upper=I> internet; // Índice de internet para cada estudiante
  
  vector[N] y;                // Variable respuesta para cada estudiante
  real<lower=0, upper=1> tau; // Cuantil deseado

  array[N2] int<lower=1, upper=J> col2; // Índice auxiliar de colegio
  array[N2] int<lower=1, upper=K> muni2; // Índice auxiliar de municipio
  vector[N] x;              // Edad
}

parameters {
  vector[J] alpha;            // Efectos aleatorios por colegio
  real<lower=0> sigma_cole;  // Desviación estándar para cada colegio
  vector[K] mu;               // Media por municipio
  real mu_global;             // Media global
  real<lower=0> sigma_global; // Desviación estándar global para las medias de los municipios
  real<lower=0> sigma;
  real beta_age;              // Coeficiente para la covariable de edad
  vector[E] estrato_effect;   // Niveles de estrato vivienda
  vector[I] internet_effect;  // Niveles de tiene internet
}

transformed parameters {
  vector[E] estrato_cen;
  estrato_cen = estrato_effect - mean(estrato_effect);
  vector[I] internet_cen;
  internet_cen = internet_effect - mean(internet_effect);
}

model {
  // Priors
  mu_global ~ normal(420, 1000);
  sigma_global ~ inv_gamma(0.001, 0.001);
  mu ~ normal(mu_global, sigma_global);
  sigma ~ inv_gamma(0.001, 0.001);
  sigma_cole ~ cauchy(0,10);
  beta_age ~ normal(0, 1000); // Priori para el coeficiente de edad
  estrato_effect ~ normal(0, 1000); // Priori para los efectos de estrato
  internet_effect ~ normal(0, 1000); // Priori para tiene internet

  for (j in 1:N2) {
    alpha[col2[j]] ~ normal(mu[muni2[j]], sigma_cole);
  }

  for (i in 1:N) {
    y[i] ~ skew_double_exponential(alpha[cole[i]] + beta_age * x[i] + estrato_cen[estrato[i]] + internet_cen[internet[i]], 2*sigma, tau);
  }
}

generated quantities {
    real<lower=0> S_colegio;   // Variabilidad estandarizada de colegios
    real<lower=0> S_municipio; // Variabilidad estandarizada de municipios
    real<lower=0> S_estrato;   // Variabilidad estandarizada de estrato vivienda
    real<lower=0> S_internet;  // Variabilidad estandarizada de tiene internet

    matrix[J, J] J1;           // Matriz para colegios
    row_vector[J] b1 = rep_vector(1.0, J)'; // Vector de unos para colegios
    matrix[K, K] K1;           // Matriz para municipios
    row_vector[K] b2 = rep_vector(1.0, K)'; // Vector de unos para municipios
    matrix[E, E] E1;           // Matriz para estrato vivienda
    row_vector[E] b4 = rep_vector(1.0, E)'; // Vector de unos para estrato vivienda
    matrix[I, I] I1;           // Matriz para tiene internet
    row_vector[I] b5 = rep_vector(1.0, I)'; // Vector de unos para tiene internet
    
    for (j in 1:J) J1[j] = b1;
    for (j in 1:K) K1[j] = b2;
    for (j in 1:E) E1[j] = b4;
    for (j in 1:I) I1[j] = b5;

    S_colegio = pow(pow(J-1, -1) * alpha' * (diag_matrix(rep_vector(1.0, J)) - pow(J, -1) * J1) * alpha, 0.5);
    S_municipio = pow(pow(K-1, -1) * mu' * (diag_matrix(rep_vector(1.0, K)) - pow(K, -1) * K1) * mu, 0.5);
    S_estrato = pow(pow(E-1, -1) * estrato_effect' * (diag_matrix(rep_vector(1.0, E)) - pow(E, -1) * E1) * estrato_effect, 0.5);
    S_internet = pow(pow(I-1, -1) * internet_effect' * (diag_matrix(rep_vector(1.0, I)) - pow(I, -1) * I1) * internet_effect, 0.5);
}