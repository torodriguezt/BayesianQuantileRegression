data {
  int<lower=1> k;
  int<lower=0> n;
  matrix[n, k] X;
  array[n] int y;  // Utiliza la nueva sintaxis para arrays
} 

parameters {
  vector[k] beta;
  real alpha;
} 

model {
  target += bernoulli_logit_glm_lpmf(y | X, alpha, beta);
}