data {
  int<lower = 1> I;
  int<lower = 0> ii[I];
  vector<lower = -1, upper = 1>[I] r;
  int<lower = 1> n[I];
}
transformed data {
  vector[I] z;
  vector[I] sigma;
  for (i in 1:I) {
    z[i] = 0.5 * log( (1 + r[i]) / (1 - r[i]) );
    sigma[i] = 1 / sqrt( n[i] - 3 );
  }
}
parameters {
  real mu;
  vector[I] b_ii;
  real<lower = 0> tau_ii;
}
transformed parameters {
  real theta[I];
  for (i in 1:I) {
    theta[i] = mu + b_ii[ii[i]]*tau_ii;
  }
}
model {
  z ~ normal(theta, sigma);
  b_ii ~ normal(0, 1);
  mu ~ normal(0, 0.31605);
  tau_ii ~ cauchy(0, 0.3);
}
generated quantities {
  real<lower = -1, upper = 1> r_mean = ( exp(2 * mu) - 1 ) / ( exp(2 * mu) + 1 );
  vector<lower = -1, upper = 1>[I] r_pred;
  for (i in 1:I)
    r_pred[i] = ( exp(2 * theta[i]) - 1 ) / ( exp(2 * theta[i]) + 1 );
}
