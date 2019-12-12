data {
  int<lower = 1> I;
  int<lower = 1> J;
  int<lower = 0> K;
  int<lower = 0> ii[I];
  int<lower = 1, upper = J> jj[I];
  vector<lower = -1, upper = 1>[I] r;
  int<lower = 1> n[I];
  row_vector[K] X[I];
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
  vector[max(ii)] b_ii;
  vector[J] b_jj;
  real<lower = 0> tau_ii;
  real<lower = 0> tau_jj;
  vector[K] b_kk;
}
transformed parameters {
  real theta[I];
  for (i in 1:I) {
    if (ii[i] == 0) {
      theta[i] = mu + X[i] * b_kk + b_jj[jj[i]]*tau_jj;
    } else {
      theta[i] = mu + X[i] * b_kk + b_jj[jj[i]]*tau_jj + b_ii[ii[i]]*tau_ii;
    }
  }
}
model {
  z ~ normal(theta, sigma);
  b_ii ~ normal(0, 1);
  b_jj ~ normal(0, 1);
  b_kk ~ normal(0, 1);
  mu ~ normal(0, 0.31605);
  tau_ii ~ cauchy(0, 0.3);
  tau_jj ~ cauchy(0, 0.3);
}
generated quantities {
  real<lower = -1, upper = 1> r_mean = ( exp(2 * mu) - 1 ) / ( exp(2 * mu) + 1 );
  real<lower = 0> b_kk_sd = sd(append_row(b_kk, 0));
  vector<lower = -1, upper = 1>[I] r_pred;
  for (i in 1:I)
    r_pred[i] = ( exp(2 * theta[i]) - 1 ) / ( exp(2 * theta[i]) + 1 );
}
