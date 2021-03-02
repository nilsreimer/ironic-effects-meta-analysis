data {
  int<lower = 1> I;
  int<lower = 1> K;
  int<lower = 0> ii[I];
  int<lower = 1, upper = K> kk[I];
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
  vector[K-1] b_kk_free;
  real<lower = 0> tau_ii;
}
transformed parameters {
  vector[K] b_kk = mu + append_row(0, b_kk_free);
  vector[I] theta;
  
  for (i in 1:I)
    theta[i] = b_kk[kk[i]] + b_ii[ii[i]]*tau_ii;
}
model {
  z ~ normal(theta, sigma);
  b_ii ~ normal(0, 1);
  b_kk_free ~ normal(0, 1);
  mu ~ normal(0, 0.31605);
  tau_ii ~ cauchy(0, 0.3);
}
generated quantities {
  vector<lower = -1, upper = 1>[K] r_kk;
  real R2;
  for (k in 1:K)
    r_kk[k] = ( exp(2 * b_kk[k]) - 1 ) / ( exp(2 * b_kk[k]) + 1 );
  if (K == 1) {
    R2 = 0;
  } else {
    R2 = variance(b_kk[kk]) / variance(theta);
  }
}
