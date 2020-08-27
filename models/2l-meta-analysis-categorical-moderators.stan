data {
  int<lower = 1> I;
  int<lower = 1> J;
  int<lower = 1> K;
  int<lower = 0> ii[I];
  int<lower = 1, upper = J> jj[I];
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
  vector[max(ii)] b_ii;
  vector[J] b_jj;
  vector[K-1] b_kk_free;
  real<lower = 0> tau_ii;
  real<lower = 0> tau_jj;
}
transformed parameters {
  vector[K] b_kk = mu + append_row(0, b_kk_free);
  real theta[I];
  
  for (i in 1:I) {
    if (ii[i] == 0) {
      theta[i] = b_kk[kk[i]] + b_jj[jj[i]]*tau_jj;
    } else {
      theta[i] = b_kk[kk[i]] + b_jj[jj[i]]*tau_jj + b_ii[ii[i]]*tau_ii;
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
  vector<lower = -1, upper = 1>[K] r_kk;
  for (k in 1:K)
    r_kk[k] = ( exp(2 * b_kk[k]) - 1 ) / ( exp(2 * b_kk[k]) + 1 );
}
