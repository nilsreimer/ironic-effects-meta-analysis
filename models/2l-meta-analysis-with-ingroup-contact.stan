data {
  int<lower = 1> I;
  int<lower = 1> J;
  int<lower = 0> ii[I];
  int<lower = 1, upper = J> jj[I];
  vector<lower = -1, upper = 1>[I] r_ig;
  vector<lower = -1, upper = 1>[I] r_og;
  vector<lower = -1, upper = 1>[I] r_ig_og;
  int<lower = 1> n[I];
}
transformed data {
  vector[I] z_ig;
  vector[I] z_og;
  vector[I] z_ig_og;
  vector[I] sigma;
  for (i in 1:I) {
    z_ig[i] = 0.5 * log( (1 + r_ig[i]) / (1 - r_ig[i]) );
    z_og[i] = 0.5 * log( (1 + r_og[i]) / (1 - r_og[i]) );
    z_ig_og[i] = 0.5 * log( (1 + r_ig_og[i]) / (1 - r_ig_og[i]) );
    sigma[i] = 1 / sqrt( n[i] - 3 );
  }
}
parameters {
  real mu_ig;
  real mu_og;
  real mu_ig_og;
  vector[max(ii)] b_ig_ii;
  vector[max(ii)] b_og_ii;
  vector[max(ii)] b_ig_og_ii;
  vector[J] b_ig_jj;
  vector[J] b_og_jj;
  vector[J] b_ig_og_jj;
  real<lower = 0> tau_ig_ii;
  real<lower = 0> tau_og_ii;
  real<lower = 0> tau_ig_og_ii;
  real<lower = 0> tau_ig_jj;
  real<lower = 0> tau_og_jj;
  real<lower = 0> tau_ig_og_jj;
}
transformed parameters {
  vector[I] theta_ig;
  vector[I] theta_og;
  vector[I] theta_ig_og;
  for (i in 1:I) {
    if (ii[i] == 0) {
      theta_ig[i] = mu_ig + b_ig_jj[jj[i]]*tau_ig_jj;
      theta_og[i] = mu_og + b_og_jj[jj[i]]*tau_og_jj;
      theta_ig_og[i] = mu_ig_og + b_ig_og_jj[jj[i]]*tau_ig_og_jj;
    } else {
      theta_ig[i] = mu_ig + b_ig_jj[jj[i]]*tau_ig_jj + b_ig_ii[ii[i]]*tau_ig_ii;
      theta_og[i] = mu_og + b_og_jj[jj[i]]*tau_og_jj + b_og_ii[ii[i]]*tau_og_ii;
      theta_ig_og[i] = mu_ig_og + b_ig_og_jj[jj[i]]*tau_ig_og_jj + b_ig_og_ii[ii[i]]*tau_ig_og_ii;
    }
  }
}
model {
  z_ig ~ normal(theta_ig, sigma);
  z_og ~ normal(theta_og, sigma);
  z_ig_og ~ normal(theta_ig_og, sigma);
  b_ig_ii ~ normal(0, 1);
  b_og_ii ~ normal(0, 1);
  b_ig_og_ii ~ normal(0, 1);
  b_ig_jj ~ normal(0, 1);
  b_og_jj ~ normal(0, 1);
  b_ig_og_jj ~ normal(0, 1);
  mu_ig ~ normal(0, 0.31605);
  mu_og ~ normal(0, 0.31605);
  mu_ig_og ~ normal(0, 0.31605);
  tau_ig_ii ~ cauchy(0, 0.3);
  tau_og_ii ~ cauchy(0, 0.3);
  tau_ig_og_ii ~ cauchy(0, 0.3);
  tau_ig_jj ~ cauchy(0, 0.3);
  tau_og_jj ~ cauchy(0, 0.3);
  tau_ig_og_jj ~ cauchy(0, 0.3);
}
generated quantities {
  real<lower = -1, upper = 1> r_ig_mean = ( exp(2 * mu_ig) - 1 ) / ( exp(2 * mu_ig) + 1 );
  real<lower = -1, upper = 1> r_og_mean = ( exp(2 * mu_og) - 1 ) / ( exp(2 * mu_og) + 1 );
  real<lower = -1, upper = 1> r_ig_og_mean = ( exp(2 * mu_ig_og) - 1 ) / ( exp(2 * mu_ig_og) + 1 );
}
