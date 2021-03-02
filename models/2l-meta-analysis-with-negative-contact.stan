data {
  int<lower = 1> I;
  int<lower = 1> J;
  int<lower = 0> ii[I];
  int<lower = 1, upper = J> jj[I];
  vector<lower = -1, upper = 1>[I] r_pc;
  vector<lower = -1, upper = 1>[I] r_nc;
  vector<lower = -1, upper = 1>[I] r_pc_nc;
  int<lower = 1> n[I];
}
transformed data {
  vector[I] z_pc;
  vector[I] z_nc;
  vector[I] z_pc_nc;
  vector[I] sigma;
  for (i in 1:I) {
    z_pc[i] = 0.5 * log( (1 + r_pc[i]) / (1 - r_pc[i]) );
    z_nc[i] = 0.5 * log( (1 + r_nc[i]) / (1 - r_nc[i]) );
    z_pc_nc[i] = 0.5 * log( (1 + r_pc_nc[i]) / (1 - r_pc_nc[i]) );
    sigma[i] = 1 / sqrt( n[i] - 3 );
  }
}
parameters {
  real mu_pc;
  real mu_nc;
  real mu_pc_nc;
  vector[max(ii)] b_pc_ii;
  vector[max(ii)] b_nc_ii;
  vector[max(ii)] b_pc_nc_ii;
  vector[J] b_pc_jj;
  vector[J] b_nc_jj;
  vector[J] b_pc_nc_jj;
  real<lower = 0> tau_pc_ii;
  real<lower = 0> tau_nc_ii;
  real<lower = 0> tau_pc_nc_ii;
  real<lower = 0> tau_pc_jj;
  real<lower = 0> tau_nc_jj;
  real<lower = 0> tau_pc_nc_jj;
}
transformed parameters {
  vector[I] theta_pc;
  vector[I] theta_nc;
  vector[I] theta_pc_nc;
  for (i in 1:I) {
    if (ii[i] == 0) {
      theta_pc[i] = mu_pc + b_pc_jj[jj[i]]*tau_pc_jj;
      theta_nc[i] = mu_nc + b_nc_jj[jj[i]]*tau_nc_jj;
      theta_pc_nc[i] = mu_pc_nc + b_pc_nc_jj[jj[i]]*tau_pc_nc_jj;
    } else {
      theta_pc[i] = mu_pc + b_pc_jj[jj[i]]*tau_pc_jj + b_pc_ii[ii[i]]*tau_pc_ii;
      theta_nc[i] = mu_nc + b_nc_jj[jj[i]]*tau_nc_jj + b_nc_ii[ii[i]]*tau_nc_ii;
      theta_pc_nc[i] = mu_pc_nc + b_pc_nc_jj[jj[i]]*tau_pc_nc_jj + b_pc_nc_ii[ii[i]]*tau_pc_nc_ii;
    }
  }
}
model {
  z_pc ~ normal(theta_pc, sigma);
  z_nc ~ normal(theta_nc, sigma);
  z_pc_nc ~ normal(theta_pc_nc, sigma);
  b_pc_ii ~ normal(0, 1);
  b_nc_ii ~ normal(0, 1);
  b_pc_nc_ii ~ normal(0, 1);
  b_pc_jj ~ normal(0, 1);
  b_nc_jj ~ normal(0, 1);
  b_pc_nc_jj ~ normal(0, 1);
  mu_pc ~ normal(0, 0.31605);
  mu_nc ~ normal(0, 0.31605);
  mu_pc_nc ~ normal(0, 0.31605);
  tau_pc_ii ~ cauchy(0, 0.3);
  tau_nc_ii ~ cauchy(0, 0.3);
  tau_pc_nc_ii ~ cauchy(0, 0.3);
  tau_pc_jj ~ cauchy(0, 0.3);
  tau_nc_jj ~ cauchy(0, 0.3);
  tau_pc_nc_jj ~ cauchy(0, 0.3);
}
generated quantities {
  real<lower = -1, upper = 1> r_pc_mean = ( exp(2 * mu_pc) - 1 ) / ( exp(2 * mu_pc) + 1 );
  real<lower = -1, upper = 1> r_nc_mean = ( exp(2 * mu_nc) - 1 ) / ( exp(2 * mu_nc) + 1 );
  real<lower = -1, upper = 1> r_pc_nc_mean = ( exp(2 * mu_pc_nc) - 1 ) / ( exp(2 * mu_pc_nc) + 1 );
}
