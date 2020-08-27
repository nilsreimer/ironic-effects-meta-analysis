data {
  int<lower = 1> I;
  int<lower = 1> J;
  int<lower = 0> ii[I];
  int<lower = 1, upper = J> jj[I];
  vector<lower = -1, upper = 1>[I] r;
  int<lower = 1> n[I];
  vector<lower = 0>[I] x;
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
  real b_x;
  vector[max(ii)] b_ii;
  vector[J] b_jj;
  real<lower = 0> tau_ii;
  real<lower = 0> tau_jj;
}
transformed parameters {
  real theta[I];
  
  for (i in 1:I) {
    if (ii[i] == 0) {
      theta[i] = mu + b_x*x[i] + b_jj[jj[i]]*tau_jj;
    } else {
      theta[i] = mu + b_x*x[i] + b_jj[jj[i]]*tau_jj + b_ii[ii[i]]*tau_ii;
    }
  }
}
model {
  z ~ normal(theta, sigma);
  b_ii ~ normal(0, 1);
  b_jj ~ normal(0, 1);
  b_x ~ normal(0, 1);
  mu ~ normal(0, 0.31605);
  tau_ii ~ cauchy(0, 0.3);
  tau_jj ~ cauchy(0, 0.3);
}
