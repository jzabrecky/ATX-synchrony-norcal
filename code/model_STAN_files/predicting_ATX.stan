data {
  int<lower=0> N; // number of visits to each site reach
  int<lower=0> c; // number of covariates
  array[N] real<lower=0, upper=100> y; // response or % cover bounded by 0 and 100
  matrix[N, c] covar; // covariate matrix with N (row length) * number of covariates
}


parameters {
  real b0; //intercept
  vector[c] b; // vector of betas for covariates
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 1:N) {
   y[i] ~ normal(b0 + b1*autoreg[i] + covar*b, sigma) T[0,100];
  }

  // prior for sigma
  sigma~normal(0,10)T[0,];
}
