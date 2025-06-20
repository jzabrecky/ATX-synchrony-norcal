data {
  int<lower=0> N; // number of visits to each site reach
  int<lower=0> c; // number of covariates
  array[N] real<lower=0, upper=100> y; // response/% cover bounded by 0 and 100
  array[N] real<lower=0, upper=100> autoreg; // autoregressive term, y at previous
  // time step (not using y-1 because we go through multiple sites in one df)
  matrix[N, c] covar; // covariate matrix with N (row length) * number of covariates
}


parameters {
  real b0; //intercept
  real b1; // for autoregressive term
  vector[c] b; // vector of betas for covariates
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 1:N) {
   y[i] ~ normal(b0 + b1*autoreg[i] + covar*b, sigma);
  }

  // prior for sigma
  sigma~normal(0,10)T[0,];
}
