data {
  int<lower=0> N; // number of visits to each site reach
  int<lower=0> c; // number of covariates
  array[N] real<lower=0, upper=100> future; // response/anatoxins we are predicting
  matrix[N, c] covar; // covariate matrix with N (row length) * number of covariates
  // covariates are all at current time step (time step prior to future we are predicting)
}


parameters {
  real b0; //intercept
  vector[c] b; // vector of betas for covariates
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 1:N) {
   future[i] ~ normal(b0 + covar[i,]*b, sigma) T[0,100];
  }

  // prior for sigma
  sigma~normal(0,10)T[0,];
}
