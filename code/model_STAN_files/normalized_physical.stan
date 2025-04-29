data {
  int<lower=0> N; // number of visits to each site reach
  // normalized % cover or anatoxins (bounded 0 to 100)
  array[N] real<lower=0, upper=100> y; 
  vector[N] dis; // standardized discharge
  vector[N] temp; // standardized temperature
}


parameters {
  real b0; // intercept
  real b1; // for autoregressive term
  real b2; // for discharge
  real b3; // for temperature
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 2:N) {
   y[i] ~ normal(b0 + b1*y[i-1] + b2*dis[i] + b3*temp[i], sigma);
  }

  // not including priors???
  sigma~normal(0,10)T[0,];
}

