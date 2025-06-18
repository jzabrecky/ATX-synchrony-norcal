data {
  int<lower=0> N; // number of visits to each site reach
  // normalized % cover (bounded 0 to 100)
  array[N] real<lower=0, upper=100> y; 
  vector[N] cover; // standardized cover
  vector[N] GPP; // standardized GPP
}


parameters {
  real b0; // intercept
  real b1; // for autoregressive term
  real b2; // for cover
  real b3; // for GPP
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 2:N) {
   y[i] ~ normal(b0 + b1*y[i-1] + b2*cover[i] + b3*GPP[i], sigma);
  }

  //need to think about priors at some point later...
  //will need separate models to include priors?
  sigma~normal(0,10)T[0,];
}
