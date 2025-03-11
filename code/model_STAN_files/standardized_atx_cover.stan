data {
  int<lower=0> N; // number of visits to each site reach
  vector[N] y; // array of standardized response
  vector[N] cover; // standardized benthic cyanobacteria (taxa-specific) cover
}

parameters {
  real b0; // intercept
  real b1; // for autoregressive term
  real b2; // for cover
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 2:N) {
   y[i] ~ normal(b0 + b1*y[i-1] + b2*cover, sigma);
  }

  //need to think about priors at some point later...
  //will need separate models to include priors?
  sigma~normal(0,10)T[0,];
}
