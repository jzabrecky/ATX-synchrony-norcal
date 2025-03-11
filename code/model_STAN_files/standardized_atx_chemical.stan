data {
  int<lower=0> N; // number of visits to each site reach
  vector[N] y; // array of standardized response
  vector[N] cover; // standardized benthic cyanobacteria (taxa-specific) cover
  vector[N] dis; // standardized discharge
  vector[N] temp; // standardized temperature
  vector[N] nit; // standardized nitrate in water column
  vector[N] amm; // standardized ammonium in water column
  vector[N] ophos; // standardized orthophosphate in water column
  vector[N] cond; // standardized conductivity in water column
}


parameters {
  real b0; // intercept
  real b1; // for autoregressive term
  real b2; // for cover
  real b3; // for discharge
  real b4; // for temperature
  real b5; // for nitrate
  real b6; // for ammonium
  real b7; // for orthophosphate
  real b8; // for conductivity
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 2:N) {
   y[i] ~ normal(b0 + b1*y[i-1] + b2*cover[i] + b3*dis[i] + b4*temp[i] +
   b5*nit[i] + b6*amm[i] + b7*ophos[i] + b8*cond[i], sigma);
  }

  //need to think about priors at some point later...
  //will need separate models to include priors?
  sigma~normal(0,10)T[0,];
}
