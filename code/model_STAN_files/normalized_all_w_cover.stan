data {
  int<lower=0> N; // number of visits to each site reach
  // normalized % cover or anatoxins (bounded 0 to 100)
  array[N] real<lower=0, upper=100> y; 
  vector[N] cover; // standardized cover
  vector[N] dis; // standardized discharge
  vector[N] temp; // standardized temperature
  vector[N] din; // standardized DIN in water column
  vector[N] ophos; // standardized orthophosphate in water column
  vector[N] cond; // standardized conductivity in water column
  vector[N] GPP; // standardized GPP
}


parameters {
  real b0; // intercept
  real b1; // for autoregressive term
  real b2; // for cover
  real b3; // for discharge
  real b4; // for temperature
  real b5; // for DIN
  real b6; // for ophosphate
  real b7; // for conductivity
  real b8; // for GPP
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 2:N) {
   y[i] ~ normal(b0 + b1*y[i-1] + b2*cover[i] + b3*dis[i] + b4*temp[i] + b5*din[i]
   + b6*ophos[i] + b7*cond[i] + b8*GPP[i], sigma);
  }

  // not including priors???
  sigma~normal(0,10)T[0,];
}

