data {
  int<lower=0> N; // number of visits to each site reach
  // normalized % cover or anatoxins (bounded 0 to 100)
  array[N] real<lower=0, upper=100> y; 
  vector[N] autoreg; // autoregressive term
  vector[N] dis; // standardized discharge
  vector[N] temp; // standardized temperature
  vector[N] din; // standardized DIN in water column
  vector[N] ophos; // standardized orthophosphate in water column
  vector[N] cond; // standardized conductivity in water column
}


parameters {
  real b0; // intercept
  real b1; // for autoregressive term
  real b2; // for discharge
  real b3; // for temperature
  real b4; // for DIN
  real b5; // for ophosphate
  real b6; // for conductivity
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 2:N) {
   y[i] ~ normal(b0 + b1*autoreg[i] + b2*dis[i] + b3*temp[i] + b4*din[i]
   + b5*ophos[i] + b6*cond[i], sigma);
  }

  // not including priors???
  sigma~normal(0,10)T[0,];
}

