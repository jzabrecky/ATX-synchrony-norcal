data {
  int<lower=0> N; // number of visits to each site reach
  // normalized % cover (bounded 0 to 100)
  array[N] real<lower=0, upper=100> y; 
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
  real b2; // for discharge
  real b3; // for temperature
  real b4; // for nitrate
  real b5; // for ammonium
  real b6; // for orthophosphate
  real b7; // for conductivity
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 2:N) {
   y[i] ~ normal(b0 + b1*y[i-1] + b2*dis[i] + b3*temp[i] +
   b4*nit[i] + b5*amm[i] + b6*ophos[i] + b7*cond[i], sigma);
  }

  //need to think about priors at some point later...
  //will need separate models to include priors?
  sigma~normal(0,10)T[0,];
}
