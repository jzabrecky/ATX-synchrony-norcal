data {
  int<lower=0> N; // number of visits to each site reach
  // normalized % cover or anatoxins (bounded 0 to 100)
  array[N] real<lower=0, upper=100> y; 
  vector[N] din; // standardized DIN in water column
  vector[N] ophos; // standardized orthophosphate in water column
  vector[N] cond; // standardized conductivity in water column
}


parameters {
  real b0; // intercept
  real b1; // for autoregressive term
  real b2; // for din
  real b3; // for orthophosphate
  real b4; // for conductivity
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 2:N) {
   y[i] ~ normal(b0 + b1*y[i-1] + b2*din[i] + b3*ophos[i] +
   b4*cond[i], sigma) T[0,100];
  }

  // not including priors??
  sigma~normal(0,10)T[0,];
}
