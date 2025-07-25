data {
  int<lower=0> N; // number of visits to each site reach
  array[N] real<lower=0, upper=100> future; // response/percent cover we are predicting
  array[N] real<lower=0, upper=100> present; // cover at current timestep (prior to one we are predicting)
  vector[N] temp; // temperature at present
  vector[N] disc; // discharge at present
}


parameters {
  real b0; //intercept
  real b1; // for autoregressive term
  real b2; // for temperature 
  real b3; // for discharge
  real<lower=0> sigma;
}

// autoregressive model with covariates
model {
  for(i in 1:N) {
   future[i] ~ normal(b0 + b1*present[i] + b2*temp[i] + b3*disc[i],
   sigma) T[0,100];
  }

  // prior for sigma
  sigma~normal(0,10)T[0,];
  
  // prior for autoregressive term, bounded [0,1]
  b1~uniform(0,1);
}
