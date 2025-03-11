data {
  int<lower=0> N; // number of visits to each site reach
  vector[N] y; // array of standardized response
}

parameters {
  real<lower=0> sigma;
}

// simple autoregressive model with stochasicity
model {
  for(i in 2:N) {
   y[i] ~ normal(y[i-1], sigma);
  }
  
  //need to think about priors at some point later...
  sigma~normal(0,10)T[0,];
}
