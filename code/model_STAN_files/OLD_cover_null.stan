data {
  int<lower=0> N; // number of visits to site reach
  array[N] int<lower=0> y; //relative % cover of cyano (response)
}


parameters {
  real b0;
  real b1;
  real<lower=0> sigma;
  real<lower=0> mu;
}

model {
  for(i in 2:N) {
    // exp to prevent negatives; note this is cover predicting log-cover
    y[i] ~ poisson(exp(b0+b1*(log(y[i-1]))));
  }
  
  //need to think about priors at some point later...
  b0~normal(0,10);
  b1~normal(0,10);
  sigma~normal(0,10)T[0,];
}
