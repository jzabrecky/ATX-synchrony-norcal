data {
  int<lower=0> N; //number of visits to site reach
  vector[N] cover; //% cover of cyano
}


parameters {
  real b0;
  real b1;
  real<lower=0> sigma;
}

model {
  for(i in 2:N) {
    mu = b0 + b1 * log(cover[i-1]); // log, then exp to keep positive value
    cover[i] ~ pois(exp(mu));
  }
  
  //need to think about priors at some point later...
  b0~normal(0,10);
  b1~normal(0,10);
  sigma~normal(0,10)T[0,];
}
