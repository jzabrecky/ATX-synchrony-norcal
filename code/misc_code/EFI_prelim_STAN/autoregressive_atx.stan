data {
  int<lower=0> N; //number of visits to site reach
  vector[N] atx; //anatoxins
  vector[N] cover; //% cover of cyano
}


parameters {
  real b0;
  real b1;
  real b2;
  real<lower=0> sigma;
}

model {
  for(i in 2:N) {
    atx[i] ~ normal((b0+b1*atx[i-1]+b2*cover[i]), sigma);
  }
  
  //Priors ex. from where this came
  b0~normal(0,10);
  b1~normal(0,10);
  b2~normal(0,10);
  sigma~normal(0,10)T[0,];
}
