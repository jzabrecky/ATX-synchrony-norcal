data {
  int<lower=0> N_days; //number of days
  vector[N_days] cover; //% cover of cyano
  vector[N_days] prior_cover; // autoregressive component; prior 
  vector[N_days] rapid_riffle; // proportion of transects that are rapid or riffle
  vector[N_days] avg_depth; // average depth of quadrat in survey
}


parameters {
  real b0;
  real b1;
  real b2;
  real b3;
  real<lower=0> sigma;
}

model {
  for(i in 1:N_days) {
    cover[i] ~ normal((b0+b1*prior_cover[i]+b2*rapid_riffle[i]+b3*avg_depth[i]), sigma);
  }
  
  //Priors ex. from where this came
  b0~normal(0,10);
  b1~normal(0,10);
  b2~normal(0,10);
  b3~normal(0,10);
  sigma~normal(0,10)T[0,];
}
