data {
  int<lower=0> N_days; //number of days
  vector[N_days] cover; //% cover of cyano
  vector[N_days] atx; //anatoxin concentraitons
  vector[N_days] ophos; //orthophosphate concentrations
  vector[N_days] amm; //ammonium concentrations
  vector[N_days] nitrate; // nitrate concentrations
}


parameters {
  real b0;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  real<lower=0> sigma;
}

model {
  for(i in 2:N_days) {
    atx[i] ~ normal((b0+b1*atx[i-1]+b2*cover[i]+b3*ophos[i]+b4*amm[i]+b5*nitrate[i]),
    sigma);
  }
  
  //Priors ex. from where this came
  b0~normal(0,10);
  b1~normal(0,10);
  b2~normal(0,10);
  b3~normal(0,10);
  b4~normal(0,10);
  b5~normal(0,10);
  sigma~normal(0,10)T[0,];
}
