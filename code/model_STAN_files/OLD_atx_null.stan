data {
  int<lower=0> N; // number of visits to site reach
  real L; // lower-bound for truncated normal (aka anatoxins cannot be less than 0)
  array[N] real<lower=L> y; // anatoxins (response; currently not normalized per site)
}


parameters {
  real b0;
  real b1;
  real<lower=0> sigma;
}

model {
  for(i in 2:N) {
    y[i] ~ normal((b0+b1*y[i-1]), sigma);
  }
  
  //need to think about priors at some point later...
  b0~normal(0,10);
  b1~normal(0,10);
  sigma~normal(0,10)T[0,];
}
