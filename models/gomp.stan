
data {
  int<lower=0> N;
  int y[N]; //death counts
  vector[N] pop; //population counts
  vector[N] age_c; // age - 40
}

parameters {
  real log_alpha;
  real<lower=0> beta;
}

transformed parameters {
  vector[N] mu;
  for(i in 1:N){
    mu[i] = log_alpha + beta*age_c[i];
  }
}


model {
  //likelihood
  y ~ poisson_log(mu+ log(pop));
  
  //priors
  log_alpha ~ normal(0,10);
  beta ~ normal(0,0.1);
}

