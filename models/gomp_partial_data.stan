
data {
  int<lower=0> N; //number of observations
  int<lower=0> M; //number of observed areas
  int<lower=0> K; //total number of areas
  int y[N, M]; //death counts
  real<lower=0> z; //summary mortality measure
  matrix[N,M] pop; //population counts
  vector[N] age_c; // age - 40
}

parameters {
  real log_alpha[K];
  real<lower=0> beta[K];
}

transformed parameters {
  matrix[N, K] mu;
  matrix[N, K] px;
  real<lower=0> p40[K];
  real<lower=0> q40[K];
  
  for(i in 1:N){
    for(j in 1:K){
    mu[i,j] = log_alpha[j] + beta[j]*age_c[i];
    px[i,j] = exp(-1*exp(mu[i,j]));
    }
  }
  
  for(j in 1:K){
    p40[j] = prod(px[,j]);
    q40[j] = 1-p40[j];
  }
}


model {
  //likelihood
  for(i in 1:M){
      y[,i] ~ poisson_log(mu[,i]+ log(pop[,i]));
  }
  // partial info for one area
  z ~ normal(q40[K], 0.001);

  
  //priors
  log_alpha ~ normal(-7,1);
  beta ~ normal(0.1,0.1);
}

