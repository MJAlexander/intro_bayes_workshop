
data {
  int<lower=0> N; //number of age groups
  int<lower=0> T; //time periods
  int y[N, T]; //death counts
  matrix[N,T] pop; //population counts
  vector[N] age_c; // age - 40
}

parameters {
  real<lower = -9, upper=-5> log_alpha[T];
  real<lower=0.08, upper=0.14> beta[T];
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;
}

transformed parameters {
  matrix[N,T] mu;
  for(i in 1:N){
    for(t in 1:T){
      mu[i,t] = log_alpha[t] + beta[t]*age_c[i];
    }
    
  }
}


model {
  //likelihood
  for(t in 1:T){
   y[,t] ~ poisson_log(mu[,t]+ log(pop[,t])); 
  }
  
  //priors
  log_alpha[1] ~ normal(-6,1);
  beta[1] ~ normal(0.1,0.1);
  log_alpha[2:T] ~ normal(log_alpha[1:(T-1)],sigma_alpha);
  beta[2:T] ~ normal(beta[1:(T-1)],sigma_beta);
  sigma_alpha ~ normal(0,1);
  sigma_beta ~ normal(0,1);
}

