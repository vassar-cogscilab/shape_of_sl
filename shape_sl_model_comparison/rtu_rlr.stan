data{
  int <lower = 0> J; //number of subjects
  int <lower = 0> I; //number of item presentations
  int <lower = 0> N; //total number of data points
  int <lower = 0> P; //predictable?
  
  int <lower = 0, upper = J> jj[N]; //subject index
  int <lower = 0, upper = I> ii[N]; //item index
  int <lower = 0, upper = P> pp[N]; //predictable index
  
  real <lower = 0, upper = 2000> rt[N]; //response time
  
}
parameters{
  
  real <lower = 0> sigma[J];
  
  real<lower = 0> sigma_mean;
  real<lower = 0> sigma_var;
  
  
  real <lower = 0> rtu_initial[J];
  real <lower = 0, upper =1> rtu_delta[J];
  real <lower = 0> rtu_learning_rate[J];
  
  real <lower = 0> rlr_initial[J];
  
  
  
  real <lower = 0> rtu_initial_mean;
  real <lower = 0, upper =1> rtu_delta_mean;
  real <lower = 0> rtu_learning_rate_mean;
  
  real <lower = 0> rlr_initial_mean;
  
  real <lower = 0> rtu_initial_var;
  real <lower = 0, upper =1> rtu_delta_var;
  real <lower = 0> rtu_learning_rate_var;
  
  real <lower = 0> rlr_initial_var;
  
}
transformed parameters{
  real <lower = 0> rlr[N];
  real <lower =0> rt_mu[N];
  
  for(n in 1:N){
  if(pp[n] == 1){
    rlr[n] = rlr_initial[jj[n]];
  }
  if(pp[n] == 0){
    rlr[n] = 1;
  }
  
  rt_mu[n]= rlr[n]*(rtu_initial[jj[n]] * (rtu_delta[jj[n]] * (ii[n]^(rtu_learning_rate[jj[n]]) - 1)));
  
  }
}
model{
  for(n in 1:N){
    rt[n] ~ normal(rt_mu[n],sigma[jj[n]]);
  }
  
  for(j in 1:J){
  rtu_initial[j] ~ normal(rtu_initial_mean,rtu_initial_var);
  rtu_delta[j] ~ normal(rtu_delta_mean,rtu_delta_var);
  (-rtu_learning_rate[j]) ~ normal(rtu_learning_rate_mean,rtu_learning_rate_var);
  
  rlr_initial[j] ~ normal(rlr_initial_mean,rlr_initial_var);

  
  sigma[j] ~ normal(sigma_mean,sigma_var);
  }
  
  sigma_mean ~gamma(25.0492317,0.9648083);
  sigma_var ~gamma(1.611473763, 0.009959456);
  
  rtu_initial_mean ~ normal (750, 50);
  rtu_delta_mean ~ beta(1,1);
  rtu_learning_rate_mean ~ gamma(3.652242, 7.524033);

  rlr_initial_mean ~ gamma(82.09057,81.28569);
  
  rtu_initial_var ~ gamma(5.13908034, 0.04257354);
  rtu_delta_var ~ gamma(0.88502629, 0.08009378);
  rtu_learning_rate_var ~ gamma( 0.8065802, 0.0729945);
  
  rlr_initial_var~ gamma( 0.62543608,0.05949456);
}
