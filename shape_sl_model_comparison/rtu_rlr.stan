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
  
  vector <lower = 0>[J] sigma;
  
  real<lower = 0> sigma_mean;
  real<lower = 0> sigma_var;
  
  
  vector <lower = 0>[J] rtu_initial;
  vector <lower = 0, upper =1>[J] rtu_delta;
  vector <lower = 0>[J] rtu_learning_rate;
  
  vector<lower = 0>[J] rlr_initial;
  
  
  
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
  real <lower = 0> rt_sigma[N];

  

  for(n in 1:N){
    
    rt_sigma[n] = sigma[jj[n]];
    
  if(pp[n] == 1){
    rlr[n] = rlr_initial[jj[n]];
  }
  if(pp[n] == 0){
    rlr[n] = 1;
  }
  
  rt_mu[n]= rlr[n]*(rtu_initial[jj[n]] * (rtu_delta[jj[n]] * (ii[n]^(rtu_learning_rate_transform[jj[n]]) - 1)));
  
  }
}
model{
  rt ~ normal(rt_mu,rt_sigma);

  rtu_initial ~ normal(rtu_initial_mean,rtu_initial_var);
  rtu_delta ~ normal(rtu_delta_mean,rtu_delta_var);
  -rtu_learning_rate ~ normal(rtu_learning_rate_mean,rtu_learning_rate_var);
    
  rlr_initial ~ normal(rlr_initial_mean,rlr_initial_var);
  
    
  sigma ~ normal(sigma_mean,sigma_var);
  
  
  sigma_mean ~gamma(25.0492317,0.9648083);
  sigma_var ~gamma(5.0012967, 0.0414321);
  
  rtu_initial_mean ~ normal (800, 50);
  rtu_delta_mean ~ beta(1,1);
  rtu_learning_rate_mean ~ gamma(3.652242, 7.524033);

  rlr_initial_mean ~ gamma(82.09057,81.28569);
  
  rtu_initial_var ~ gamma(7.56317917, 0.06265543);
  rtu_delta_var ~ gamma(2.450601, 7.572772);
  rtu_learning_rate_var ~ gamma( 2.450601, 7.572772);
  
  rlr_initial_var~ gamma(1.04394073,0.09930478);
  
}
