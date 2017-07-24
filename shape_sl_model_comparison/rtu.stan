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
  vector <lower = 0>[J] rlr_initial;
  
  
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
  real <lower = 0> rt_mu[N];
  real <lower = 0> rt_sigma[N];
  
  
  for(n in 1:N){
    
    rt_sigma[n] = sigma[jj[n]];
    
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

  rt ~ normal(rt_mu,rt_sigma);

  rtu_initial ~ normal(rtu_initial_mean,rtu_initial_var);
  rtu_delta ~ normal(rtu_delta_mean,rtu_delta_var);
  -rtu_learning_rate ~ normal(rtu_learning_rate_mean,rtu_learning_rate_var);
  
  rlr_initial ~ normal(rlr_initial_mean,rlr_initial_var);

  
  sigma ~ normal(sigma_mean,sigma_var);

  
  sigma_mean ~normal(50,sigma_var/20);
  sigma_var ~inv_chi_square(1.611473763/2);
  
  rtu_initial_mean ~ normal (750, rtu_initial_var/50);
  rtu_delta_mean ~ normal(.5,rtu_delta_var/2);
  rtu_learning_rate_mean ~ normal(3.652242, 7.524033);

  rlr_initial_mean ~ normal(.5,rlr_initial_var/1);
  
  rtu_initial_var ~ inv_chi_square(5.13908034/2);
  rtu_delta_var ~ inv_chi_square(0.88502629/2);
  rtu_learning_rate_var ~ inv_chi_square(0.8065802/2);
  
  rlr_initial_var~inv_chi_square(0.62543608/2);
}