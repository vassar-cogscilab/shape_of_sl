install.packages('runjags')
install.packages('parallel')
library(runjags)
library(parallel)


burnin = 500
adapt = 500
sample = 1000
thin = 1
  
n.chains = detectCores()

load('combined_model/data.for.jags')
load('combined_model/params.to.monitor')

jags.result <- run.jags('combined_model/combined-model.txt', monitor=params.to.monitor, data=data.for.jags, n.chains=n.chains,
                        burnin=burnin, sample=sample, adapt=adapt, thin = thin)

save(jags.result, file = 'combined_model/jags.result')
