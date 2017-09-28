setwd('~/model')

if(!require(runjags, lib.loc="~/model/lib")) { install.packages('runjags',lib = "~/model/lib/", repos = "http://cran.us.r-project.org") }
library(runjags, lib.loc="~/model/lib/")
library(parallel)

burnin = 5000
adapt = 2000
sample = 500
thin = 50

n.chains = detectCores()

load('data.for.jags')
load('params.to.monitor')

jags.result <- run.jags('combined-model.txt', monitor=params.to.monitor, data=data.for.jags, n.chains=n.chains,
                        burnin=burnin, sample=sample, adapt=adapt, thin = thin, method="parallel")

save(jags.result, file = paste0('generated-data/jags.result.',sample(1:10000000,1)))
