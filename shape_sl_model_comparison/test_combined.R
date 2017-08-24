library(runjags)
library(dplyr)
library(tidyr)
library(ggplot2)
library(coda)
source('DBDA2E-utilities.R')
source('Data.R')

jags.model <- read.csv('generated-data/extracted-data-niw.csv')

jags.model
length(rt)

S<-length(unique(jags.model$subject))
rt=jags.model$rt
t=jags.model$rt
N<-length(rt)
jags.model$is.predictable= mapply(function(x){if(x==1){return(0)};if(x == 3){return(1)}}, jags.model$predictable)


data.for.jags <- list(
  rt = jags.model$rt,
  subject = jags.model$subject,
  is.predictable = jags.model$is.predictable,
  t = jags.model$t,
  N = length(rt),
  S = length(unique(jags.model$subject))
)

length(data.for.jags)
  
  params.to.monitor <- c('sd.rt',
                                            'tau.rt.1','tau.rt.2', 
                                            'a.adapt.0','a.adapt.1',
                                            'b.adapt.0', 'c.adapt.0', 
                                            'b.logistic','c.logistic','d.logistic',
                                            'b.power','c.power',
                                            'b.constant.piecewise', 'split', 
                                            'p', 'learn','z', 'prob','alpha')
  
  jags.result <- run.jags('jags-models/combined-model.txt', monitor=params.to.monitor, data=data.for.jags, n.chains=2,
                          burnin=100, sample=500, adapt=100)
