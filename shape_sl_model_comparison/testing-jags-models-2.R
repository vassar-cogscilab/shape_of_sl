library(runjags)
library(dplyr)
library(tidyr)
library(ggplot2)
library(coda)


source('DBDA2E-utilities.R')

jags.constant.model.test <- read.csv('generated-data/powerRLR-constantRLR-generated-data.csv')
jags.constant.model.test$X <- NULL
jags.constant.model.test$rt <- round(jags.constant.model.test$rt)

data.for.jags <- list(
  rt = jags.constant.model.test$rt,
  subject = jags.constant.model.test$subject,
  is.predictable = jags.constant.model.test$is.predictable,
  t = jags.constant.model.test$t,
  N = length(jags.constant.model.test$rt),
  S = length(unique(jags.constant.model.test$subject))
)

params.to.monitor <- c('sd.rt', 'tau.rt', 'a.adapt.1', 'a.adapt.2', 'b.adapt', 'c.adapt')
params.to.monitor <- c('sd.rt', 'tau.rt.1','tau.rt.2', 'a.adapt.1', 'a.adapt.2', 'b.adapt.1', 'c.adapt.1', 'b.adapt.2', 'c.adapt.2')
params.to.monitor <- c('sd.rt', 'tau.rt.1','tau.rt.2', 'a.adapt.1', 'a.adapt.2', 'a.adapt.3', 'b.adapt.1', 'c.adapt.1', 'b.adapt.2', 'c.adapt.2', 'p','z')

jags.result <- run.jags('jags-models/constant-power-model.txt', monitor=params.to.monitor, data=data.for.jags, n.chains=2,
         burnin=1000, sample=1000, adapt=1000)

result <- as.matrix(as.mcmc(jags.result))

posterior.check <- function(subj, samples){
  p <- ggplot(jags.constant.model.test %>% filter(subject==subj), aes(x=t, y=rt, colour=factor(is.predictable)))+geom_point()+theme_bw()
  overlay.list <- c()
  for(i in 1:samples){
    overlay.list <- c(
      overlay.list, 
      stat_function(fun = function(x){
        v <- sample(1:nrow(result), 1)
        a <- result[v, paste0('a.adapt.1[',subj,']')]
        b <- result[v, paste0('b.adapt.1[',subj,']')]
        c <- result[v, paste0('c.adapt.1[',subj,']')]
        return(a * (1 + b*(x^-c - 1)))
      }, colour='red', alpha = 0.3),
      stat_function(fun = function(x){
        v <- sample(1:nrow(result), 1)
        z<-result[v, paste0('z[',subj,']')]
        
        if(z == 0){
          a3 <- result[v, paste0('a.adapt.3[',subj,']')]
          
          b1 <- result[v, paste0('b.adapt.1[',subj,']')]
          c1 <- result[v, paste0('c.adapt.1[',subj,']')]
          return(a3 *(1 + b1*(x^-c1 - 1)))
        }
        
        if(z==1){
        
        a2 <- result[v, paste0('a.adapt.2[',subj,']')]
        b2 <- result[v, paste0('b.adapt.2[',subj,']')]
        c2 <- result[v, paste0('c.adapt.2[',subj,']')]
        b1 <- result[v, paste0('b.adapt.1[',subj,']')]
        c1 <- result[v, paste0('c.adapt.1[',subj,']')]
        return(a2 * (1 + b2*(x^-c2 - 1))*(1 + b1*(x^-c1 - 1)))
        }
      }, colour='blue', alpha=0.3)
    )
  }
  print(overlay.list)
  p + overlay.list
}

posterior.check(7, 4)
