library(runjags)
library(dplyr)
library(tidyr)
library(ggplot2)
library(coda)
source('DBDA2E-utilities.R')

jags.model.test <- read.csv('generated-data/extracted-data-niw.csv')
jags.model.test <- jags.model.test %>% mutate(is.predictable=recode(predictable, "1"=0, "3"=1)) %>% select(-cond, -predictable)

data.for.jags <- list(
  rt = jags.model.test$rt,
  subject = jags.model.test$subject,
  is.predictable = jags.model.test$is.predictable,
  t = jags.model.test$t,
  N = length(jags.model.test$rt),
  S = length(unique(jags.model.test$subject))
)

# constant model params
params.to.monitor <- c('sd.rt', 'tau.rt', 'a.adapt.1', 'a.adapt.2', 'b.adapt', 'c.adapt')
# power model params
params.to.monitor <- c('sd.rt', 'tau.rt.1','tau.rt.2', 'a.adapt.1', 'a.adapt.2', 'b.adapt.1', 'c.adapt.1', 'b.adapt.2', 'c.adapt.2')
# piecewise model params
params.to.monitor <- c('sd.rt', 'tau.rt.1','tau.rt.2', 'a.adapt.0','a.adapt.1', 'b.adapt.0', 'c.adapt.0', 'b.learn', 'split', 'p','z')
# logisitic model params
params.to.monitor <- c('sd.rt', 'tau.rt.1','tau.rt.2', 'a.adapt.0', 'a.adapt.1', 'b.adapt.0', 'c.adapt.0', 'b.learn', 'c.learn', 'd.learn', 'p','z')

jags.result <- run.jags('jags-models/constant-logistic-model.txt', monitor=params.to.monitor, data=data.for.jags, n.chains=2,
                        burnin=1000, sample=100, adapt=1000)

result <- as.matrix(as.mcmc(jags.result))


posterior.check <- function(subj, samples, model, result){
  
  if(model == 'power'){
    p <- ggplot(jags.model.test %>% filter(subject==subj), aes(x=t, y=rt, colour=factor(is.predictable)))+geom_point()+theme_bw()
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
          z <- result[v, paste0('z.rt[',subj,']')]
          
          
          if(z == 0){
            a2 <- result[v, paste0('a.adapt.2[',subj,']')]
            
            b1 <- result[v, paste0('b.adapt.1[',subj,']')]
            c1 <- result[v, paste0('c.adapt.1[',subj,']')]
            return(a2 *(1 + b1*(x^-c1 - 1)))
          }
          
          if(z==1){
            a2 <- result[v, paste0('a.adapt.2[',subj,']')]
            b2 <- result[v, paste0('b.learn[',subj,']')]
            c2 <- result[v, paste0('c.learn[',subj,']')]
            b1 <- result[v, paste0('b.adapt.1[',subj,']')]
            c1 <- result[v, paste0('c.adapt.1[',subj,']')]
            
            return((a2 *(1+b2*(x^-c2 - 1))*(1 + b1*(x^-c1 - 1))))
          }
        }, colour='blue', alpha=0.3)
      )
    }
  }
  
  if(model == 'logistic'){
    p <- ggplot(jags.model.test %>% filter(subject==subj), aes(x=t, y=rt, colour=factor(is.predictable)))+geom_point()+theme_bw()
    overlay.list <- c()
    for(i in 1:samples){
      overlay.list <- c(
        overlay.list, 
        stat_function(fun = function(x){
          v <- sample(1:nrow(result), 1)
          a <- result[v, paste0('a.adapt.0[',subj,']')]
          b <- result[v, paste0('b.adapt.0[',subj,']')]
          c <- result[v, paste0('c.adapt.0[',subj,']')]
          return(a * (1 + b*(x^-c - 1)))
        }, colour='red', alpha = 0.3),
        stat_function(fun = function(x){
          v <- sample(1:nrow(result), 1)
          z <- result[v, paste0('z[',subj,']')]
          if(z == 0){
            a2 <- result[v, paste0('a.adapt.1[',subj,']')]
            b1 <- result[v, paste0('b.adapt.0[',subj,']')]
            c1 <- result[v, paste0('c.adapt.0[',subj,']')]
            return(a2 *(1 + b1*(x^-c1 - 1)))
          }
          if(z == 1){
            a2 <- result[v, paste0('a.adapt.1[',subj,']')]
            b2 <- result[v, paste0('b.learn[',subj,']')]
            c2 <- result[v, paste0('c.learn[',subj,']')]
            d2 <- result[v, paste0('d.learn[',subj,']')]
            b1 <- result[v, paste0('b.adapt.0[',subj,']')]
            c1 <- result[v, paste0('c.adapt.0[',subj,']')]
            base <- a2 * (1 + b1 * (x ^ -c1 - 1))
            rlr <- 1 + (-b2) / (1+exp(-c2 * (x- d2)))
            return( base * rlr )
          }
          
        }, colour='blue', alpha=0.3)
      )
    }
  }
  
  if(model == 'constant.piecewise'){
    p <- ggplot(jags.model.test %>% filter(subject==subj), aes(x=t, y=rt, colour=factor(is.predictable)))+geom_point()+theme_bw()
    overlay.list <- c()
    for(i in 1:samples){
      overlay.list <- c(
        overlay.list, 
        stat_function(fun = function(x){
          v <- sample(1:nrow(result), 1)
          a <- result[v, paste0('a.adapt.0[',subj,']')]
          b <- result[v, paste0('b.adapt.0[',subj,']')]
          c <- result[v, paste0('c.adapt.0[',subj,']')]
          return(a * (1 + b*(x^-c - 1)))
        }, colour='red', alpha = 0.3),
        stat_function(fun = function(x){
          v <- sample(1:nrow(result), 1)
          z <- result[v, paste0('z[',subj,']')] 
          if(z == 0){
            a2 <- result[v, paste0('a.adapt.1[',subj,']')]
            b1 <- result[v, paste0('b.adapt.0[',subj,']')]
            c1 <- result[v, paste0('c.adapt.0[',subj,']')]
            return(a2 *(1 + b1*(x^-c1 - 1)))
          }
          
          if(z == 1){
            a2 <- result[v, paste0('a.adapt.1[',subj,']')]
            b2 <- result[v, paste0('b.learn[',subj,']')]
            s <-  result[v, paste0('split[',subj,']')]
            b1 <- result[v, paste0('b.adapt.0[',subj,']')]
            c1 <- result[v, paste0('c.adapt.0[',subj,']')]
            
            return(sapply(x, function(x){
              if(x <= s){
                return(a2 *(1 + b1*(x^-c1 - 1)))
              } else {
                return(a2*b2 *(1 + b1*(x^-c1 - 1)))
              }
            }))
          }
        }, colour='blue', alpha=0.3)
      )
    }
  }
  p + overlay.list
}

posterior.check(201, 10, model='logistic', result)
hist(result.1[,'z[201]'])

