library(runjags)
library(dplyr)
library(tidyr)

jags.constant.model.test <- read.csv('generated-data/jags-constant-model-test.csv')
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
jags.result <- run.jags('jags-models/constant-model.txt', monitor=params.to.monitor, data=data.for.jags, n.chains=2,
         burnin=1000, sample=1000, adapt=1000)

result <- as.matrix(as.mcmc(jags.result))


posterior.check <- function(subj, samples){
  p <- ggplot(jags.constant.model.test %>% filter(subject==subj), aes(x=t, y=rt, colour=factor(is.predictable)))+geom_point()+theme_bw()
  overlay.list <- c()
  for(i in 1:samples){
    overlay.list <- c(
      overlay.list, 
      stat_function(fun = function(x){
        z <- sample(1:nrow(result), 1)
        a <- result[z, paste0('a.adapt.1[',subj,']')]
        b <- result[z, paste0('b.adapt[',subj,']')]
        c <- result[z, paste0('c.adapt[',subj,']')]
        return(a * (1 + b*(x^-c - 1)))
      }, colour='red', alpha = 0.3),
      stat_function(fun = function(x){
        z <- sample(1:nrow(result), 1)
        a <- result[z, paste0('a.adapt.2[',subj,']')]
        b <- result[z, paste0('b.adapt[',subj,']')]
        c <- result[z, paste0('c.adapt[',subj,']')]
        return(a * (1 + b*(x^-c - 1)))
      }, colour='blue', alpha=0.3)
    )
  }
  print(overlay.list)
  p + overlay.list
}

posterior.check(6, 4)
