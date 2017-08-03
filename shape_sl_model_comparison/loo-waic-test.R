library(loo)
library(gamlss)
library(dplyr)


load('generated-data/mcmc/power-model-jags-test.Rdata')
data <- read.csv('generated-data/extracted-data-niw.csv')
data <- data %>% mutate(is.predictable=recode(predictable, "1"=0, "3"=1)) %>% dplyr::select(-c(cond, predictable))
result.matrix <- as.matrix(as.mcmc(jags.result))

get.mcmc.sample <- function(m, parameter, step, indicators=NA){
  if(is.na(indicators)){
    param.string <- parameter
  } else {
    param.string <- paste0(parameter,'[', indicators,']')
  }
  return(m[step, param.string])
}

calculate.log.probability <- function(){
  chain.length <- dim(result.matrix)[1]
  total.data.points <- length(data$rt)
  log.prob.matrix <- matrix(0, nrow=chain.length, ncol=length(data$rt))
  pb <- txtProgressBar(min=1, max=chain.length, style=3)
  for(s in 1:chain.length){
    setTxtProgressBar(pb, s)
    for(n in 1:total.data.points){
      
      t <- data$t[n]
      subject <- data$subject[n]
      is.predictable <- data$is.predictable[n]
      
      # calculate mu
      if(is.predictable == 0){
        mu <- (get.mcmc.sample(result.matrix, 'a.adapt.0', s, subject) * (1 + get.mcmc.sample(result.matrix, 'b.adapt.0', s, subject) * (t ^ -get.mcmc.sample(result.matrix, 'c.adapt.0', s, subject) - 1)))
      } else {
        z <- get.mcmc.sample(result.matrix, 'z', s, subject)
        base <- (get.mcmc.sample(result.matrix, 'a.adapt.1', s, subject) * (1 + get.mcmc.sample(result.matrix, 'b.adapt.0', s, subject) * (t ^ -get.mcmc.sample(result.matrix, 'c.adapt.0', s, subject) - 1)))
        if(z == 1){
          mu <- base
        } else {
          rlr <- (1 + get.mcmc.sample(result.matrix, 'b.learn', s, subject) * (t ^ -get.mcmc.sample(result.matrix, 'c.learn', s, subject) - 1))
          mu <- base * rlr
        }
      }
      
      # calculate sigma
      sigma <- get.mcmc.sample(result.matrix, 'sd.rt', s, subject)
      
      # calculate nu
      if(is.predictable == 0){
        nu <- get.mcmc.sample(result.matrix, 'tau.rt.1', s)
      } else {
        nu <- get.mcmc.sample(result.matrix, 'tau.rt.2', s)
      }
      
      log.prob.matrix[s,n] <- dexGAUS(data$rt[n], mu = mu, sigma = sigma, nu = nu, log=T)
    }
  }
  return(log.prob.matrix)
}

lpm <- calculate.log.probability()

loo(lpm)
