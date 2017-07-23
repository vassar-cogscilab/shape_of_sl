#install.packages('polynom')
require(polynom)
require(MCMCpack)


source('model_predict.R')
source('DBDA2E-utilities.R')

convert.prior<-function(prior, m){
  new.prior<-list()
  
  
  new.prior<-list('intercept' = prior$intercept, 
                  'base' = prior$base,
                  'rate' = prior$rate,
                  'proportion' = prior$proportion,
                  'jump' = prior$jump,
                  'base.1' = prior$base,
                  'rate.1' = prior$rate,
                  'split' = prior$split,
                  'sigma' = prior$sigma)
  return(new.prior)
  
}




model.params<-function(fit,m){
  
  if(m == 'power.constant'){
    params<- c(as.numeric(fit$power.constant$intercept),
               as.numeric(fit$power.constant$base),
               as.numeric(fit$power.constant$rate),
               as.numeric(fit$power.constant$proportion),
               as.numeric(fit$power.constant$sigma))
  }
  
  if(m == 'power.power'){
    params<-c(as.numeric(fit$power.power$intercept),
              as.numeric(fit$power.power$base),
              as.numeric(fit$power.power$rate),
              as.numeric(fit$power.power$proportion),
              as.numeric(fit$power.power$base.1),
              as.numeric(fit$power.power$rate.1),
              as.numeric(fit$power.power$sigma))
  }
  if(m == 'piecewise.power.constant'){
    params<-c(as.numeric(fit$piecewise.power.constant$intercept),
              as.numeric(fit$piecewise.power.constant$base),
              as.numeric(fit$piecewise.power.constant$rate),
              as.numeric(fit$piecewise.power.constant$proportion),
              as.numeric(fit$piecewise.power.constant$jump),
              as.numeric(fit$piecewise.power.constant$split),
              as.numeric(fit$piecewise.power.constant$sigma))
  }
  if(m == 'power.logistic'){
    params<-c(as.numeric(fit$power.logistic$intercept),
              as.numeric(fit$power.logistic$base),
              as.numeric(fit$power.logistic$rate),
              as.numeric(fit$power.logistic$proportion),
              as.numeric(fit$power.logistic$jump),
              as.numeric(fit$power.logistic$rate.1),
              as.numeric(fit$power.logistic$split),
              as.numeric(fit$power.logistic$sigma))
  }
  return(params)
}



r.normal<- function(params){
  mu<-params[1]
  sd<-params[2]
  return(rnorm(1, mean = mu , sd = sd))
}

r.gamma<- function(params){
  a<-gammaShRaFromModeSD(params[1], params[2])$shape
  b<-gammaShRaFromModeSD(params[1], params[2])$rate
  return(rgamma(1, shape = a , rate = b))
}

r.invgamma<- function(params){
  p<- c(params[2], - (4*params[2] + params[1]^2), (5*params[2] - 2*params[1]^2), -(2*params[2] + params[1]^2))
  p<- as.polynomial(p)
  
  a<- max(Re(solve(p)))
  b<- params[1]*(a +1)
  return(rinvgamma(1, a , b))
}


r.beta<-function (params){
  a<-betaABfromModeKappa(params[1],params[2])$a
  b<-betaABfromModeKappa(params[1],params[2])$b
  return(rbeta(1, a, b))
}

r.exp<-function(params){
  return(rexp(1, params[1]))
}

r.unif<-function(params){
  return(runif(1, params[1], params[2]))
}

r.pois<-function(params){
  return(rpois(1, params[1]))
}

r.cat<-function(params){
  return(rmultinom(1, 1, rep(params[1],params[2])))
}


r.prior.fn<- function(fn, params){
  return(fn(params))
}



r.prior<-function(type, priors){
  
  if(type == 'intercept'){
    fn <- priors$intercept$fn
    params<- c(as.numeric(priors$intercept$par1), as.numeric(priors$intercept$par2))
  }
  if(type == 'base'){
    fn <- priors$base$fn
    params<- c(as.numeric(priors$base$par1), as.numeric(priors$base$par2))
  }
  if(type == 'base.1'){
    
    fn <- priors$base.1$fn
    params<- c(as.numeric(priors$base.1$par1), as.numeric(priors$base.1$par2))
    
  }
  if(type == 'rate' ){
    fn <- priors$rate$fn
    params<- c(as.numeric(priors$rate$par1), as.numeric(priors$rate$par2))
  }
  if(type == 'rate.1'){
    fn <- priors$rate.1$fn
    params<- c(as.numeric(priors$rate.1$par1), as.numeric(priors$rate.1$par2))
  }
  if(type == 'sigma'){
    fn <- priors$sigma$fn
    params<- c(as.numeric(priors$sigma$par1), as.numeric(priors$sigma$par2))
  }
  if(type == 'split'){
    fn<- priors$split$fn
    params<- c(as.numeric(priors$split$par1), as.numeric(priors$split$par2))
  }
  if(type == 'jump'){
    fn <- priors$jump$fn
    params<- c(as.numeric(priors$jump$par1), as.numeric(priors$jump$par2))
  }
  if(type == 'proportion'){
    fn <- priors$proportion$fn
    params<- c(as.numeric(priors$proportion$par1), as.numeric(priors$proportion$par2))
  }
  
  
  
  
  
  
  if(fn == 'normal'){
    return(r.prior.fn(r.normal,params))
  }
  if(fn == 'gamma'){
    return(r.prior.fn(r.gamma,params))
  }
  if(fn == 'inv_gamma'){
    return(r.prior.fn(r.invgamma,params))
  }
  if(fn == 'beta'){
    return(r.prior.fn(r.beta,params))
  }
  if(fn == 'exp'){
    return(r.prior.fn(r.exp,params))
  }
  if(fn == 'unif'){
    return(r.prior.fn(r.unif,params))
  }
  if(fn == 'pois'){
    return(r.prior.fn(r.pois,params))
  }
  if(fn == 'cat'){
    return(r.prior.fn(r.cat,params))
  }
}


generate.parameter.values<-function(priors, model = c('power.constant',
                                                      'power.logistic',
                                                      'power.power',
                                                      'piecewise.power.constant')){
  
  
  recovery.parameter.values<- list()
  i=0
  
  
  intercept = r.prior('intercept',priors)
  base= r.prior('base',priors)
  rate= - r.prior('rate',priors)
  
  for(m in model){
    i=i+1
    if(m == 'power.constant'){
      recovery.parameter.values[[i]] = list(intercept = intercept,
                                            base= base, 
                                            rate= rate,
                                            proportion = r.prior('proportion',priors),
                                            sigma = r.prior('sigma',priors))
    }
    if(m == 'piecewise.power.constant'){
      recovery.parameter.values[[i]] =  list(intercept = intercept,
                                             base= base, 
                                             rate= rate,
                                             proportion = r.prior('proportion',priors),
                                             jump = r.prior('jump',priors),
                                             split = r.prior('split',priors),
                                             sigma=r.prior('sigma',priors))
    }
    if(m == 'power.power'){
      recovery.parameter.values[[i]] = list(intercept = intercept,
                                            base= base, 
                                            rate= rate,
                                            proportion = r.prior('proportion',priors), 
                                            base.1= r.prior('base.1',priors), 
                                            rate.1= -r.prior('rate.1',priors),
                                            sigma = r.prior('sigma',priors))
    }
    if(m == 'power.logistic'){
      recovery.parameter.values[[i]] =  list(intercept = intercept,
                                             base= base, 
                                             rate= rate,
                                             proportion = r.prior('proportion',priors), 
                                             jump = r.prior('jump',priors), 
                                             rate.1= -r.prior('rate.1',priors),
                                             split = r.prior('split',priors),
                                             sigma=r.prior('sigma',priors))
      
    }
  }
  names(recovery.parameter.values) = model
  return(recovery.parameter.values)
}


add.noise<-function(recover,sigma){
  n<-length(recover)
  
  recover.plus.noise<-numeric()
  for(i in 1:n){
    noise<-rnorm(1, 0, sigma)
    val<-recover[i]
    while(abs(val)<= abs(noise)){
      noise<-rnorm(1,0,sigma)
    }
    recover.plus.noise[i]<-val+noise
  }
  return(recover.plus.noise)
}


create.recovery.data<-function(par, t, models= c('power.constant',
                                                 'power.logistic',
                                                 'power.power',
                                                 'piecewise.power.constant')){
  
  
  
  for(m in models){
    
    
    if(m == 'power.constant'){
      params<-model.params(par, 'power.constant')
      rtu<-mapply(u.power.model.predict,t, MoreArgs = list(params[1],params[2],params[3]))
      rl<-mapply(rl.constant.model.predict,t,MoreArgs= list(params[4]))
      rtp<-rl*rtu
      power.constant.recover.u<-add.noise(rtu,sqrt(params[5]))
      power.constant.recover.p<-add.noise(rtp,sqrt(params[5]))
    }
    
    if(m == 'power.logistic'){
      
      
      params<-model.params(par, 'power.logistic')
      rtu<-mapply(u.power.model.predict,t,MoreArgs = list(params[1],params[2],params[3]))
      rl<-mapply(rl.logistic.model.predict,t,MoreArgs = list(params[4],params[5],params[6],params[7]))
      rtp<-rl*rtu
      
      print(params[8])
      
      power.logistic.recover.u<-add.noise(rtu,sqrt(params[8]))
      power.logistic.recover.p<-add.noise(rtp,sqrt(params[8]))
    }
    
    if(m == 'power.power'){
      params<-model.params(par, 'power.power')
      rtu<-mapply(u.power.model.predict,t,MoreArgs = list(params[1],params[2],params[3]))
      rl<-mapply(rl.power.model.predict,t,MoreArgs = list(params[4],params[5],params[6]))
      
      rtp<-rl*rtu
      power.power.recover.u<-add.noise(rtu,sqrt(params[7]))
      power.power.recover.p<-add.noise(rtp,sqrt(params[7]))
    }
    if(m == 'piecewise.power.constant'){
      rtp<-numeric(length(t))
      params<-model.params(par, 'piecewise.power.constant')
      t1<-head(t,params[6])
      t2<-tail(t,-params[6])
      
      rtu<-as.numeric(mapply(u.power.model.predict,t,MoreArgs = list(params[1],params[2],params[3])))
      rl1<-as.numeric(mapply(rl.constant.model.predict,t1,MoreArgs = list(params[4])))
      rl2<-as.numeric(mapply(rl.constant.model.predict,t2,MoreArgs = list(params[5])))
      
      rtp[t1]<-rl1[t1]*rtu[t1]
      
      
      rtp<-c(rtp, rl2*rtu[t2])
      
      piecewise.power.constant.recover.u<-add.noise(rtu,sqrt(params[7]))
      piecewise.power.constant.recover.p<-add.noise(rtp,sqrt(params[7]))
      
      
    }
  }
  
  
  recovery.data<-expand.grid(t=t, model=models)
  
  rtu<-c()
  rtp<-c()
  
  for(m in models){
    if(m == 'power.constant'){
      rtu<-c(rtu,power.constant.recover.u)
      rtp<-c(rtp,power.constant.recover.p)
    }
    
    if(m == 'power.logistic'){
      rtu<-c(rtu,power.logistic.recover.u)
      rtp<-c(rtp,power.logistic.recover.p)
    }
    
    if(m == 'power.power'){
      rtu<-c(rtu,power.power.recover.u)
      rtp<-c(rtp,power.power.recover.p)
    }
    if(m == 'piecewise.power.constant'){
      rtu<-c(rtu,piecewise.power.constant.recover.u)
      rtp<-c(rtp,piecewise.power.constant.recover.p)
    }
  }
  
  recovery.data$predictable<-rtp
  recovery.data$unpredictable<-rtu
  
  return(recovery.data)  
}



fake.subject.data <- function(prior, model, t, n.subjects){
  
  fake.prior<-convert.prior(prior, model)
  
  params = list()
  fake.data = data.frame(subject=numeric(),t=numeric(),model=character(),predictable= numeric(),unpredictable=numeric())
  
  for(i in 1:n.subjects){
    
    params[[i]]<-generate.parameter.values(fake.prior, model= model)
    
    
    fake.subject <- cbind(subject = rep(i, length(t)),create.recovery.data(params[[i]], t, models = model))
    fake.data <- rbind(fake.data, fake.subject)
  }
  return(list(fake.data = fake.data,params = params))
}

generate.fake.data<-function(prior.mean, model,t,n.subjects){
  fake.data.l<-fake.subject.data(prior.m,model,t,n.subjects)
  fake.data.c<-fake.subject.data(prior.m,c('power.constant'),t,n.subjects)
  
  params.l<-fake.data.l$params
  params.c<-fake.data.c$params
  
  params<-c(params.l,params.c)
  
  fake.subject.l<-subset(fake.data.l$fake.data,model ==model)
  fake.subject.c<-subset(fake.data.c$fake.data,model =='power.constant')
  fake.subject.c$subject<-fake.subject.c$subject+20
  
  fake.data<-rbind(fake.subject.l,fake.subject.c)
  
  return(list(fake.data=fake.data,params=params))
}

prior.params<-function(prior){
  
  noise<-rbeta(1,1,1)
  fn<-prior$fn
  params<-c(prior$par1,prior$par2)
  
  if(fn == 'normal'){
    return(params*(.5+noise))
  }
  if(fn == 'gamma'){
    a<-gammaShRaFromModeSD(params[1], params[2])$shape
    b<-gammaShRaFromModeSD(params[1], params[2])$rate
    params<-c(a,b)
    return(params*(.5+noise))
  }
  if(fn == 'inv_gamma'){
    
    p<- c(params[2], - (4*params[2] + params[1]^2), (5*params[2] - 2*params[1]^2), -(2*params[2] + params[1]^2))
    p<- as.polynomial(p)
    
    a<- max(Re(solve(p)))
    b<- params[1]*(a +1)
    
    params<-c(a,b)
    return(params*(.5+noise))
  }
  if(fn=='beta'){
    a<-betaABfromModeKappa(params[1], params[2])$a
    b<-betaABfromModeKappa(params[1],params[2])$b
    params<-c(a,b)
    return(params*(.5+noise))
  }
  
  if(fn =='unif'){
    return(params*(.5+noise))
  }
  
  if(fn == 'exp'){
    return(params*(.5+noise))
  }
}
