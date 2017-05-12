library(DEoptim)
library(ggplot2)

# get data loaded in ####
model.data <- read.csv('generated_data/extracted-data-niw.csv')

##likelihood Normal, equal sd##

log.likelihood<-function(x,mu,sigma){
  sigma=10
  return(dnorm(x,mu,sigma,log=T))
}

likelihood<-function(data, mu,sigma){
  log.lik<-sum(mapply(log.likelihood,data,mu,sigma))
  return(log.lik)
}

##Model predit##
linear.model.predict<-function(t,a,b){
  rt<-a+b*t
  return(rt)
}

constant.model.predict<-function(t,s){
  rt<-s
  return(rt)
}

exponential.model.predict<-function(t,a,b,c){
  rt<-a+b*exp(c*t)
  return(rt)
}
power.model.predict<-function(t,a,b,c){
  rt<-a+b*t^(c)
  return(rt)
}
constant.piecewise.model.predict<-function(t,a,b,c){
  if(t<c){
    rt<-a
  }
  if(t>=c){
    rt<-b
  }
  return(rt)
}
linear.piecewise.model.predict<-function(t,a,b,c,d,e){
  if(t < c){
    rt <-a + b*t
  }
  if(t >= c){
    rt <-d + e*(t-c+1)
  }
  return(rt)
}
exponential.piecewise.model.predict<-function(t,a,b,c,d,e,f,g){
  if(t<d){
    rt<-a+b*exp(c*t)
  }
  if(t>=d){
    rt<-e+f*exp(g*(t-d+1))
  }
  return(rt)
}
power.piecewise.model.predict<-function(t,a,b,c,d,e,f,g){
  if(t<d){
    rt<-a + b*t^(c)
  }
  if(t>=d){
    rt<-e + f*(t-d+1)^(g)
  }
  return(rt)
}



##fit function##
subject.fit.MLE<-function(subject.data){
  
  subject.data<-subject.data
  
  constant.fit<-function(params){
    t<-subject.data$t
    a<-params[1]
    sigma<-params[2]
    subject.data$model.predict<-mapply(constant.model.predict,t,a)
    lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma)
    return(-lik)
  }
  
  linear.fit<-function(params){
    t<-subject.data$t
    a<-params[1]
    b<-params[2]
    sigma<-params[3]
    subject.data$model.predict<-mapply(linear.model.predict,t,a,b)
    lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma)
    return(-lik)
  }
  
  exponential.fit<-function(params){
    t<-subject.data$t
    a<-params[1]
    b<-params[2]
    c<-params[3]
    sigma<-params[4]
    subject.data$model.predict<-mapply(exponential.model.predict,t,a,b,c)
    lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma)
    return(-lik)
  }
  
  power.fit<-function(params){
    t<-subject.data$t
    a<-params[1]
    b<-params[2]
    c<-params[3]
    sigma<-params[4]
    subject.data$model.predict<-mapply(power.model.predict,t,a,b,c)
    lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma)
    return(-lik)
  }
  
  constant.piecewise.fit<-function(params){
    t<-subject.data$t
    a<-params[1]
    b<-params[2]
    c<-params[3]
    sigma<-params[4]
    subject.data$model.predict<-mapply(constant.piecewise.model.predict,t,a,b,c)
    lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma)
    return(-lik)
  }
  
  linear.piecewise.fit<-function(params){
    t<-subject.data$t
    a<-params[1]
    b<-params[2]
    c<-params[3]
    d<-params[4]
    e<-params[5]
    sigma<-params[6]
    subject.data$model.predict<-mapply(linear.piecewise.model.predict,t,a,b,c,d,e)
    lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma)
    return(-lik)
  }
  
  exponential.piecewise.fit<-function(params){
    t<-subject.data$t
    a<-params[1]
    b<-params[2]
    c<-params[3]
    d<-params[4]
    e<-params[5]
    f<-params[6]
    g<-params[7]
    sigma<-params[8]
    subject.data$model.predict<-mapply(exponential.piecewise.model.predict,t,a,b,c,d,e,f,g)
    lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma)
    return(-lik)
  }
  
  power.piecewise.fit<-function(params){
    t<-subject.data$t
    a<-params[1]
    b<-params[2]
    c<-params[3]
    d<-params[4]
    e<-params[5]
    f<-params[6]
    g<-params[7]
    sigma<-params[8]
    subject.data$model.predict<-mapply(power.piecewise.model.predict,t,a,b,c,d,e,f,g)
    lik<-likelihood(subject.data$difference, subject.data$model.predict,sigma)
    return(-lik)
  }
  
  constant<-DEoptim(constant.fit, lower=c(-2001,1),upper=c(2001,1000))
  linear<-DEoptim(linear.fit,lower=c(-2001, -27,1),upper=c(2001,27,1000))
  exponential<-DEoptim(exponential.fit,lower=c(-2001,-1000,-25,1),upper=c(2001,0,0,1000))
  power<-DEoptim(power.fit,lower=c(-2001,-1000,-25,1),upper=c(2001,0,0,1000))
  constant.piecewise    <- DEoptim(constant.piecewise.fit,    lower = c(-2001,-2001,0,1),                          upper=c(2001,2001,72,1000))
  linear.piecewise      <- DEoptim(linear.piecewise.fit,      lower = c(-2001, 0, 0,-2001, 0,1),                  upper=c(2001,27,72,2001,27,1000))
  exponential.piecewise <- DEoptim(exponential.piecewise.fit, lower = c(-2001,-1000, -25, 0, -2001, -1000, -25, 1), upper=c(2001,0,0,72,2001,0,0,1000))
  power.piecewise       <- DEoptim(power.piecewise.fit,       lower = c(-2001,-1000, -25, 0, -2001, -1000, -25, 1), upper=c(2001,0,0,72,2001,0,0,1000))
  
  constant.values<-list('intercept'=constant$optim$bestmem[1],'sigma' =constant$optim$bestmem[2], 'Likelihood'=constant$optim$bestval)
  linear.values<-list ('intercept'=linear$optim$bestmem[1],'slope'=linear$optim$bestmem[2], 'sigma'=linear$optim$bestmem[3], 'Likelihood'=linear$optim$bestval)
  exponential.values<-list('intercept'=exponential$optim$bestmem[1], 'base'= exponential$optim$bestmem[2],'rate'=exponential$optim$bestmem[3], 'sigma'=exponential$optim$bestmem[4], 'Likelihood'=exponential$optim$bestval)
  power.values<-list('intercept'=power$optim$bestmem[1],'base'=power$optim$bestmem[2],'rate'=power$optim$bestmem[3], 'sigma' =power$optim$bestmem[4],'Likelihood'=power$optim$bestval)
  constant.piecewise.values<-list('intercept'=constant.piecewise$optim$bestmem[1],'intercept.2'=constant.piecewise$optim$bestmem[2],'split'=constant.piecewise$optim$bestmem[3], 'sigma'=constant.piecewise$optim$bestmem[4], 'Likelihood'=constant.piecewise$optim$bestval)
  linear.piecewise.values<-list('intercept'=linear.piecewise$optim$bestmem[1], 'slope'=linear.piecewise$optim$bestmem[2], 'split'= linear.piecewise$optim$bestmem[3],'intercept.2'=linear.piecewise$optim$bestmem[4], 'slope.2'=linear.piecewise$optim$bestmem[5], 'sigma'=linear.piecewise$optim$bestmem[6],  'Likelihood'=linear.piecewise$optim$bestval)
  exponential.piecewise.values<-list('intercept'=exponential.piecewise$optim$bestmem[1],'base'=exponential.piecewise$optim$bestmem[2], 'rate'=exponential.piecewise$optim$bestmem[3],'split'=exponential.piecewise$optim$bestmem[4],'intercept.2'= exponential.piecewise$optim$bestmem[5], 'base.2'=exponential.piecewise$optim$bestmem[6],'rate.2'=exponential.piecewise$optim$bestmem[7], 'sigma'=exponential.piecewise$optim$bestmem[8],   'Likelihood'=exponential.piecewise$optim$bestval)
  power.piecewise.values<-list('intercept'=power.piecewise$optim$bestmem[1], 'base' = power.piecewise$optim$bestmem[2], 'rate'=power.piecewise$optim$bestmem[3],'split'=power.piecewise$optim$bestmem[4],'intercept.2'= power.piecewise$optim$bestmem[5], 'base.2'=power.piecewise$optim$bestmem[6], 'rate.2'=power.piecewise$optim$bestmem[7],'sigma'=power.piecewise$optim$bestmem[8],'Likelihood'=power.piecewise$optim$bestval)
  
  out <-list(
    constant=constant.values,
    linear=linear.values,
    exponential=exponential.values,
    power=power.values,
    constant.piecewise=constant.piecewise.values,
    linear.piecewise=linear.piecewise.values,
    power.piecewise=power.piecewise.values,
    exponential.piecewise=exponential.piecewise.values
  )
  
  return(out)
}

# fake data to verify that power law is working
# fake.data <- data.frame(t=1:72)
# fake.data$difference <- sapply(fake.data$t,function(x){ return(exponential.piecewise.model.predict(x, -200, -100, -1, 40, 400, -400, -1)) })
# plot(difference ~ t, data=fake.data)
all.subject.fits <- list()
#for(i in 1:3){
  ## run test case ####
  which.subject <- 3
  subject.data <- subset(model.data, subject==which.subject)
  #subject.data <- fake.data
  fit <- subject.fit.MLE(subject.data)
  fit$subject_id <- i
  all.subject.fits[[i]] <- fit
#}

 
 line.fit <- expand.grid(t=1:72, model=c('constant','linear','exponential','power','constant.piecewise', 'linear.piecewise', 'power.piecewise', 'exponential.piecewise'))
 
 line.fit$difference <- mapply(function(m,t){
   if(m == 'constant'){
     return(constant.model.predict(t,
                                           fit$constant$intercept))
   }
   if(m == 'linear'){
     return(linear.model.predict(t,
                                           fit$linear$intercept,
                                           fit$linear$slope))
   }
   if(m == 'exponential'){
     return(exponential.model.predict(t,
                                                fit$exponential$intercept,
                                                fit$exponential$base,
                                                fit$exponential$rate))
   }
   if(m == 'power'){
     return(power.model.predict(t,
                                          fit$power$intercept,
                                          fit$power$base,
                                          fit$power$rate))
   }
   if(m == 'constant.piecewise'){
     return(constant.piecewise.model.predict(t,
                                             fit$constant.piecewise$intercept,
                                             fit$constant.piecewise$intercept.2,
                                             fit$constant.piecewise$split))
   }
   if(m == 'linear.piecewise'){
     return(linear.piecewise.model.predict(t,
                                           fit$linear.piecewise$intercept,
                                           fit$linear.piecewise$slope,
                                           fit$linear.piecewise$split,
                                           fit$linear.piecewise$intercept.2,
                                           fit$linear.piecewise$slope.2))
   }
   if(m == 'exponential.piecewise'){
     return(exponential.piecewise.model.predict(t,
                                             fit$exponential.piecewise$intercept,
                                             fit$exponential.piecewise$base,
                                             fit$exponential.piecewise$rate,
                                             fit$exponential.piecewise$split,
                                             fit$exponential.piecewise$intercept.2,
                                             fit$exponential.piecewise$base.2,
                                             fit$exponential.piecewise$rate.2))
   }
   if(m == 'power.piecewise'){
     return(power.piecewise.model.predict(t,
                                          fit$power.piecewise$intercept,
                                          fit$power.piecewise$base,
                                          fit$power.piecewise$rate,
                                          fit$power.piecewise$split,
                                          fit$power.piecewise$intercept.2,
                                          fit$power.piecewise$base.2,
                                          fit$power.piecewise$rate.2))
   }
 },line.fit$model, line.fit$t)
 
 subject.data$difference
 
 ggplot(subject.data, aes(x=t, y=difference)) + 
  geom_point()+
  geom_line(data=line.fit, aes(color=model), size=1)

 Likelihood<-c(fit$constant$Likelihood,
                  fit$linear$Likelihood,
                   fit$exponential$Likelihood,
                   fit$power$Likelihood,
                   fit$constant.piecewise$Likelihood,
                   fit$linear.piecewise$Likelihood,
                   fit$exponential.piecewise$Likelihood,
                   fit$power.piecewise$Likelihood)
 best.fit.lik<-which.min(Likelihood)
 Likelihood
 