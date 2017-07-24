library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source('visualize_models.R')
source('visualize_priors.R')
source('make_data.R')
source('fit_conversion.R')

prior.m<-list('intercept' = list(fn = 'gamma', par1 =800, par2 =100 ), 
              'base' = list(fn = 'unif', par1 =0, par2=.75),
              'rate' = list(fn = 'gamma', par1= .01,par2 =.3),
              'proportion' = list(fn =  'gamma', par1 =1, par2 = .1),
              'jump' = list(fn = 'beta', par1=.5, par2=30),
              'split' = list(fn = 'unif', par1=0, par2=72),
              'sigma' = list(fn = 'gamma', par1= 25, par2=5))

#variances
prior.v<-list('intercept' = list(fn = 'gamma', par1 =100, par2 =50 ), 
              'base' = list(fn = 'gamma', par1 =.2, par2=.2),
              'rate' = list(fn = 'gamma', par1= .2,par2 =.2),
              'proportion' = list(fn =  'gamma', par1 =1, par2 = 10),
              'jump' = list(fn = 'gamma', par1=.2, par2=.2),
              'split' = list(fn = 'gamma', par1=10, par2=50),
              'sigma' = list(fn = 'gamma', par1=20, par2=10))


prior.params(prior.v$base)
write_json(prior.m, 'prior.m.json')
write_json(prior.v, 'prior.v.json')

##plot prior distributions 
prior.plots('prior.m.json')
prior.plots('prior.v.json')

##draw fake data from the priors
model = c('power.power') #specify model
t = 1:72 
n.subjects = 20

subject.data<-generate.fake.data(prior.m,model,t,n.subjects)


params<-subject.data$params
fake.data<-subject.data$fake.data

#view fake data plots
which.subject= 12

graph.fake.data(fake.data,params,which.subject)

##run stan on fake data
nchains = 1


# fake.data<-subset(fake.data, model == 'power.constant')
fake.pred<-data.frame(j= fake.data$subject,t= fake.data$t, rt= fake.data$predictable, p = rep(1,length(fake.data$t)))
fake.unpred<-data.frame(j = fake.data$subject, t= fake.data$t, rt = fake.data$unpredictable, p = rep(0,length(fake.data$t)))
fake.data<-rbind(fake.pred,fake.unpred)

pp
fake.data
fake.data<-subset(fake.data, rt<2000)
I = max(fake.data$t)
J  = length(unique(fake.data$j))
P  = length(unique(fake.data$p))
N  = length(fake.data$rt)
rt = fake.data$rt
jj = fake.data$j
ii = fake.data$t
pp = fake.data$p

model.data<-list(list(I=I,
                      J=J,
                      P=P,
                      N=N,
                      rt=rt,
                      jj=jj,
                      ii=ii,
                      pp=pp))
model.data



test <- stan(file = 'rtu.stan', data = model.data, iter = 20000, warmup =2000 ,thin = 2, 
             chains = nchains, verbose = T)

which.subject=13

extract.fit.parameters(test, which.subject, 'power.constant')
params[which.subject+20]
graph.fit.data(test, subject.data$fake.data, which.subject, 'power.constant')
fit.summary<-summary( test , pars = c(paste0('sigma[',which.subject,']'),
                                     paste0('rtu_initial[',which.subject,']'),
                                     paste0('rtu_delta[',which.subject,']'),
                                     paste0('rtu_learning_rate[',which.subject,']'),
                                     paste0('rlr_initial[',which.subject,']')))
fit.summary
summary(test , pars = c(paste0('rtu_initial_var')))

prior.params(prior.v$proportion)
        