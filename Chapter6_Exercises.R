# Chapter 6. Exercises
library(tidyverse)
rm(list = ls())
#6E1

# continuous
# should be higher as the amount of possible events increase
# uncertainty of different types should be additive.

# 6E2
# entropy =  -E ln(pi) = -sum p_i*ln(p_i)

p = c(0.7,0.3)
entropy = -sum(p*log(p))

# 6E3
p = c(0.2,0.25,0.25,0.3)
entropy = -sum(p*log(p))

# 6E4
p = c(0.33,0.33,0.33)
entropy = -sum(p*log(p))


# 6M1
# Answer:
#   - From most to less general: WAIC > DIC > AIC
#   - assumptions: no > Gaussian posterior > Gaussian posterior + flat priors
#   - when the posterior predictive mean is a good representation of the posterior predictive
#   distribution, WAIC and DIC will tend to agree, as a  consequence from the Gaussian
#   assumptions on posterior distribution.
#   - when priors are effectively flat or overwhelmed by the amount of data, the DIC and AIC will tend to agree

# 6M2


# Model selection refers to selecting one (or several) models of many. Model averaging refers
# to weighting the predictions of many models by some relative information criterion in order
# to create an aggregate posterior prediction distribution.
#
# Under model selection, the strength of models that were almost as good is lost. Said
# differently, it discards model uncertainty. Under model averaging, we're often able to create
# the same ensembled posterior predictive distribution from two different sets of models. As
# such, information criteria values of each model (in each model set) is by definition
# somewhere lost.

# 6M3
# Information criteria are based on deviance, which is accrued over observations without being
# divided by the number of observations (page 182). Thus, it is a sum and not an average. So,
# all else being equal, a model with more observations will have a higher deviance and thus
# worse accuracy according to information criteria. It would be an unfair comparison to
# contrast models fit to different numbers of observations.


# 6M4
# Effective number of parameters should reduce, as models become less 'flexible' and more rigid

# 6M5
# Informative priors instruct our model to "not get too excited by the data." In other words,
# they tell our models not to overfit! Less sensitive to data

## 6M6

# If a prior is overly informative, even the regular features of a sample will not be learned
# by our model.

# 6H1
library(rethinking)
data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
set.seed( 1000 )
i <- sample(1:nrow(d),size=nrow(d)/2)
d1 <- d[ i , ]
d2 <- d[ -i , ]

formulas <- list()
sigma.rb <- 20
b.sd <- 100
a.sd <- 100

formulas[[1]] <- alist(
  height ~ dnorm(mu , sigma),
  mu <- a + b1*age,
  a ~ dnorm(0, a.sd),
  b1 ~ dnorm(0, b.sd),
  sigma ~ dunif(0, sigma.rb)
)
formulas[[2]] <- alist(
  height ~ dnorm(mu , sigma),
  mu <- a + b1*age + b2*age^2,
  a ~ dnorm(0, a.sd),
  b1 ~ dnorm(0, b.sd),
  b2 ~ dnorm(0, b.sd),
  sigma ~ dunif(0, sigma.rb)
)
formulas[[3]] <- alist(
  height ~ dnorm(mu , sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3,
  a ~ dnorm(0, a.sd),
  b1 ~ dnorm(0, b.sd),
  b2 ~ dnorm(0, b.sd),
  b3 ~ dnorm(0, b.sd),
  sigma ~ dunif(0, sigma.rb)
)
formulas[[4]] <- alist(
  height ~ dnorm(mu , sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4,
  a ~ dnorm(0, a.sd),
  b1 ~ dnorm(0, b.sd),
  b2 ~ dnorm(0, b.sd),
  b3 ~ dnorm(0, b.sd),
  b4 ~ dnorm(0, b.sd),
  sigma ~ dunif(0, sigma.rb)
)
formulas[[5]] <- alist(
  height ~ dnorm(mu , sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5,
  a ~ dnorm(0, a.sd),
  b1 ~ dnorm(0, b.sd),
  b2 ~ dnorm(0, b.sd),
  b3 ~ dnorm(0, b.sd),
  b4 ~ dnorm(0, b.sd),
  b5 ~ dnorm(0, b.sd),
  sigma ~ dunif(0, sigma.rb)
)
formulas[[6]] <- alist(
  height ~ dnorm(mu , sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5 + b6*age^6,
  a ~ dnorm(0, a.sd),
  b1 ~ dnorm(0, b.sd),
  b2 ~ dnorm(0, b.sd),
  b3 ~ dnorm(0, b.sd),
  b4 ~ dnorm(0, b.sd),
  b5 ~ dnorm(0, b.sd),
  b6 ~ dnorm(0, b.sd),
  sigma ~ dunif(0, sigma.rb)
)

models <- list()
for(i in 1:6){
  print(paste("fitting model ",i))
  models[[i]] <- rethinking::map(formulas[[i]], data=d1)
}


### 6H1
models.cmp <- compare(models[[1]], models[[2]], models[[3]],
                      models[[4]], models[[5]], models[[6]]) 
models.cmp
plot(models.cmp)



#### 6H2
age.seq <- seq(-2, 3.5, by=0.1)
par(mfrow=c(3,3))
plot.predictions <- TRUE
d4p <- d1
d.predict <- data.frame(age=age.seq)

plot_model <- function(name, m, d.predict, d.raw, plot.predictions.pi=TRUE){
  mu <- link(m, data=d.predict)
  mu.avg <- apply(mu, 2, mean)
  mu.pi <- apply(mu, 2, PI, .97)
  
  # calculate data for points predictions
  if(plot.predictions.pi){
    predictions <- sim(m, data=d.predict)
    #predictions.avg <- apply(predictions, 2, mean)
    predictions.pi <- apply(predictions, 2, PI, .97) 
  }
  
  plot(height ~ age, d.raw, xlim=c(-2,3.5), ylim=c(0, 200), col=col.alpha('slateblue', 0.5))
  lines(d.predict$age, mu.avg, col='red')
  shade(mu.pi, d.predict$age, col = col.alpha("blue",0.15), border=1)
  mtext(paste('#params=',name))
  if(plot.predictions){
    #lines(age.seq,predictions.avg)
    shade(predictions.pi, age.seq)
  }
}
for(i in 1:6) {
  m <- models[[i]]
  
  plot_model(i, m, d.predict, d4p)
}

# The predictions differ pretty dramatically across models. M1 is underfit and only makes good
# predictions when age is around -1 and 1. M2 does a better job but makes poor predictions
# below -1.5 and above 1.5. M3 does a better job still but introduces an unlikely upward tick
# after around 2.0. M4 does the best job of all, but has a wide fanning out of its interval
# after around 2.0. M5 and M6 are similar to M4 for the majority of the age range, until around
# 2.5 when they both fan out even worse around either a down or up-tick.


### 6H3
par(mfrow=c(2,1))
#### model 4
plot_model(4, models[[4]], d.predict, d4p)

#### ensemble
m.ensemble <- ensemble( models[[1]], models[[2]], models[[3]],
                        models[[4]], models[[5]], models[[6]] ,
                        data=d.predict )
mu.avg <- apply( m.ensemble$link , 2 , mean )
mu.pi <- apply( m.ensemble$link , 2 , PI, 0.97 )
predictions.pi <- apply(m.ensemble$sim, 2, PI, .97)

plot(height ~ age, d4p, xlim=c(-2,3.5), ylim=c(0, 200), col=col.alpha('slateblue', 0.5))
lines(age.seq, mu.avg, col='red')
shade(mu.pi, age.seq, col = col.alpha("blue",0.15), border=1)
shade(predictions.pi, age.seq)
mtext(paste('ensemble'))


## 6H4
#m1
calc_dev <- function(data, mu, sigma) {
  -2*sum(dnorm(data$height, mu, sigma,log = TRUE))
}
d4p <- d2
cf <- coef(models[[1]])
mu <- cf[['a']] + d4p$age*cf[['b1']]
tdev1 <- calc_dev(d4p, mu, cf[['sigma']])

cf <- coef(models[[2]])
mu <- cf[['a']] + d4p$age*cf[['b1']] + d4p$age^2*cf[['b2']]
tdev2 <- calc_dev(d4p, mu, cf[['sigma']])

cf <- coef(models[[3]])
mu <- cf[['a']] + d4p$age*cf[['b1']] + d4p$age^2*cf[['b2']] + d4p$age^3*cf[['b3']]
tdev3 <- calc_dev(d4p, mu, cf[['sigma']])

cf <- coef(models[[4]])
mu <- cf[['a']] + d4p$age*cf[['b1']] + d4p$age^2*cf[['b2']] + d4p$age^3*cf[['b3']] + d4p$age^4*cf[['b4']]
tdev4 <- calc_dev(d4p, mu, cf[['sigma']])

cf <- coef(models[[5]])
mu <- cf[['a']] + d4p$age*cf[['b1']] + d4p$age^2*cf[['b2']] + d4p$age^3*cf[['b3']] + d4p$age^4*cf[['b4']] + d4p$age^5*cf[['b5']]
tdev5 <- calc_dev(d4p, mu, cf[['sigma']])

cf <- coef(models[[6]])
mu <- cf[['a']] + d4p$age*cf[['b1']] + d4p$age^2*cf[['b2']] + d4p$age^3*cf[['b3']] + d4p$age^4*cf[['b4']] + d4p$age^5*cf[['b5']] + d4p$age^6*cf[['b6']]
tdev6 <- calc_dev(d4p, mu, cf[['sigma']])

# 6H5
tdev <- c(tdev1, tdev2, tdev3, tdev4, tdev5, tdev6)
d.tdev <- tdev-min(tdev)
names(d.tdev) <- paste0('m',1:6)
sort(d.tdev)
models.cmp


# M4 makes the best out-of-sample predictions as well as in-sample predictions. The ranking of
# models is also very similar by both WAIC and out-of-sample deviance. The only differences in
# ranking were swapping M5 and M6, although both were similar in magnitude. In this case, WAIC
# did a good job of estimating test deviance, although it tended to underestimate deviance for
# poorly fitting models. It’s also worth noting that we assigned observations to train and test
# sets randomly, which increases the likelihood that they are representative of one another.

# 6H6
b.sd <- 5
formulas66 <- alist(
  height ~ dnorm(mu , sigma),
  mu <- a + b1*age + b2*age^2 + b3*age^3 + b4*age^4 + b5*age^5 + b6*age^6,
  a ~ dnorm(0, a.sd),
  b1 ~ dnorm(0, b.sd),
  b2 ~ dnorm(0, b.sd),
  b3 ~ dnorm(0, b.sd),
  b4 ~ dnorm(0, b.sd),
  b5 ~ dnorm(0, b.sd),
  b6 ~ dnorm(0, b.sd),
  sigma ~ dunif(0, sigma.rb)
)
m66 <- map(formulas66, data=d1)
m66
models[[4]]
coeftab(models[[4]],m66)

plot_model('6 regularised', m66, d.predict, d1)



d4p <- d2
cf <- coef(m66)
mu <- cf[['a']] + d4p$age*cf[['b1']] + d4p$age^2*cf[['b2']] + d4p$age^3*cf[['b3']] + d4p$age^4*cf[['b4']] + d4p$age^5*cf[['b5']] + d4p$age^6*cf[['b6']]
tdev66 <- calc_dev(d4p, mu, cf[['sigma']])
tdev66
tdev4