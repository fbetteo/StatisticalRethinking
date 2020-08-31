# Chapter 7. Exercises 
library(rethinking)
# 8M1  ####
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd.trim <- dd[ , c("log_gdp","rugged","cont_africa") ]

m8m1.ch <- map2stan( 
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ) ,
  data=dd.trim )
precis(m8m1.ch)
pairs(m8m1.ch)

m8m1.un <- map2stan( 
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ) ,
  data=dd.trim )
precis(m8m1.un)
pairs(m8m1.un)

m8m1.exp <- map2stan( 
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dexp(1)
  ) ,
  data=dd.trim )
precis(m8m1.exp)
pairs(m8m1.exp)

# plot sigma densities
sigma.ch <- extract.samples( m8m1.ch)$sigma
sigma.un <- extract.samples( m8m1.un)$sigma
sigma.ex <- extract.samples( m8m1.exp)$sigma

par(mfrow=c(1,1))
dens(sigma.ch, xlim=c(0.7, 1.2), col='red')
dens(sigma.un, add=T, col='blue')
dens(sigma.ex, add=T)

# 8M2
# The answer is that choice of Cauchy priors or exponentially scaled priors does not influence the result of inference.
# The only difference that I've encountered happens for exp(10) - mean is tiny shifted towards zero in this case. Still, it can be just the effect of the random nature of sampling.
# I assume that 170 observations are enough to overcome priors.

# 8M3 ####
formula8m3 <- alist(
  log_gdp ~ dnorm( mu , sigma ) ,
  mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
  a ~ dnorm(0,100),
  bR ~ dnorm(0,10),
  bA ~ dnorm(0,10),
  bAR ~ dnorm(0,10),
  sigma ~ dcauchy(0,2)
) 

m8m3.w1 <- map2stan(formula8m3, data=dd.trim, iter=1001, warmup=1)
precis(m8m3.w1)#awfull results, n_eff=1

m8m3.w10 <- map2stan(formula8m3, data=dd.trim, iter=1010, warmup=10)
precis(m8m3.w10)#not so awfull results, n_eff=~100..200, troubles with estimates bA & sigma
plot(m8m3.w10)

m8m3.w100 <- map2stan(formula8m3, data=dd.trim, iter=1100, warmup=100)
precis(m8m3.w100)#enough of warmup
plot(m8m3.w100)

m8m3.w500 <- map2stan(formula8m3, data=dd.trim, iter=1500, warmup=500)
precis(m8m3.w500)#"wasted" warmup
plot(m8m3.w500)

m8m3.w1k <- map2stan(formula8m3, data=dd.trim, iter=2000, warmup=1000)
precis(m8m3.w1k)#"wasted" warmup
plot(m8m3.w1k)

# 8H1

mp <- map2stan(
  alist(
    a ~ dnorm(0,1),
    b ~ dcauchy(0,1)
  ),
  data = list(y = 1),
  start = list(a = 0,b = 0),
  iter = 1e4, warmup = 100, WAIC = FALSE)
  
precis(mp)
pairs(mp)
tracerplot(mp)

# There is no model for the data so just sampling from the prior
# Cauchy is highly unstable
# The mean looks fine but sigma has an horrible traceplot.

samples <- extract.samples(mp)
hist(samples$a)#perfect Gausssian
hist(samples$b)#hard to examine, cause has sevral extreme values (as expected from cauchy thick tails)



# 8H3 ####
N <- 100 # number of individuals
height <- rnorm(N,10,2) # simulate total height of each individual
leg_prop <- runif(N,0.4,0.5) # leg as proportion of the height
leg_left <- leg_prop*height + # simulate left leg as proportion + error
  rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height + # simulate right leg as proportion + error
  rnorm( N , 0 , 0.02 )
# combine into data frame
d <- data.frame(height,leg_left,leg_right)

m5.8s <- map2stan(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=d, chains=4,
  start=list(a=10,bl=0,br=0,sigma=1) )
plot(m5.8s)
precis(m5.8s)
pairs(m5.8s)



m5.8s2 <- map2stan( 
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) & T[0,] ,
    sigma ~ dcauchy( 0 , 1 )
  ) ,
  data=d, chains=4,
  start=list(a=10,bl=0,br=0,sigma=1) )
plot(m5.8s2)
precis(m5.8s2)
pairs(m5.8s2)

coeftab(m5.8s, m5.8s2)
compare(m5.8s, m5.8s2)

# but sum of bl+br stay the same
par(mfrow=c(2,2))
ss5.8s <- extract.samples(m5.8s)
b1 <- ss5.8s$bl+ss5.8s$br
dens(ss5.8s$bl, col='red', ylim=c(0, .25), xlim=c(-12, 12))
abline(v=0, lty=2)
mtext('b_left model=m5.8s')
dens(ss5.8s$br, col='blue', ylim=c(0, .25), xlim=c(-12, 12))
abline(v=0, lty=2)
mtext('b_right model=m5.8s')

ss5.8s2 <- extract.samples(m5.8s2)
b2 <- ss5.8s2$bl+ss5.8s2$br
dens(ss5.8s2$bl, col='red', ylim=c(0, .25), xlim=c(-12, 12))
abline(v=0, lty=2)
mtext('b_left model=m5.8s2(truncated prior)')
dens(ss5.8s2$br, col='blue', ylim=c(0, .25), xlim=c(-12, 12))
abline(v=0, lty=2)
mtext('b_right model=m5.8s2(truncated prior)')

# plot sum of bl+br for both models
par(mfrow=c(1,1))
dens(b1, col='red', ylim=c(0,6.5), xlim=c(1.5,2.5))
dens(b2,add=T, col='blue')

# Effective number of parameters for the second model is smaller. 
# Intuitively it is smaller because we restricted "freedom" of the 'br' coefficient. This parameter couldn't be negative for the second model, while the probability of having big values is still very small as for the first model. Thus overall freedom of the model declined.
# More formally, pWAIC is defined as sum of variance of the points likelihood, thus the second model has smaller variance of data likelihood(==> it's 'more restricted')

#same story with effective numbet of parameters for DIC
DIC(m5.8s) # pD=3.9
DIC(m5.8s2) # pD=3.4