# Chapter 4
# Modeling
library(rethinking)
data(Howell1)
d = Howell1 # height, weight, age and gender

d2 = d[d$age >= 18, ] # just adults

# Modeling Height without independent variables
# height as a Normal mu, sigma
# where mu has a prior also Normal 178,20
# and sigma has a prior uniform 0,50  . Only positives

# ploting priors
curve(dnorm(x,178,20), from = 100, to = 250)
curve(dunif(x, 0, 50), from = -10, to = 60)

# simulate heights sampling from the priors (wihout seeing DATA!)
sample_mu = rnorm(1e4, 178, 20)
sample_sigma = runif(1e4, 0, 50)
prior_h = rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

# grid search para encontrar posterior
# todo en log
mu.list = seq(from = 140, to = 160, length.out = 200)
sigma.list = seq(from = 4, to = 9, length.out = 200)
post = expand.grid(mu = mu.list, sigma = sigma.list) # grid
post$LL = sapply(1:nrow(post), # likelihood
                 function(i) sum( 
                   dnorm(
                   d2$height,
                   mean = post$mu[i],
                   sd = post$sigma[i],
                   log = TRUE)
                   )
                 )
post$prod = post$LL + dnorm(post$mu, 178 , 20, log = TRUE) + dunif( post$sigma, 0, 50, log = TRUE) # likelihood por priors
post$prob = exp(post$prod - max(post$prod)) # nunca calcula el denominador de bayes no?
# normaliza por el maximo porque todos los valores son muy chicos y se redondearia a 0 sino
# ver nota 67 del libro
# https://www.xarg.org/2016/06/the-log-sum-exp-trick-in-machine-learning/ es para evitar rounding problems (?)

# viz
rethinking::contour_xyz(post$mu, post$sigma, post$prob)
rethinking::image_xyz(post$mu, post$sigma, post$prob)

# sampling from the posterior
sample.rows = sample(1:nrow(post), size = 1e4,
                     replace = TRUE,
                     prob = post$prob)
sample.mu = post$mu[ sample.rows ]
sample.sigma = post$sigma[ sample.rows ]

plot( sample.mu, sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2,0.1))

# visiaulizar marginal posteriors
dens(sample.mu)
dens(sample.sigma)

# HPDI
HPDI(sample.mu)
HPDI(sample.sigma)

### Analisis con 20 casos
d3 = sample(d2$height, size = 20)

# mismo codigo de antes, cambian boundaries
mu.list = seq(from = 150, to = 170, length.out = 200)
sigma.list = seq(from = 4, to = 20, length.out = 200)
post2 = expand.grid(mu = mu.list, sigma = sigma.list) # grid
post2$LL = sapply(1:nrow(post), # likelihood
                 function(i) sum( 
                   dnorm(
                     d3,
                     mean = post2$mu[i],
                     sd = post2$sigma[i],
                     log = TRUE)
                 )
)
post2$prod = post2$LL + dnorm(post2$mu, 178 , 20, log = TRUE) + dunif( post2$sigma, 0, 50, log = TRUE) # likelihood por priors
post2$prob = exp(post2$prod - max(post2$prod)) # nunca calcula el denominador de bayes no?
sample2.rows = sample(1:nrow(post2), size = 1e4,
                     replace = TRUE,
                     prob = post2$prob)
sample2.mu = post2$mu[ sample2.rows ]
sample2.sigma = post2$sigma[ sample2.rows ]

plot( sample2.mu, sample2.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2,0.1))

dens(sample2.sigma, norm.comp = TRUE)


#### MAP -----

library(rethinking)
data(Howell1)
d = Howell1
d2 = d[d$age >= 18, ]

# model definition
flist = alist(  # se usa para formulas parece porque no evalua el codigo
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178,20),
  sigma ~ dunif(0,50)
)

#fit
m4.1 = rethinking::map(flist, data = d2)
precis(m4.1)



# Prior de Mu casi sin desvío
m4.2 = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0,50)
  ),
  data = d2)
  
precis(m4.2)
vcov(m4.1) # var cov de los parametros de la posterior

# descomposicion en varianza y correlaciones
diag(vcov(m4.1)) # varianzas
cov2cor(vcov(m4.1)) # correlacion

# sampleamos de la multi dimensional posterior
post = extract.samples(m4.1, n = 1e4)
head(post)

precis(post)

#### Estimar log  de sigma ya que sigma es posible que sea LogNormal con poca data ----
m4.1_logsigma = rethinking::map(
  alist(
    height ~ dnorm(mu, exp(log_sigma)),
    mu ~ dnorm(178, 20),
    log_sigma ~ dnorm(2,10)
    
  ),
  data = d2
)

post = extract.samples(m4.1_logsigma)
sigma = exp(post$log_sigma)


#### MAP modelo con predictor ----

library(rethinking)
data(Howell1)
d = Howell1
d2 = d[d$age > 18, ]


# fit model
m4.3 = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(178, 100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,50)
    
  ),
  data = d2
)

# plot superimposing MAP
plot(height ~ weight, data = d2)
abline( a = coef(m4.3)["a"], b = coef(m4.3)["b"])

post = extract.samples(m4.3)
post[1:5,]

# reestimamos con menos data para plotear incertidumbre más claramente
N = 10
dN = d2[1:N,]
mN = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(178, 100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,50)
    
  ),
  data = dN
)

# ploteamos 20 lineas para ver incertidumbre
# extraemos 20 muestras de la posterior
post = extract.samples(mN, n = 20)

# display raw data and sample size
plot(dN$weight, dN$height, 
     xlim = range(d2$weight), ylim = range(d2$height),
     col = rangi2, xlab = "weight", ylab = "height")
mtext(concat("N = ", N))

# plot the lines, with transparency
for (i in 1:20)
  abline( a = post$a[i], b = post$b[i], col = col.alpha("black", 0.3))

# Confidence Interval pg 105 106 ---- 

weight.seq = seq(from=25, to = 70, by = 1)

mu = rethinking::link(m4.3, data = data.frame(weight = weight.seq))

plot(height ~ weight, d2, type = "n")
for (i in 1:100){
  points(weight.seq, mu[i,], pch = 16, col = col.alpha(rangi2, 0.1))
}

mu.mean = apply(mu, 2, mean)
mu.HPDI = apply(mu, 2, HPDI, prob = 0.89)

plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.HPDI , weight.seq )

# Prediction interval ----

sim.height = sim(m4.3, data = list(weight = weight.seq))
str(sim.height)

height.PI = apply(sim.height, 2, PI, prob = 0.89)

plot(height ~weight, d2, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade(height.PI, weight.seq)
