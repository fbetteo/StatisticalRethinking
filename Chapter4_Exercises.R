# Chapter 4. Exercises
library(tidyverse)
rm(list = ls())
# 4M1

mu_prior = rnorm(1000,0, 10)
sigma_prior = runif(1000,0,10)

y_prior = rnorm(1000, mean = mu_prior, sd = sigma_prior)
rethinking::dens(y_prior)

# 4M2
mlist = alist(
  y ~ dnorm(mu, sigma),
  mu ~ dnorm(mean = 0, sd = 10),
  sigma = dunif(min = 0, max = 10)
)


# 4M3 a 6 directo en libro

# 4H1
library(rethinking)
data(Howell1)
d = Howell1


ggplot(data = d, aes(x = weight, y= height)) + 
  geom_point()

# modelo rapido basado en el plot
model = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(120,10),
    b ~ dnorm(10, 10),
    sigma ~ dunif(0,20)
),
  data = d
)


# simulate heights from model
individual.weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
simulated.heights <- sim(model, data = list(weight = individual.weights))

# summarize results
simulated.heights.mean <- apply(X = simulated.heights, MARGIN = 2, FUN = mean)
simulated.heights.PI <- apply(X = simulated.heights, MARGIN = 2, FUN = PI, prob = .89)

# 4H2
d2 = d[d$age > 18, ]

#a
model_h2 = rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(120,10),
    b ~ dnorm(10, 10),
    sigma ~ dunif(0,20)
  ),
  data = d2
)

summary(model_h2)

# 9 cm per 10 units

# b


# confidence interval
weight.seq = seq(from=0, to = 70, by = 1)
mu = rethinking::link(model_h2, data = data.frame(weight = weight.seq))
mu.mean = apply(mu, 2, mean)
mu.HPDI = apply(mu, 2, HPDI, prob = 0.89)

plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.HPDI , weight.seq )

# adding prediction interval
sim.height = sim(model_h2, data = list(weight = weight.seq))
str(sim.height)

height.PI = apply(sim.height, 2, PI, prob = 0.89)

plot(height ~weight, d2, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade(height.PI, weight.seq)


# 4H3
rm(list = ls())
data(Howell1)
d = Howell1

model_h3 = rethinking::map(
  alist(
    height ~dnorm(mu,sigma),
    mu <- a + b*log(weight),
    a ~ dnorm(178,100),
    b ~dnorm(0,100),
    sigma ~dunif(0,50)
  ),
  data = d
)

summary(model_h3)

# semi elasticidad? como que no parece tener sentido

# b
plot(height ~ weight, data = d, 
     col = col.alpha(rangi2, 0.4))

weight.seq = seq(from=0, to = 70, by = 1)
mu = rethinking::link(model_h3, data = data.frame(weight = weight.seq))
mu.mean = apply(mu, 2, mean)
mu.HPDI = apply(mu, 2, HPDI, prob = 0.97)

sim.height = sim(model_h3, data = list(weight = weight.seq))
height.PI = apply(sim.height, 2, PI, prob = 0.97)

plot(height ~weight, d, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade(height.PI, weight.seq)
