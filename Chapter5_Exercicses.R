# Chapter 5. Exercises
library(tidyverse)
rm(list = ls())
# 5M1
n = 100
x1 = rnorm(n, 20,3)
x2 = rnorm(n,mean = x1, sd = 1)
y = 40 + 1.2*x1 + rnorm(n,0,3)

df = data.frame(y = y, x1 = x1, x2 = x2)

ggplot(data =df) +
  geom_point(aes(x = x1, y = y)) + 
  geom_point(aes(x = x2, y = y), color = "red")

cor(y,x1)
cor(y,x2)
bb = lm(y ~ x2, df)
summary(bb)

cc = lm(y ~ x1 + x2, data = df)
summary(cc)

# So, in this model, x1 predicts y  when 
# x2  is already known, but x2 seen does not predict 
# y when x1  is already known. Thus, the 
# bivariate association between y and x2 is spurious.

# 5M2
N <- 1000
rho <- 0.8
set.seed(11)
x <- rnorm(N, 1) # alcohol
y <- rnorm(N, rho*x , sqrt(1-rho^2) ) # illness
z <- rnorm(N, x - y, 0.5) # happiness
d <- data.frame(x=x, y=y, z=z)
pairs(d)
summary(lm(z~y, d))
summary(lm(z~x, d))
summary(lm(z~x+y, d))
 
# Indeed, the slopes for alcohol and illness became much larger in magnitude in the multivariate
# model. Thus, in this example (and maybe not in real life), because alcohol increases happiness
# and feelings of illness, and feelings of illness decrease happiness, the bivariate 
# relationships of alcohol and feelings of illness to happiness are masked.


# 5H1
library(rethinking)
d = data(foxes)
d = foxes
summary(d)

# using area
mod1 = map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b*area,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,40)
  ),
  data = d
)

summary(mod1)

area.seq = seq(from=0, to = 4, by = 0.1)
mu = rethinking::link(mod1, data = data.frame(area = area.seq))
mu.mean = apply(mu, 2, mean)
mu.HPDI = apply(mu, 2, HPDI, prob = 0.89)

plot( weight ~ area , data=d , col=col.alpha(rangi2,0.5) )
lines( area.seq , mu.mean )
shade( mu.HPDI , area.seq )

# using groupsize
mod2 = map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b*groupsize,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,40)
  ),
  data = d
)

summary(mod2)

group.seq = seq(from=0, to = 9, by = 0.1)
mu = rethinking::link(mod2, data = data.frame(groupsize = group.seq))
mu.mean = apply(mu, 2, mean)
mu.HPDI = apply(mu, 2, HPDI, prob = 0.89)

plot( weight ~ groupsize , data=d , col=col.alpha(rangi2,0.5) )
lines( group.seq , mu.mean )
shade( mu.HPDI , group.seq )

precis(mod2)

# Based on only the bivariate regressions, it appears that neither territory area nor group size
# are very important for the prediction of body weight. Group size seemed like it may have a 
# slightly negative relationship with body weight, but nothing too precise.

# 5H2
# using area AND groupsize

mod3 = map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba*area +  bg*groupsize,
    a ~ dnorm(0,100),
    ba ~ dnorm(0,10),
    bg ~ dnorm(0,10),
    sigma ~ dunif(0,40)
  ),
  data = d
)

summary(mod3)
# both seem distinct than 0 

# area having groupsize cosntant
area.seq = seq(from=0, to = 4, by = 0.1)
mu = rethinking::link(mod3, data = data.frame(area = area.seq,
                                              groupsize = mean(d$groupsize)))
mu.mean = apply(mu, 2, mean)
mu.HPDI = apply(mu, 2, HPDI, prob = 0.89)

plot( weight ~ area , data=d , col=col.alpha(rangi2,0.5) )
lines( area.seq , mu.mean )
shade( mu.HPDI , area.seq )

# groupsize having area cosntant
group.seq = seq(from=0, to = 9, by = 0.1)
mu = rethinking::link(mod3, data = data.frame(groupsize = group.seq,
                                              area = mean(d$area)))
mu.mean = apply(mu, 2, mean)
mu.HPDI = apply(mu, 2, HPDI, prob = 0.89)

plot( weight ~ groupsize , data=d , col=col.alpha(rangi2,0.5) )
lines( group.seq , mu.mean )
shade( mu.HPDI , group.seq )


# The multiple regression model tells a different story than the bivariate models. It says that 
# territory area is positively related to body weight and that group size is negatively related
# to body weight. The results differ because this is an example of a masking relationship. 
# Territory area is positively related to body weight and group size is negatively related to
# body weight, but these effects get cancelled out in the bivariate regressions because territory
# area and group size are positively related.

# 5H3
# using avgfood AND groupsize

mod4 = map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + bf*avgfood +  bg*groupsize,
    a ~ dnorm(0,100),
    bf ~ dnorm(0,10),
    bg ~ dnorm(0,10),
    sigma ~ dunif(0,40)
  ),
  data = d
)

summary(mod4)

# using area, avgfood and groupsize

mod5 = map(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + ba*area +  bf*avgfood +  bg*groupsize,
    a ~ dnorm(0,100),
    ba ~dnorm(0,10),
    bf ~ dnorm(0,10),
    bg ~ dnorm(0,10),
    sigma ~ dunif(0,40)
  ),
  data = d
)

summary(mod5)


# This model is similar to the previous one in terms of the group size slope, so we have again 
# addressed the masking relationship problem. However, the estimates of the average food and
# territory area slopes have decreased in magnitude compared to previous models. This is likely 
# due to multicollinearity between the two variables.