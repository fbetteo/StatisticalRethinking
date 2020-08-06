# Chapter 2. Exercises
library(tidyverse)

# 2M1 ----

## 1 
grid_p = seq(from = 0, to = 1, length.out = 20)
prior  = rep(1,20)

likelihood = dbinom(3,3, grid_p)

unstd.posterior = likelihood*prior
posterior       = unstd.posterior/sum(unstd.posterior)

ggplot() + 
  geom_line(aes( x = grid_p, y = posterior))

## 2
likelihood = dbinom(3,4, grid_p)

unstd.posterior = likelihood*prior
posterior       = unstd.posterior/sum(unstd.posterior)

ggplot() + 
  geom_line(aes( x = grid_p, y = posterior))

## 3
likelihood = dbinom(5,7, grid_p)

unstd.posterior = likelihood*prior
posterior       = unstd.posterior/sum(unstd.posterior)

ggplot() + 
  geom_line(aes( x = grid_p, y = posterior))

# 2M2 ----

prior = ifelse(grid_p < 0.5, 0,1)
likelihood = dbinom(3,3, grid_p)

unstd.posterior = likelihood*prior
posterior       = unstd.posterior/sum(unstd.posterior)

ggplot() + 
  geom_line(aes( x = grid_p, y = posterior))

likelihood = dbinom(3,4, grid_p)

unstd.posterior = likelihood*prior
posterior       = unstd.posterior/sum(unstd.posterior)

ggplot() + 
  geom_line(aes( x = grid_p, y = posterior))

likelihood = dbinom(5,7, grid_p)

unstd.posterior = likelihood*prior
posterior       = unstd.posterior/sum(unstd.posterior)

ggplot() + 
  geom_line(aes( x = grid_p, y = posterior))

# 2M3 ----

0.3*0.5 + 1*0.5
0.3*0.5/0.65



# Resto Medium en libro ----

## Hard ----
# 2H1 ----
# p(twins) = p(species=A)*p(twins|A)+p(species=B)*p(twins|B)
# before first breeding P(species=A)=p(species=B)=0.5
# to get answer for the second round we need to calculate new p(A) and p(B)
# it's posterior of first breeding that becomes prior for the second one
# p(tweens_2|tweens_1) = p(species=A|tweens_1)*p(twins|A)+p(species=B|tweens_1)*p(twins|B)
p_twins_A <- 0.1
p_twins_B <- 0.2
likelihood <- c(p_twins_A, p_twins_B)
prior <- c(1, 1)
posterior <- prior * likelihood
posterior <- posterior/sum(posterior)

# result
sum(posterior*likelihood)


# 2H2 ----
p_twins_A <- 0.1 # likelihood
p_A = 1/2 # prior
p_twins = 0.1*1/2 + 0.2*1/2
p_A_twins = p_twins_A*p_A/p_twins # posterior

# 2H3----
# we update the posterior
unstd_posterior_A = p_A_twins * 0.9 # posterior updated por prob de single
unstd_posterior_B = (1 - p_A_twins) * 0.8
posterior_A = unstd_posterior_A/(unstd_posterior_A + unstd_posterior_B)

# prolijo por otra persona
# 2H3
p_twins_A <- 0.1
p_twins_B <- 0.2
# first Bayesian update
likelihood_twins <- c(p_twins_A, p_twins_B)
prior <- c(1, 1)
posterior <- prior * likelihood_twins
posterior <- posterior/sum(posterior)

# second Bayesian update
likelihood_single <- c(1-p_twins_A, 1-p_twins_B)
prior <- posterior
posterior <- prior * likelihood_single
posterior <- posterior/sum(posterior)

posterior[1] #0.36


# 2H4----
#p1
likelihood_test <- c(0.8, 1-0.65)
prior <- c(1, 1)
posterior_vet <- prior * likelihood_test
posterior_vet <- posterior_vet/sum(posterior_vet)

posterior_vet[1] #0.6956522
#p2
p_twins_A <- 0.1
p_twins_B <- 0.2
likelihood_twins <- c(p_twins_A, p_twins_B)
prior <- posterior_vet
posterior <- prior * likelihood_twins
posterior <- posterior/sum(posterior)

posterior[1] #0.5333333
