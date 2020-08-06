# Chapter 3. Exercises
library(tidyverse)
# 3E ----
p_grid = seq(from = 0, to = 1, length.out = 1000)
prior = rep(1,1000)
likelihood = dbinom(6, size = 9, prob = p_grid)
posterior = likelihood * prior
posterior = posterior / sum(posterior)
set.seed(100)
samples = sample(p_grid, prob = posterior, size = 1000, replace = TRUE)

# 3E1
sum(samples < 0.2) / 1000
#3E2
sum(samples > 0.8) / 1000
#3E3
sum(samples > 0.2 & samples < 0.8)/1000
# 3e4 3e5
quantile(samples, c(0.2, 0.8))
#3E6
rethinking::HPDI(samples, prob = 0.66)
#3E7
quantile(samples, c(0.17, 0.83))

###

# 3M1 ----
p_grid = seq(from = 0, to = 1, length.out = 1000)
prior = rep(1,1000)
likelihood = dbinom(8, size = 15, prob = p_grid)
posterior = likelihood * prior
plot(p_grid, posterior, type='l')
# 3M2
samples = sample(p_grid, prob = posterior, size = 10000, replace = TRUE)
rethinking::HPDI(samples, 0.9)
# 3M3
sim = rbinom(1e5, size = 15, prob = samples)
sum(sim == 8) / 1e5
# 3M4
sim2 = rbinom(1e5, size = 9, prob = samples)
sum(sim2 == 6) / 1e5

# 3M5
p_grid = seq(from = 0, to = 1, length.out = 1000)
prior = ifelse(p_grid < 0.5,0,1)
likelihood = dbinom(8, size = 15, prob = p_grid)
posterior = likelihood * prior
posterior = posterior / sum(posterior)
plot(p_grid, posterior, type='l')

samples = sample(p_grid, prob = posterior, size = 10000, replace = TRUE)
rethinking::HPDI(samples, 0.9)

sim = rbinom(1e5, size = 15, prob = samples)
sum(sim == 8) / 1e5

sim2 = rbinom(1e5, size = 9, prob = samples)
sum(sim2 == 6) / 1e5

####

# 3H1 ----
rm(list = ls())
library(rethinking)
data(homeworkch3)
n_boys = sum(birth1) + sum(birth2)

p_grid = seq(from = 0, to = 1, length.out = 1000)
prior = rep(1,1000)
likelihood = dbinom(n_boys, size = 200, prob = p_grid)
posterior = likelihood * prior
posterior = posterior / sum(posterior)
plot(p_grid, posterior, type='l')
p_grid[which.max(posterior)]

# 3H2
samples = sample(p_grid,prob = posterior, size = 1e5, replace = TRUE)
HPDI(samples, prob = c(0.5,0.89,0.97))

# 3H3
sim = rbinom(1e5, 200, prob = samples)

simplehist(sim)
mean(sim) #110.9
median(sim) #111
dens(sim)
abline(v = n_boys, col = "red")

# 3H4
sim2 = rbinom(1e5, 100, prob = samples)
mean(sim2)
dens(sim2)
abline(v = sum(birth1), col = "red")

# 3H5
birth_after_female = birth2[birth1 == 0] # 49 births after having a female in first birth
sum(birth_after_female) # 39 were male

simh5 = rbinom(1e5, size = length(birth_after_female), prob = samples)
mean(simh5)
dens(simh5)
abline(v = sum(birth_after_female), col = "red")

# model underestimates number of boys for the second child after the first girl

# conclusion - gender of the second child is not independent from the first one
