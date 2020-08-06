# Chapter 3
# Sampling from a grid-approximate posterior

# Posterior distribution
p_grid = seq(0,1, length.out = 1000)
prior = rep(1,1000)
likelihood = dbinom(6,9, prob = p_grid)
posterior = likelihood * prior
posterior = posterior/sum(posterior)


# 10000 samples from the posterior distribution
samples = sample(p_grid, prob = posterior, size = 10000, replace = TRUE)

plot(samples)

rethinking::dens(samples)

# how much mass between 0.5 and 0.75?
sum(samples > 0.5 & samples < 0.75) / 10000

# lower 80% of the distribution
quantile(samples, 0.8)

# Diferencia entre 50% confidence interval y 50 HDPI (highest posterior density interval)
p_grid = seq(0,1, length.out = 1000)
prior = rep(1,1000)
likelihood = dbinom(3,3, prob = p_grid)
posterior = likelihood * prior
posterior = posterior/sum(posterior)
samples = sample(p_grid, prob = posterior, size = 10000, replace = TRUE)

rethinking::PI(samples, prob = 0.5) # 0.70;0.93
rethinking::HPDI(samples, prob = 0.5) # 0.84;1

0
