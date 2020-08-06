# Chapter 2
# Grid approximation (pg 40) for the Globe Tossing Example.

# define grid
p_grid = seq(from = 0, to = 1, length.out = 20)

# define prior
prior = rep(1,20) # uniform
prior = ifelse(p_grid < 0.5, 0, 1) # step prior
prior = exp(-5*abs(p_grid - 0.5)) # peak prior

# compute likelihood at each value in grid
likelihood = dbinom(6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior = likelihood * prior

# standardize the posterior, so it sums to 1
posterior = unstd.posterior / sum(unstd.posterior)

plot(p_grid, posterior, type = "b",
     xlab = "probability of water", ylab = "posterior probability")
mtext("20 points")


# Quadratic Approximation
globe.qa = rethinking::map(
    alist(
        w ~ dbinom(9,p) , # binomial likelihood
        p ~ dunif(0,1)    # uniform prior
    ),
    data = list(w=6))

# display summary of quadratic approximation
rethinking::precis(globe.qa)
