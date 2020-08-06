# Chapter 6
# Overfitting, Regularization and Information Criteria
library(rethinking)

# pg 178
# Entropy
p = c(0.3, 0.7)
-sum(p*log(p))

#pg 182
# deviance 
d = iris

m6.1 = lm(Sepal.Length ~ Petal.Length, d)
# con logLik
deviance_lglk = (-2) * logLik(m6.1)  # suma de log probabilities
# a mano
deviance_mano = (-2) *sum(dnorm(d$Sepal.Length,
                mean = m6.1$coefficients[1] + m6.1$coefficients[2]*d$Petal.Length,
                sd = sd(m6.1$residuals),
          log = TRUE))

# con Bayesian
m6.8 = map(
  alist(
    Sepal.Length ~ dnorm(mu, sigma),
    mu <- a + b*Petal.Length,
    a ~ dnorm(0,100),
    b ~ dnorm(0,100),
    sigma ~ dunif(0,20)
  ),
  data = d
)

# extract MAP estimates
theta = coef(m6.8)

# compute deviance
dev = (-2)*sum(dnorm(d$Sepal.Length, 
                     mean = theta[1] + theta[2]*d$Petal.Length,
                     sd = theta[3],
               log = TRUE))


# WAIC
data(cars)
m = map(
  alist(
    dist ~dnorm(mu, sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,30)
  ),
  data = cars
)
post = extract.samples(m, n = 1000)

n_samples = 1000
ll = sapply(1:n_samples,
            function(s){
              mu = post$a[s] + post$b[2]*cars$speed
              dnorm(cars$dist, mu, post$sigma[2], log = TRUE)
            })

n_cases = nrow(cars)
lppd = sapply(1:n_cases, function(i) log_sum_exp(ll[i,]) - log(n_samples))

pwaic = sapply(1:n_cases, function(i) var(ll[i,]))

-2*(sum(lppd) - sum(pwaic))

waic_vec = -2 * (lppd-pwaic)
sqrt(n_cases * var(waic_vec))
