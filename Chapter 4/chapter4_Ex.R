# Chapter 4 Exercises
library(rethinking)
# Medium
# 4M1 For the model definition below, simulate observed heights from the prior
n <- 10000
mu <- rnorm(n, 0, 10)
sigma <- runif(n, 0, 10)
y_prior <- rnorm(n, mu, sigma)
dens(y_prior)
# values are distributed with mean 0 and sd ~ 10

# 4M2 Translate the model above into a map formula
flist <- alist(
            y ~ dnorm(mu, sigma),
            mu ~ dnorm(0, 10),
            sigma ~ dunif(0,10)
          ) 

# 4M3