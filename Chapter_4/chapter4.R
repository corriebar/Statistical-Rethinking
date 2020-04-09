library(rethinking)
# how normality can arise from ultiplication
growth <- replicate(1000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = T)

# the smaller the effect of each locus, the better the additive approximation s
big <- replicate(1000, prod(1 + runif(12, 0, 0.5)))
dens(big, norm.comp = T)

small <- replicate(1000, prod(1 + runif(12, 0, 0.0001)))
dens(small, norm.comp = T)

# normal by log-multiplication
log.big <- replicate(1000, log( prod( 1 + runif(12, 0, 0.05))))
dens(log.big, norm.comp = T)

# The !Kung Data
data("Howell1")
d <- Howell1
str(d )

hist(d$height)

# filter out only adult people
d2 <- d[ d$age >= 18 ,]
str(d2)

hist(d2$height)
dens(d2$height)
# looks kinda Gaussian

# plot our priors
curve( dnorm( x, 178, 20 ), from=100, to=250) # prior for mean
curve( dunif( x, 0, 50), from=-10, to=60) # prior for variance

# sample from the prior
sample_mu <- rnorm(1e4, mean=178, sd=20)
sample_sigma <- runif(1e4, 0, 50)
prior_height <- rnorm(1e4, mean=sample_mu, sd=sample_sigma)
dens(sample_height)


mu.list <- seq(from=140, to=160, length.out = 200)
sigma.list <- seq(from=4, to=9, length.out = 200)
post <- expand.grid(mu=mu.list, sigma=sigma.list) # make grid over each value combination
post$LL <- sapply( 1:nrow(post), function(i) sum( dnorm(  # for each possible parameter combination, 
    d2$height,                                            # compute for all observed height values
    mean=post$mu[i],                                      # the probability density (that is the likelihood for this paramter combination)  
    sd=post$sigma[i],                                     # we use log, so that we can multiply the likelihood by using addiing
    log=TRUE                                              # the likelihood values
)))
post$prod <- post$LL + dnorm( post$mu, 178, 20, TRUE) +   # add log likelihood value to log prior values (corresponds to multiplying)
  dunif( post$sigma, 0, 50, TRUE)
post$prob <- exp( post$prod - max(post$prod))     # divide by the maximum value to avoid too small values
                                                  # result is thus not proper probabilites but relative probabilities
contour_xyz( post$mu, post$sigma, post$prob)
# highest probability for mu is somewhere around 155 with highest prob for sigma around 8
image_xyz( post$mu, post$sigma, post$prob)

# sample from posterior
sample.rows <- sample( 1:nrow(post), size=1e4, replace=TRUE,
                       prob=post$prob)
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows]

# now have 10,000 samples, with replacement, from the posterior for the height data
plot(sample.mu, sample.sigma, cex=0.5, pch=16, col=col.alpha(rangi2, 0.1))

# shapes of the marginal posterior
dens( sample.mu )
dens( sample.sigma )

# summarize widths of these densities with highest posterior density interval
HPDI( sample.mu )
HPDI( sample.sigma)


# repeat example with smaller sample
d3 <- sample( d2$height, size=20)

mu.list <- seq(from=140, to=160, length.out = 200)
sigma.list <- seq(from=4, to=9, length.out = 200)
post2 <- expand.grid(mu=mu.list, sigma=sigma.list) # make grid over each value combination
post2$LL <- sapply( 1:nrow(post2), function(i) sum( dnorm(  # for each possible parameter combination, 
  d3,                                            # compute for all observed height values
  mean=post2$mu[i],                                      # the probability density (that is the likelihood for this paramter combination)  
  sd=post2$sigma[i],                                     # we use log, so that we can multiply the likelihood by using addiing
  log=TRUE                                              # the likelihood values
)))
post2$prod <- post2$LL + dnorm( post2$mu, 178, 20, TRUE) +   # add log likelihood value to log prior values (corresponds to multiplying)
  dunif( post2$sigma, 0, 50, TRUE)
post2$prob <- exp( post2$prod - max(post2$prod))     # divide by the maximum value to avoid too small values
# result is thus not proper probabilites but relative probabilities
contour_xyz( post2$mu, post2$sigma, post2$prob)
# highest probability for mu is somewhere around 155 with highest prob for sigma around 8
image_xyz( post2$mu, post2$sigma, post2$prob)

# sample from posterior
sample2.rows <- sample( 1:nrow(post2), size=1e4, replace=TRUE,
                       prob=post2$prob)
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows]

plot( sample2.mu, sample2.sigma, cex=0.5, col=col.alpha(rangi2, 0.1), xlab='mu', ylab='sigma', pch=16)
# sigma spreads quite different, compared to before
dens( sample2.sigma, norm.comp = TRUE)
# posterior for sigma is not Gaussian, but rather has a long tail of uncertaintinty towards higher values




# Same Model again, this time using MAP, maximum a posteriori
# First, define the model (flist = formula list)
flist <- alist(
  height ~ dnorm( mu, sigma) ,
  mu ~ dnorm( 178, 20),
  sigma ~ dunif(0, 50)
)

m4.1 <- map( flist, data=d2 )
precis( m4.1 )

# compare with model before:
HPDI( sample.mu )
HPDI( sample.sigma )
# pretty much the same


# lets compare with using a more informative prior
m4.2 <- map( 
          alist(
            height ~ dnorm(mu, sigma),
            mu ~ dnorm(178, 0.1),
            sigma ~ dunif(0, 50)
          ), 
          data=d2)
precis(m4.2)
# now the estimate for mu has hardly moved off the prior. the prior as very concentrated around 178
# also, the estimate for sigma has changed quite a lot even though we didn't change its prior at all

# To sample from the quadratic approximation, note that the quadratic approximation
# of a posterior distribution is just a multi-dimensional Gaussian distribution.
# To sufficiently describe a multidimensional Gaussian distribution, we only need 
# a list of means and a matrix of variances and covariances
vcov( m4.1 )    # variance-covariance matrix
diag( vcov(m4.1 ))  # list of variances, if you take the square-root of this vector, you get the standard deviation
                    # as shown in precis
cov2cor( vcov( m4.1 )) # correlation matrix

# to get samples, we sample vectors of values from a multi-dimensional Gaussian distribution:
post <- extract.samples( m4.1, n=1e4)
head(post)
precis(post)
# these samples also preserve the covariance between mu and sigma

# equivalent to the function extract samples:
library(MASS)
post <- mvrnorm(n = 1e4, mu=coef(m4.1), Sigma=vcov(m4.1))

# Sometimes better to estimate log(sigma):
m4.1_logsigma <- map(
                  alist(
                    height ~ dnorm(mu, exp(log_sigma) ),
                    mu ~ dnorm(178, 20),
                    log_sigma ~ dnorm(2, 10)
                  ), data=d2 )
post <- extract.samples(m4.1_logsigma)
sigma <- exp(post$log_sigma)
