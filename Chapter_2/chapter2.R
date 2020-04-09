# Chapter 2, Small Worlds and Large Worlds
# 1. Globe tossing model
# grid approximation of the posterior

# define grid
num_pts = 20
p_grid = seq(from=0, to=1, length.out=num_pts)

# define prior (uniform)
prior <- rep(1, num_pts)

# compute likelihood in grid
w = 6
n = 9
likelihood <- dbinom(w, size=n, prob=p_grid)




compute_posterior <- function(w, n, prior) {
  num_pts = length(prior)
  p_grid = seq(from=0, to=1, length.out=num_pts)
  likelihood <- dbinom(w, size=n, prob=p_grid)
  
  # compute product of likelihood and prior (unstandardized posterior)
  unstd.posterior <- likelihood * prior
  
  # standardize posterior
  posterior <- unstd.posterior / sum(unstd.posterior)
  
  plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
  mtext(paste(num_pts, "points"))
}

uniform_prior <- rep(1, num_pts)
par(mfrow=c(1,3))
# a 
compute_posterior(w=3, n=3, prior=uniform_prior)

# b
compute_posterior(w=3, n=4, prior=uniform_prior)

# c
compute_posterior(w=5, n=7, prior=uniform_prior)

### step prior
step_prior <- ifelse(p_grid < 0.5, 0, 2)
par(mfrow=c(1,3))
# a 
compute_posterior(w=3, n=3, prior=step_prior)

# b
compute_posterior(w=3, n=4, prior=step_prior)

# c
compute_posterior(w=5, n=7, prior=step_prior)


par(mfrow=c(1,3))
num_pts=20
uniform_prior <- rep(1, num_pts)
compute_posterior(w=3, n=3, prior=uniform_prior)
num_pts=200
uniform_prior <- rep(1, num_pts)
compute_posterior(w=3, n=3, prior=uniform_prior)
num_pts=2000
uniform_prior <- rep(1, num_pts)
compute_posterior(w=3, n=3, prior=uniform_prior)


compute_posterior2 <- function(w, n, prior) {
  num_pts = length(prior)
  p_grid = seq(from=0, to=1, length.out=num_pts)
  likelihood <- dbinom(w, size=n, prob=p_grid)
  
  # compute product of likelihood and prior (unstandardized posterior)
  unstd.posterior <- likelihood * prior
  
  # standardize posterior
  posterior <- num_pts * unstd.posterior / sum(unstd.posterior)
  
  plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
  mtext(paste(num_pts, "points"))
}

par(mfrow=c(1,3))
num_pts=20
uniform_prior <- rep(1, num_pts)
compute_posterior2(w=3, n=3, prior=uniform_prior)
num_pts=200
uniform_prior <- rep(1, num_pts)
compute_posterior2(w=3, n=3, prior=uniform_prior)
num_pts=2000
uniform_prior <- rep(1, num_pts)
compute_posterior2(w=3, n=3, prior=uniform_prior)
