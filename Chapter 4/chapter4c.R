# Chapter 4.c Polynomial Regression
library(rethinking)
data("Howell1")
d <- Howell1
str(d)

plot(height ~ weight, data=d)
# not very linear

# Standardize
d$weight.s <- ( d$weight - mean(d$weight) ) /sd(d$weight)
mean(d$weight.s)
sd(d$weight.s)

d$weight.s2 <- d$weight.s ^2

m4.5 <- map(
          alist(
            height ~ dnorm(mu, sigma) ,
            mu <- a + b1*weight.s + b2*weight.s2 ,
            a ~ dnorm(178, 100) ,
            b1 ~ dnorm(0,10),
            b2 ~ dnorm(0,10),
            sigma ~ dunif(0, 50)
          ), data=d
)

precis(m4.5)
# harder to interpret, eg. alpha is not the mean of the height anymore
mean(d$height)


# plot
weight.seq <- seq(from=-2.2, to=2, length.out=30)
pred_data <- list(weight.s=weight.seq, weight.s2=weight.seq^2)
mu <- link(m4.5, data=pred_data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(m4.5, data=pred_data)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

plot(height ~ weight.s, d, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)


# cubed
d$weight.s3 <- d$weight.s^3
m4.6 <- map(
        alist(
          height ~ dnorm(mu, sigma),
          mu <- a + b1*weight.s + b2*weight.s2 + b3*weight.s3,
          a ~ dnorm(178,100),
          b1 ~ dnorm(0,10),
          b2 ~ dnorm(0,10),
          b3 ~ dnorm(0,10),
          sigma ~ dunif(0,50)
        ), data=d
)
precis(m4.6)

# plot
pred_data <- list(weight.s=weight.seq, weight.s2=weight.seq^2, weight.s3=weight.seq^3)
mu <- link(m4.6, data=pred_data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)
sim.height <- sim(m4.6, data=pred_data)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

plot(height ~ weight.s, d, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)
