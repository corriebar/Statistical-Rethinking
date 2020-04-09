# Chapter 5 - Masked relationship
library(rethinking)
data(milk)
d <- milk
str(d)


# for now, we only consider 
#      kcal.per.g = Kilocalories of energy per gram of milk
#      mass = Average female body mass, in kilograms
#      neocortex.perc = the percent of total brain mass that is neocortex mass
pairs(~kcal.per.g + log(mass) + neocortex.perc, data=d)

# first model: bivariate regression between kilocalories and neocortex percent
# but first remove NA values
dcc <- d[ complete.cases(d), ]

m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm( mu, sigma) ,
    mu <- a + bn*neocortex.perc,
    a ~ dnorm( 0, 100),
    bn ~ dnorm( 0, 1) ,
    sigma ~ dunif( 0, 1)
  ), data=dcc
)

precis ( m5.5, digits=3)
plot( precis( m5.5))

# bn is very small: a change from the smallest neocortex percent in the data (55%)
# to the largest (76%) results in an expected change of only:
coef(m5.5)["bn"] *( 76 -55)

# plot predicted mean and 89% interva for the mean
np.seq <- 0:100
pred.data <- data.frame( neocortex.perc=np.seq)

mu <- link(m5.5, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data=dcc, col=rangi2 )
lines( np.seq, mu.mean )
lines( np.seq, mu.PI[1, ], lty=2 )
lines( np.seq, mu.PI[2, ], lty=2 )


# same for log.mass
dcc$log.mass <- log(dcc$mass)

# fit the model
m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm( mu, sigma) ,
    mu <- a + bm*log.mass,
    a ~ dnorm(0, 100),
    bm ~ dnorm(0, 1),
    sigma ~ dunif( 0, 1)
  ), data=dcc
)
precis(m5.6)
plot( precis(m5.6) )

lm.seq <- seq(from=-2.5, to=4.7, length.out = 30)
pred.data <- data.frame( log.mass=lm.seq)

mu <- link(m5.6, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data=dcc, col=rangi2)
lines(lm.seq, mu.mean)
lines( lm.seq, mu.PI[1,], lty=2)
lines( lm.seq, mu.PI[2,], lty=2)


# fit model with both predictors together
m5.7 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma) ,
    mu <- a + bn*neocortex.perc + bm*log.mass ,
    a ~ dnorm( 0, 100),
    bn ~ dnorm(0, 1), 
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data=dcc
)

precis(m5.7)
plot( precis(m5.7))
# quite large stdev for intercept a
# compare with the single bivariate models
precis(m5.6)
precis(m5.5, digits=3)
# mass now has less of an influence, while bn has a stronger influence

# counterfactual plots
# vary neocortex.perc, keep mass fixed
mean.log.mass <- mean(log(dcc$mass))
np.seq <- 0:100
pred.data <- data.frame(
  neocortex.perc=np.seq,
  log.mass=mean.log.mass
)

mu <- link( m5.7, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply( mu, 2, PI )

plot(kcal.per.g ~ neocortex.perc, data=dcc, type="n")
lines( np.seq, mu.mean)
lines( np.seq, mu.PI[1,], lty=2)
lines( np.seq, mu.PI[2,], lty=2)

# vary mass, keep neocortex.perc fixed
mean.neocortex.perc <- mean(dcc$neocortex.perc)
lm.seq <- seq(from=-2.5, to=4.7, length.out = 30)
pred.data <- data.frame(
  neocortex.perc=mean.neocortex.perc,
  log.mass=lm.seq
)

mu <- link( m5.7, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data=dcc, type="n")
lines( lm.seq, mu.mean )
lines( lm.seq, mu.PI[1,], lty=2)
lines( lm.seq, mu.PI[2,], lty=2)

# counterfactual plots show a strong association with both variables (but in opposite direction!)



# synthetic masked association
N <- 100
rho <- 0.7                           # correlation between x_pos and x_neg
x_pos <- rnorm( N )                  # x_pos as Gaussian
x_neg <- rnorm( N, rho*x_pos,        # x_neg correlated with x_pos
                sqrt(1-rho^2) )
y <- rnorm( N, x_pos - x_neg)        # y equally associated with x_pos, x_eg
d <- data.frame( y, x_pos, x_neg )
pairs(d)

# bivariate models
m5.8 <- map(
  alist(
    y ~ dnorm( mu, sigma) ,
    mu <- a + bp*x_pos ,
    a ~ dnorm( 0, 10),
    bp ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data=d
)
plot( precis(m5.8))

m5.9 <- map(
  alist(
    y ~ dnorm( mu, sigma) ,
    mu <- a +  bn*x_neg,
    a ~ dnorm( 0, 10),
    bn ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data=d
)
plot( precis( m5.9))

# multivariate model
m5.10 <- map(
  alist(
    y ~ dnorm( mu, sigma) ,
    mu <- a + bp*x_pos + bn*x_neg,
    a ~ dnorm( 0, 10),
    bp ~ dnorm(0, 10),
    bn ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data=d
)

par(mfrow=c(3,1))
plot( precis(m5.10), main="Multivariate Regression")
plot( precis(m5.9), main="Bivariate Regression, x_pos")
plot( precis(m5.9), main="Bivariate Regression x_neg")

