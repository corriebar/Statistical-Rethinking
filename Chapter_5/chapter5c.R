# Chapter 5 - When adding variables hurt
library(rethinking)
# Multicollinear legs
# Predicting ones person height using leg length
N <- 100                        # number of individuals
height <- rnorm(N, 10, 2)       # sim total height of each
leg_prop <- runif(N, 0.4, 0.5)  # leg as proportion of height (between 40%-50%)
leg_left <- leg_prop*height + rnorm(N, 0, 0.02)
leg_right <- leg_prop*height + rnorm(N, 0, 0.02)

d <- data.frame(height, leg_left, leg_right)

# fit simulated data
m5.8<- map(
  alist(
    height ~ dnorm( mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm( 2, 10),
    sigma ~ dunif(0, 10)
  ), data=d
)
precis(m5.8)
plot( precis( m5.8))

post <- extract.samples( m5.8 )
plot( bl ~ br, post, col=col.alpha(rangi2, 0.1), pch=16)
# the posterior distribution for these two parameters is highly correlated
# see also:
precis( m5.8, corr = TRUE)


# lets have a look at the sum instead
sum_blbr <- post$bl + post$br
dens( sum_blbr, col=rangi2, lwd=2, xlab="sum of bl and br")
# the standard deviation for the sum is much smaller

m5.9 <- map(
  alist(
    height ~ dnorm( mu, sigma),
    mu <- a + bl*leg_left,
    a ~ dnorm(10, 100),
    bl ~ dnorm( 2, 10),
    sigma ~ dunif(0, 10)
  ), data=d
)
precis(m5.9)
plot(precis(m5.9))


# Multicollinear milk
data(milk)
d <- milk
# two variables: perc.fat and perc.lactose that we might use to model the energy contet
# we first fit two bivariate models
# kcal.per.g regressed on perc.fat
m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm( mu, sigma),
    mu <- a + bf*perc.fat,
    a ~ dnorm( 0.6, 10),
    bf ~ dnorm( 0, 1),
    sigma ~ dunif( 0, 10)
  ), data=d
)

# kcal.per.g regressed on perc.lactose
m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm( mu, sigma),
    mu <- a + bl*perc.lactose,
    a ~ dnorm( 0.6, 10),
    bl ~ dnorm( 0, 1),
    sigma ~ dunif( 0, 10 )
  ), data=d
)

precis( m5.10, digits=3)
precis( m5.11, digits=3)
par(mfrow=c(2,1))
plot( precis( m5.10))
plot( precis(m5.11))
par(mfrow=c(1,1))   #back to default

# both predictors in one model
m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm( mu, sigma ),
    mu <- a + bl*perc.lactose + bf*perc.fat,
    a ~ dnorm( 0.6, 10),
    bf ~ dnorm( 0, 1),
    bl ~ dnorm( 0, 1 ),
    sigma ~ dunif( 0, 10 )
  ), data=d
)
precis( m5.12, digits=3)
plot( precis( m5.12, digits=3))
pairs( ~ kcal.per.g + perc.fat + perc.lactose, data=d, col=rangi2)

cor( d$perc.fat, d$perc.lactose)

# Experiment: simulate variable correlated with perc.fat
sim.coll <- function( r=0.9){
  d$x <- rnorm(nrow(d), mean=r*d$perc.fat,
               sd=sqrt( ( 1-r^2)*var(d$perc.fat) ) )
  m <- lm(kcal.per.g ~ perc.fat + x, data=d)
  sqrt( diag( vcov(m) ) )[2]   # stdev of parameter
}
rep.sim.coll <- function( r=0.9, n=100){
  stddev <- replicate(n, sim.coll(r) )
  mean(stddev)                        # compute average standard deviation for 100 regressions
}
r.seq <- seq(from=0, to=0.99, by=0.01)
stddev <- sapply(r.seq, function(z) rep.sim.coll(r=z, n=100))   # takes some time
plot( stddev ~ r.seq, type="l", col=rangi2, lwd=2, xlab="correlation")



# Post-treatmet bias
# Plant example
N <- 100

# simulate initial heights
h0 <- rnorm(N, 10, 2)

# assign treatments and simulate fungus and growth
treatment <- rep(0:1, each=N/2)       # 50/50 treatment, no treatment
fungus <- rbinom( N, size=1, prob=0.5 - treatment*0.4)
h1 <- h0 + rnorm(N, 5-3*fungus)

# compose a clean data frame
d <- data.frame(h0=h0, h1=h1, treatment=treatment, fungus=fungus)

# fit model including all of the available variables
m5.13 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh*h0 + bt*treatment + bf*fungus,
    a ~ dnorm(0, 100),
    c(bh, bt, bf) ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data=d
)
precis(m5.13)
plot( precis(m5.13 ))

# omit fungus
m5.14 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh*h0 + bt*treatment,
    a ~ dnorm(0, 100),
    c(bh, bt) ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data=d
)
precis(m5.14)
plot(precis(m5.14))
