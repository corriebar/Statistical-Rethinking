# Chapter 5 - Categorical variables
library(rethinking)
# binary categories
data("Howell1")
d <- Howell1
str(d)

# fit model using the variable male as predictor
m5.15 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm*male,
    a ~ dnorm(178, 100),
    bm ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data=d
)
precis(m5.15)
plot(precis(m5.15))
# to get average heigt of males, use sample:
post <- extract.samples(m5.15)
mu.male <- post$a + post$bm
PI(mu.male)

m5.15b <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- af*(1-male) + am*male,
    af ~ dnorm( 178, 100),
    am ~ dnorm( 178, 100),
    sigma ~ dunif(0, 50)
  ), data=d
)
precis(m5.15b)
plot(precis(m5.15b))


# Many categories
data(milk)
d <- milk
unique(d$clade)

# create a dummy variable for new world monkey
( d$clade.NWM <- ifelse( d$clade == "New World Monkey", 1, 0) )
# more dummy variables
d$clade.OWM <- ifelse( d$clade == "Old World Monkey", 1, 0 )
d$clade.S <- ifelse( d$clade == "Strepsirrhine", 0, 1 )


m5.16 <- map(
  alist(
    kcal.per.g ~ dnorm( mu, sigma),
    mu <- a + b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S,
    a ~ dnorm( 0.6, 10),
    c(b.NWM, b.OWM, b.S) ~ dnorm(0, 1),
    sigma ~ dunif( 0, 10)
  ), data = d
)
precis( m5.16)

# compare: all dummy variables included
d$clade.Ape <- ifelse( d$clade == "Ape", 1, 0)
m5.16b <- map(
  alist(
    kcal.per.g ~ dnorm( mu, sigma),
    mu <- a + b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S + b.Ape*clade.Ape,
    a ~ dnorm( 0.6, 10),
    c(b.NWM, b.OWM, b.S,b.Ape) ~ dnorm(0, 1),
    sigma ~ dunif( 0, 10)
  ), data = d
)

m5.16c <- map(
  alist(
    kcal.per.g ~ dnorm( mu, sigma),
    mu <- b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S + b.Ape*clade.Ape,
    c(b.NWM, b.OWM, b.S,b.Ape) ~ dnorm(0, 10),
    sigma ~ dunif( 0, 10)
  ), data = d
)
m5.16d <- map(
  alist(
    kcal.per.g ~ dnorm( mu, sigma),
    mu <- b.NWM*(1 - clade.OWM - clade.S - clade.Ape) + b.OWM*clade.OWM + b.S*clade.S + b.Ape*clade.Ape,
    c(b.NWM, b.OWM, b.S,b.Ape) ~ dnorm(0, 10),
    sigma ~ dunif( 0, 10)
  ), data = d
)

precis( m5.16b)
par(mfrow=c(4,1))
plot(precis( m5.16))
plot(precis( m5.16b))
plot(precis( m5.16c))
plot(precis(m5.16d))
par(mfrow=c(1,1))
# we get a non-identifiable model with huge standard deviations

# to get the average milk energy in each category use samples
post <- extract.samples(m5.16)

# compute averages for each category
mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWM <- post$a + post$b.OWM
mu.S <- post$a + post$b.S

# summarize using precis
precis( data.frame( mu.ape, mu.NWM, mu.OWM, mu.S))
plot( precis( data.frame( mu.ape, mu.NWM, mu.OWM, mu.S) ))

# to get estimated difference between two monkey groups:
diff.NWM.OWM <- mu.NWM - mu.OWM
quantile( diff.NWM.OWM, probs=c(0.025, 0.5, 0.975))


# Unique intercepts approach
(d$clade_id <- coerce_index(d$clade))
# fit model with one intercept for each group
m5.16_alt <- map(
  alist(
    kcal.per.g ~ dnorm( mu, sigma ),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm( 0.6, 10),
    sigma ~ dunif(0, 10)
  ), data=d
)
precis(m5.16_alt, depth=2)
