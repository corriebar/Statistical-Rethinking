library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[ d$age >= 18, ]

plot(height ~ weight, data=d2)
# looks like a nice perfect linear relationship!

# first linear model
m4.3 <- map(
        alist(
          height ~ dnorm( mu, sigma),
          mu <- a + b*weight ,
          a ~ dnorm( 156, 100),
          b ~ dnorm( 0, 10),
          sigma ~ dunif(0, 50)
        ) ,
        data = d2
)

precis( m4.3, corr=T )
# the intercept a corresponds now to the height for some that weighs 0kg
# not very interpretable

# Same model with centered weight
d2$weight.c <- d2$weight - mean(d2$weight )

m4.4 <- map(
          alist(
            height ~ dnorm( mu, sigma) ,
            mu <- a + b*weight.c ,
            a ~ dnorm( 156, 100) ,
            b ~ dnorm(0, 10) ,
            sigma ~ dunif(0, 50)
          ),
          data=d2
)

precis( m4.4, corr=T)
# now the intercept a corresponds to a value of weight.c of zero, which is just the mean
# of weight.c (because it's centered)

plot( height ~ weight, data=d2)
abline(a=coef(m4.3)["a"], b=coef(m4.3)["b"])

post <- extract.samples(m4.3)
head(post)


# lets try out the influence of adding more points to the model
Ns <- c(10, 50, 150, 352)
par(mfrow=c(2,2))
for (N in Ns){
  #N <- 10
  dN <- d2[1:N,]
  mN <- map(
          alist(
            height ~ dnorm(mu, sigma) ,
            mu <- a + b*weight ,
            a ~ dnorm(156, 100) ,
            b ~ dnorm(0, 10) ,
            sigma ~ dunif(0, 50) 
          ), data=dN
  )
  # extract 20 samples from the posterior
  post <- extract.samples(mN, n=20)
  
  # display raw data and sample size
  plot(dN$weight, dN$height, col=rangi2, xlab="weight", ylab="height",
       xlim=range(d2$weight), ylim=range(d2$height))
  mtext(concat("N = ",N))
  
  # plot the lines with transparency
  for (i in 1:20) 
    abline(a=post$a[i], b=post$b[i], col=col.alpha("black", 0.3))
}
par(mfrow=c(1,1))

post <- extract.samples(m4.3)
mu_at_50 <- post$a + post$b * 50
dens( mu_at_50, col=rangi2, lwd=2, xlab="mu | weight=50")
HPDI(mu_at_50, prob=0.89)


# compute mu for each weight observation in data
# link computes the value of each linear model at each sample for each case in the data
mu <- link(m4.3)
# each row is a sample from the posterior distribution


# define sequence of weights to compute predictions
# for these values will be on the horizontal axis
weight.seq <- seq( from=25, to=70, by=1)

# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link(m4.3, data=data.frame(weight=weight.seq))
str(mu)

# plot
plot(height ~ weight, d2, type="n")

# loop over samples and plot each mu value
for (i in 1:100)
  points(weight.seq, mu[i,], pch=16, col=col.alpha(rangi2, 0.1))



# summarize the distribution of mu
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)


# plot raw data
plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))

# plot the MAP line
lines(weight.seq, mu.mean)

# plot a shaded region for 89% HPDI
shade( mu.HPDI, weight.seq)


# simulate heights
sim.height <- sim( m4.3, data=list(weight=weight.seq), n=1e4)
str(sim.height)

height.PI <- apply(sim.height, 2, PI, prob=0.89)

par(mfrow=c(1,1))
# plot everything together
plot( height ~ weight, d2, col=col.alpha(rangi2, 0.5))

# draw a MAP line
lines(weight.seq, mu.mean)

# draw HPDI region for line
shade(mu.HPDI, weight.seq)

# draw PI region for simulated heights
shade(height.PI, weight.seq )


# plot different boundaries
bds <- c(0.67, 0.89, 0.95, 0.97)
par(mfrow=c(2,2))
for (b in bds) {
  height.PI <- apply(sim.height, 2, PI, prob=b)
  
  # plot everything together
  plot( height ~ weight, d2, col=col.alpha(rangi2, 0.5), main=b)
  
  # draw a MAP line
  lines(weight.seq, mu.mean)
  
  # draw HPDI region for line
  shade(mu.HPDI, weight.seq)
  
  # draw PI region for simulated heights
  shade(height.PI, weight.seq )
}



# run your own sim
post <- extract.samples(m4.3)
weight.seq <- 25:70
sim.height <- sapply( weight.seq, function(weight)
  rnorm(
    n=nrow(post),
    mean=post$a + post$b*weight,
    sd=post$sigma
  ))
height.PI <- apply(sim.height, 2, PI, prob=0.89)
