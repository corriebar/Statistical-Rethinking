# Chapter 5 - Spurious Associations
library(rethinking)
data("WaffleDivorce")
d <- WaffleDivorce

# standardize predictior (MedianAgeMarrige)
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)

# fit model
m5.1 <- map(
  alist(
    Divorce ~ dnorm( mu, sigma),
    mu <- a + bA*MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0,10)
  ), data=d
)

# compute shaded confidence region
MAM.seq <- seq(from=-3, to=3.5, length.out = 30)
mu <- link(m5.1, data=data.frame(MedianAgeMarriage.s=MAM.seq  ) )
mu.PI <- apply(mu, 2, PI)

# plot it all
plot( Divorce ~ MedianAgeMarriage.s, data=d, col=rangi2 )
abline( m5.1 )
shade( mu.PI, MAM.seq)

precis(m5.1)

# same for Marriage Rate
# standardize predictior (Marriage Rate)
d$Marriage.s <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)

# fit model
m5.2 <- map(
  alist(
    Divorce ~ dnorm( mu, sigma),
    mu <- a + bR*Marriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data=d 
)

# compute shaded confidence region
mu <- link(m5.2, data=data.frame(Marriage.s=MAM.seq ) )
mu.PI <- apply(mu, 2, PI)

# plot it all
plot( Divorce ~ Marriage.s, data=d, col=rangi2 )
abline( m5.2)
shade( mu.PI, MAM.seq)





# Fit a model with BOTH predictors
m5.3 <- map(
  alist(
    Divorce ~ dnorm( mu , sigma),
    mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s,
    a ~ dnorm( 10, 10) ,
    bR ~ dnorm( 0, 1 ) ,
    bA ~ dnorm( 0, 1 ) ,
    sigma ~ dunif( 0, 10 )
  ), data=d
)

precis(m5.3)

plot( precis(m5.3) )

# Different plots

# Predictor residual plots
# predict Marriage rate by Median Age Marriage
m5.4 <- map(
  alist(
    Marriage.s ~ dnorm( mu, sigma) ,
    mu <- a + b*MedianAgeMarriage.s ,
    a ~ dnorm( 0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)

# compute residuals:
# comüpute expected value at MAP, for each state
mu.R <- coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s
# compute residuals for each state
m.resid.R <- d$Marriage.s - mu.R

# plot residuals
plot( Marriage.s ~ MedianAgeMarriage.s, d, col=rangi2 )
abline( m5.4 )
# loop over states
for ( i in 1:length(m.resid) ){
  x <- d$MedianAgeMarriage.s[i]          # x location of line segment
  y <- d$Marriage.s[i]                   # observed endpoint of line segment
  # draw the line segment
  lines( c(x,x), c(mu.R[i], y), lwd=0.5, col=col.alpha("black", 0.7))
}

# predictor plot (Marriage rate)
plot( d$Divorce ~ m.resid.R, col=rangi2 )
abline( a=coef(m5.3)['a'], b=coef(m5.3)['bR'] )
abline(v=0, lty=2)



# the other way round: predict median age using marriage rate
m5.5 <- map(
  alist(
    MedianAgeMarriage.s ~ dnorm( mu, sigma) ,
    mu <- a + b*Marriage.s ,
    a ~ dnorm( 0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)

# compute residuals:
# compute expected value at MAP, for each state
mu.A <- coef(m5.5)['a'] + coef(m5.4)['b']*d$Marriage.s
# compute residuals for each state
m.resid.A <- d$MedianAgeMarriage.s - mu.A

# plot residuals
plot( MedianAgeMarriage.s ~ Marriage.s, d, col=rangi2 )
abline( m5.5 )
# loop over states
for ( i in 1:length(m.resid) ){
  x <- d$Marriage.s[i]          # x location of line segment
  y <- d$MedianAgeMarriage.s[i]                   # observed endpoint of line segment
  # draw the line segment
  lines( c(x,x), c(mu.A[i], y), lwd=0.5, col=col.alpha("black", 0.7))
}

# predictor plot (Median Age)
plot( d$Divorce ~ m.resid.A, col=rangi2)
abline(a=coef(m5.3)['a'], b=coef(m5.3)['bA'])
abline(v=0, lty=2)




# counterfactual plots
# prepare new counterfactual data, holding Median Age fixed, vary Marriage rate
A.avg <- mean( d$MedianAgeMarriage.s )
R.seq <- seq( from=-3, to=3, length.out = 30)
pred.data <- data.frame(Marriage.s = R.seq, 
                        MedianAgeMarriage.s=A.avg)

# compute counterfactual mean divorce (mu)
mu <- link( m5.3, data=pred.data )
mu.mean <- apply(mu, 2, mean )
mu.PI <- apply(mu, 2, PI)

# simulate counterfactual divorce outcomes
R.sim <- sim( m5.3, data=pred.data, n=1e4 )
R.PI <- apply( R.sim, 2, PI)

# display predictions, hiding raw data wit type="n"
plot(Divorce ~ Marriage.s, data=d, type="n")
mtext("MedianAgeMarriage.s = 0")
lines( R.seq, mu.mean )
shade( mu.PI, R.seq )
shade( R.PI, R.seq )


# same the other way, holding Rate fixed, vary Median Age
R.avg <- mean( d$Marriage.s )
A.seq <- seq( from=-3, to=3, length.out = 30)
pred.data <- data.frame(Marriage.s = R.avg, 
                        MedianAgeMarriage.s=A.seq)

# compute counterfactual mean divorce (mu)
mu <- link( m5.3, data=pred.data )
mu.mean <- apply(mu, 2, mean )
mu.PI <- apply(mu, 2, PI)

# simulate counterfactual divorce outcomes
A.sim <- sim( m5.3, data=pred.data, n=1e4 )
A.PI <- apply( A.sim, 2, PI)

# display predictions, hiding raw data wit type="n"
plot(Divorce ~ MedianAgeMarriage.s, data=d, type="n")
mtext("Marriage.s = 0")
lines( A.seq, mu.mean )
shade( mu.PI, A.seq )
shade( A.PI, A.seq )



# Posterior prediction plots
# call link without specifying new data
# so it uses original data
mu <- link( m5.3 )

# summarize samples accross cases
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply( mu, 2, PI )

# simulate observations
# again, no new data, so uses original data
divorce.sim <- sim( m5.3, n=1e4 )
divorce.PI <- apply(divorce.sim, 2, PI)

# plot observed vs predicted (mean) divrorce rate
plot( mu.mean ~ d$Divorce, col=rangi2, ylim=range(mu.PI),
      xlab="Observed divorce", ylab="Predicted divroce")
abline(a=0, b=1, lty=2 )
for ( i in 1:nrow(d) ) 
  lines( rep(d$Divorce[i], 2), c(mu.PI[1, i], mu.PI[2, i]), col=rangi2)
identify( x=d$Divorce, y=mu.mean, labels=d$Loc, cex=0.8 )


# residual plot showing the mean prediction error
# compute residuals
divorce.resid <- d$Divorce - mu.mean
# get ordering by divorce rate
o <- order(divorce.resid)
# make the plot
dotchart( divorce.resid[o], labels=d$Loc[o], xlim=c(-6,5), cex=0.6 )
abline(v=0, col=col.alpha("black", 0.2))
for (i in 1:nrow(d) ) {
  j <- o[i]    # which State in order
  lines( d$Divorce[j] - c(mu.PI[1, j], mu.PI[2, j]), rep(i,2) )
  points( d$Divorce[j] - c(divorce.PI[1,j], divorce.PI[2, j]), rep(i,2) ,
          pch=3, cex=0.6, col="gray")
}




# novel predictor residual plot
d$d.resid <- divorce.resid
d$WaffleHouses.d <- d$WaffleHouses / d$Population

m5.6 <- map(
  alist(
    d.resid ~ dnorm( mu, sigma) ,
    mu <- a + b*WaffleHouses.d ,
    a ~ dnorm( 0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)

waffle.seq <- seq(from=-1, to=42, length.out = 30)
mu <- link(m5.6, data.frame(WaffleHouses.d=waffle.seq) )
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)


plot(divorce.resid ~ d$WaffleHouses.d, col=rangi2,
     xlab="Waffles per capita", ylab="Divorce error")
lines(waffle.seq, mu.mean)
shade(mu.PI, waffle.seq )



# simulate spurious associations
N <- 100
x_real <- rnorm( N)
x_spur <- rnorm( N, x_real )
y <- rnorm( N, x_real )
d <- data.frame(y, x_real, x_spur)

pairs(d)

m5.7 <- map(
  alist(
    y ~ dnorm( mu, sigma) ,
    mu <- a + bR*x_real + bS*x_spur,
    a ~ dnorm( 0, 10 ),
    bR ~ dnorm(0, 1),
    bS ~ dnorm(0, 1),
    sigma ~ dunif(0,10)
  ), data = d
)

plot( precis(m5.7))
