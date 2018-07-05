# Using information criteria

## Model comparison
library(rethinking)
data(milk)
d <- milk[ complete.cases(milk), ]     # remove NA values
d$neocortex <- d$neocortex.perc / 100
dim(d)

head(d)

# Predict kcal.per.g using the predictors neocortex and the logarithm of mass
# Use four different models (all with flat priors)
a.start <- mean(d$kcal.per.g)
sigma.start <- log( sd( d$kcal.per.g ))

m6.11 <- map( 
  alist(
    kcal.per.g ~ dnorm( a, exp(log.sigma) )
  ), data=d, start=list(a=a.start, log.sigma=sigma.start) )

m6.12 <- map(
  alist(
    kcal.per.g ~ dnorm( mu, exp(log.sigma) ),
    mu <- a + bn*neocortex
  ), data=d, start=list(a=a.start, bn=0, log.sigma=sigma.start)
)

m6.13 <- map(
  alist(
    kcal.per.g ~ dnorm( mu, exp(log.sigma) ),
    mu <- a + bm*log(mass)
  ), data=d, start=list(a=a.start, bm=0, log.sigma=sigma.start)
)

m6.14 <- map(
  alist(
    kcal.per.g ~ dnorm( mu, exp(log.sigma) ),
    mu <- a + bn*neocortex + bm*log(mass)
  ), data=d, start=list(a=a.start, bn=0, bm=0, log.sigma=sigma.start)
)

# Comparing WAIC values
WAIC( m6.14 )

# handy function in rethinking that computes WAIC for all models and ranks from best to worst:
set.seed(2405)
( milk.models <- compare( m6.11, m6.12, m6.13, m6.14) )
plot( milk.models, SE=TRUE, dSE=TRUE )

# How to interpret the weights (Akaike weights): the model m6.14 has probability of 92% to make the best predictions on new data
# Caveat: Uncertainty propagates to the weights as well (and there is a lot of uncertainty since we only have few observations)

# For example: difference between m6.14 and m6.11, centred at 7.3 with a standard deviation of 7.29
# get probability that difference is negative, i.e. the two models actually reversed:
diff <- rnorm(1e5, 7.3, 7.29)
sum( diff < 0 ) / 1e5


## Comparing estimates
coeftab( m6.11, m6.12, m6.13, m6.14 )
plot( coeftab( m6.11, m6.12, m6.13, m6.14 ) )

# different options for coeftab_plot:
plot( coeftab( m6.11, m6.12, m6.13, m6.14), by.model=TRUE )    # sort by model 
plot( coeftab( m6.11, m6.12, m6.13, m6.14), by.model=FALSE, pars=c("bn", "bm") )     # show only certain parameters


## Model averaging
### simulate and plot counterfactual predictions for the minimum-WAIC model m6.14
# neocortex from 0.5 to 0.8
nc.seq <- seq(from=0.5, to =0.8, length.out = 30)
d.predict <- list(
  kcal.per.g = rep(0, 30),            # empty outcome
  neocortex = nc.seq,                 # sequence of neocortex
  mass = rep(median(d$mass), 30)      # average mass   (no idea where the 4.5 came from)
)
pred.m6.14 <- link( m6.14, data=d.predict)
mu <- apply( pred.m6.14, 2, mean )
mu.PI <- apply( pred.m6.14, 2, PI )


# plot it all
plot( kcal.per.g ~ neocortex, d, col=rangi2 )
lines( nc.seq, mu, lty=2 )
lines( nc.seq, mu.PI[1, ], lty=2)
lines( nc.seq, mu.PI[2, ], lty=2)

# let's add model averaged posterior predictions
milk.ensemble <- ensemble( m6.11, m6.12, m6.13, m6.14, data=d.predict )
mu <- apply(milk.ensemble$link, 2, mean )
mu.PI <- apply( milk.ensemble$link, 2, PI )
kcal.per.g.PI <- apply(milk.ensemble$sim, 2, PI )
lines( nc.seq, mu )
shade( mu.PI, nc.seq )
shade( kcal.per.g.PI, nc.seq )    # yay even more uncertainty!

# do the ensemble yourself
# compute weights for each model
n <- 1e3
ctab <- compare( m6.11, m6.12, m6.13, m6.14, sort = F)
weights <- ctab@output$weight

L <- list( m6.11, m6.12, m6.13, m6.14 )
# for each "observation" in d.predict, link computes 1000 samples of the posterior mu (this for each model)
link.list <- lapply( L, function(m) link(m, data=d.predict, n=n))
# simulate for each "observation" in d.predict an outcome using a sampled mu + sigma + dnorm
sim.list <- lapply( L, function(m) sim(m, data=d.predict, n=n))

# combine values
# for each model, we have a matrix of samples (both of mu and the outcome)
# we combine this into one matrix with 1000 samples taking from each model matrix out rows according to their weight
idx <- round( weights * n)

idx_start <- rep(1,length(idx))
idx_end <- idx
if ( length(L)>1 )
  for ( i in 2:length(idx) ) {
    idx_start[i] <- min( idx_start[i-1] + idx[i-1] , n )
    idx_end[i] <- min( idx_start[i] + idx[i] - 1 , n )
    if ( i==length(idx) ) idx_end[i] <- n
  }

link_out <- link.list[[1]]    # initiate with first model matrix
sim_out <- sim.list[[1]]
for ( i in 1:length(idx) ) {
  if ( idx[i]>0 ) {
    idxrange <- idx_start[i]:idx_end[i]
    link_out[idxrange,] <- link.list[[i]][idxrange,]
    sim_out[idxrange,] <- sim.list[[i]][idxrange,]
  }
} # alternatively, we could also use sample with prob = weight
