# Chapter 6 - Overfitting
library(rethinking)
sppnames <- c("afarensis", "africanus", "habilis", "boisei", 
              "rudolfensis", "ergaster", "sapiens")
brainvolcc <- c( 438, 452, 612, 521, 752, 871, 1350 )
masskg <- c( 37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5 )
d <- data.frame( species=sppnames, brain=brainvolcc, mass=masskg)

plot( brain ~ mass, data=d)
text( brain ~ mass, data=d, labels=d$species, pos=3)

# Start fitting a series of increasingly complex model families
# Starting with the most simplest one: linear model
# we use lm, so it's flat priors
m6.1 <- lm( brain ~ mass, data=d)

# to assess the fitness of the model, we use R²
# R² gives a measure of how much variance of the outcome variable is explained by the model
1 - var(resid(m6.1)) / var(d$brain)
summary(m6.1)
# R² = 0.4902

mass.seq <- seq(from=33, to=63, length.out = 100)
pred.data <- data.frame(mass=mass.seq)
mu6.1 <- link( m6.1, data=pred.data)
mu6.1.PI <- apply( mu6.1, 2, PI, prob=0.89)

# fit more complex model, we use polynomial models, each of a higher degree than the previous one
d$mass2 <- d$mass^2
pred.data$mass2 <- pred.data$mass^2
m6.2 <- lm( brain ~ mass + mass2, data=d)
summary(m6.2)
# R² = 0.536
mu6.2 <- link( m6.2, data=pred.data)
mu6.2.PI <- apply(mu6.2, 2, PI, prob=0.89)

d$mass3 <- d$mass^3
pred.data$mass3 <- pred.data$mass^3
m6.3 <- lm( brain ~ mass + mass2 + mass3, data=d)
summary(m6.3)
# R² = 0.6798
mu6.3 <- link( m6.3, data=pred.data)
mu6.3.PI <- apply(mu6.3, 2, PI, prob=0.89)

d$mass4 <- d$mass^4
pred.data$mass4 <- pred.data$mass^4
m6.4 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4), data=d)
summary(m6.4)
# R² = 0.8144
mu6.4 <- link( m6.4, data=pred.data)
mu6.4.PI <- apply(mu6.4, 2, PI, prob=0.89)

d$mass5 <- d$mass^5
pred.data$mass5 <- pred.data$mass^5
m6.5 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5), data=d)
summary(m6.5)
# R² = 0.9889
mu6.5 <- link( m6.5, data=pred.data)
mu6.5.PI <- apply(mu6.5, 2, PI, prob=0.89)

d$mass6 <- d$mass^6
pred.data$mass6 <- pred.data$mass^6
m6.6 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6), data=d)
summary(m6.6)
# R² = 1

par(mfrow=c(3,2))
plot( brain ~ mass, data=d, main="R^2=0.49", ylim=range(mu6.1.PI)+c(0,100))
lines(mass.seq, predict(m6.1, newdata=pred.data))
shade(mu6.1.PI, mass.seq)

plot( brain ~ mass, data=d, main="R^2=0.54", ylim=range(mu6.2.PI)+c(0,100))
lines(mass.seq, predict(m6.2, newdata=pred.data))
shade(mu6.2.PI, mass.seq)

plot( brain ~ mass, data=d, main="R^2=0.68", ylim=range(mu6.3.PI)+c(0,100))
lines(mass.seq, predict(m6.3, newdata=pred.data))
shade(mu6.3.PI, mass.seq)

plot( brain ~ mass, data=d, main="R^2=0.81", ylim=range(mu6.4.PI))
lines(mass.seq, predict(m6.4, newdata=pred.data))
shade(mu6.4.PI, mass.seq)

plot( brain ~ mass, data=d, main="R^2=0.99", ylim=range(mu6.5.PI))
lines(mass.seq, predict(m6.5, newdata=pred.data))
shade(mu6.5.PI, mass.seq)

plot( brain ~ mass, data=d, main="R^2=1", ylim=c(-400, 1600))
lines(mass.seq, predict(m6.6, newdata=pred.data))
abline(h=0, lty=2)


par(mfrow=c(1,1))


# Underfitting
# a model that learns too little from data
m6.7 <- lm( brain ~ 1, data=d)
mu6.7 <- link(m6.7, data=pred.data)
mu6.7.PI <- apply(mu6.7, 2, PI)

plot(brain ~ mass, data=d)
abline(m6.7)
shade(mu6.7.PI, mass.seq)



# Underfitting and overfitting can be understood as under-sensitivity and 
# over-sensitivity to sample
par(mfrow=c(1,2))
plot( brain ~ mass, data=d, col="slateblue")
for (i in 1:nrow(d) ) {
  d.new <- d[ -i, ]
  m0 <- lm( brain ~ mass, d.new )
  abline( m0, col=col.alpha("black", 0.5))
}

plot( brain ~ mass, data=d, col="slateblue", ylim=c(-500, 2000))
for ( i in 1:nrow(d) ) {
  d.new <- d[ -i, ]
  m1 <- lm( brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6), d.new)
  lines( mass.seq, predict( m1, newdata=pred.data))
}
