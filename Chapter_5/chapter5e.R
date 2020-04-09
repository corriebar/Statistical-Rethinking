# Chapter 5 - Ordinary least squares
library(rethinking)
# fit a linear regression using OLS
d <- data.frame( x= rnorm(100), y = rnorm(100, 10, 2), z= rnorm(100, -3, 0.3))
m5.17 <- lm( y ~ 1 + x, data=d)
m5.18 <- lm( y ~ 1 + x + z, data=d)

# intercepts are optional: the two following models are equivalent
m5.17 <- lm( y ~ 1 + x, data=d)
m5.19 <- lm( y ~ x, data=d)

# omit intercept:
m5.20 <- lm( y ~ 0 + x, data=d)
m5.21 <- lm( y ~ x - 1, data=d)

# categories are dealt with automatically, but best to be explicit:
d$season <- sample.int(4, size=100, replace=TRUE)
m5.22 <- lm( y ~ 1 + as.factor(season), data=d)


# transform variables inside design formula
m5.24 <- lm( y ~ 1 + x + I(x^2) + I(x^3), data=d)
summary(m5.24)

# Note: lm does not report the standard deviation sigma! only the residual standard error! 
# slightly different estimate of sigma, but without any uncertainty information!


# building map formulas from lm formulas
data(cars)
glimmer( dist ~ speed, data=cars)

glimmer( y ~ ., data=d)

m5.25 <- glimmer( dist ~ speed, data=cars)
m5.25s <- map(m5.25$f, data=m5.25$d)