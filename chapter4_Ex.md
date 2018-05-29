Chapter 4 exercise
================
Corrie
May 21, 2018

Chapter 4 - Exercises
=====================

These are my solutions to the practice questions of chapter 4, Linear Models, of the book "Statistical Rethinking" by Richard McElreath.

Easy Questions.
---------------

**4E1.** In the model definition below, which line is the likelihood:

![latex
\\begin{align\*}
y\_i &\\sim \\text{Normal}(\\mu, \\sigma) & \\text{This is the likelihood}\\\\
\\mu &\\sim \\text{Normal}(0, 10) \\\\
\\sigma &\\sim \\text{Normal}(0,10)
\\end{align\*} ](https://latex.codecogs.com/png.latex?latex%0A%5Cbegin%7Balign%2A%7D%0Ay_i%20%26%5Csim%20%5Ctext%7BNormal%7D%28%5Cmu%2C%20%5Csigma%29%20%26%20%5Ctext%7BThis%20is%20the%20likelihood%7D%5C%5C%0A%5Cmu%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C%2010%29%20%5C%5C%0A%5Csigma%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C10%29%0A%5Cend%7Balign%2A%7D%20 "latex
\begin{align*}
y_i &\sim \text{Normal}(\mu, \sigma) & \text{This is the likelihood}\\
\mu &\sim \text{Normal}(0, 10) \\
\sigma &\sim \text{Normal}(0,10)
\end{align*} ")

**4E2.** In the model definition just above, how many parameters are in the posterior distribution?

There are **2** parameters, ![\\mu](https://latex.codecogs.com/png.latex?%5Cmu "\mu") and ![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma "\sigma").

**4E3.** Write down the appropriate form of Bayes' theorem that includes the proper likelihood and priors.

![\\begin{align\*}
P(\\mu, \\sigma| y\_i) &\\propto \\text{Likelihood } \\times \\text{ Prior probability} \\\\
\\\\
P(\\mu, \\sigma| y\_i) &= \\frac{\\prod\_i \\text{Normal}(y\_i|\\mu, \\sigma) \\times 
\\text{Normal}(\\mu| 0,10) \\times \\text{Normal}(\\sigma|0,10) }
{\\int \\prod\_i \\text{Normal}(y\_i|\\mu, \\sigma) \\times 
\\text{Normal}(\\mu| 0,10) \\times \\text{Normal}(\\sigma|0,10) \\text{ d}\\mu\\text{d}\\sigma} 
\\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0AP%28%5Cmu%2C%20%5Csigma%7C%20y_i%29%20%26%5Cpropto%20%5Ctext%7BLikelihood%20%7D%20%5Ctimes%20%5Ctext%7B%20Prior%20probability%7D%20%5C%5C%0A%5C%5C%0AP%28%5Cmu%2C%20%5Csigma%7C%20y_i%29%20%26%3D%20%5Cfrac%7B%5Cprod_i%20%5Ctext%7BNormal%7D%28y_i%7C%5Cmu%2C%20%5Csigma%29%20%5Ctimes%20%0A%5Ctext%7BNormal%7D%28%5Cmu%7C%200%2C10%29%20%5Ctimes%20%5Ctext%7BNormal%7D%28%5Csigma%7C0%2C10%29%20%7D%0A%7B%5Cint%20%5Cprod_i%20%5Ctext%7BNormal%7D%28y_i%7C%5Cmu%2C%20%5Csigma%29%20%5Ctimes%20%0A%5Ctext%7BNormal%7D%28%5Cmu%7C%200%2C10%29%20%5Ctimes%20%5Ctext%7BNormal%7D%28%5Csigma%7C0%2C10%29%20%5Ctext%7B%20d%7D%5Cmu%5Ctext%7Bd%7D%5Csigma%7D%20%0A%5Cend%7Balign%2A%7D "\begin{align*}
P(\mu, \sigma| y_i) &\propto \text{Likelihood } \times \text{ Prior probability} \\
\\
P(\mu, \sigma| y_i) &= \frac{\prod_i \text{Normal}(y_i|\mu, \sigma) \times 
\text{Normal}(\mu| 0,10) \times \text{Normal}(\sigma|0,10) }
{\int \prod_i \text{Normal}(y_i|\mu, \sigma) \times 
\text{Normal}(\mu| 0,10) \times \text{Normal}(\sigma|0,10) \text{ d}\mu\text{d}\sigma} 
\end{align*}")

 where

![\\text{Normal}(x|\\mu, \\sigma) = \\frac{1}{\\sqrt{2\\pi \\sigma^2} }\\exp(- \\frac{(x-\\mu)^2}{2\\sigma^2})](https://latex.codecogs.com/png.latex?%5Ctext%7BNormal%7D%28x%7C%5Cmu%2C%20%5Csigma%29%20%3D%20%5Cfrac%7B1%7D%7B%5Csqrt%7B2%5Cpi%20%5Csigma%5E2%7D%20%7D%5Cexp%28-%20%5Cfrac%7B%28x-%5Cmu%29%5E2%7D%7B2%5Csigma%5E2%7D%29 "\text{Normal}(x|\mu, \sigma) = \frac{1}{\sqrt{2\pi \sigma^2} }\exp(- \frac{(x-\mu)^2}{2\sigma^2})")

.

**4E4.** In the model definition below, which line is the linear model?

![\\begin{align\*}
y\_i &\\sim \\text{Normal}(\\mu\_i, \\sigma) \\\\
\\mu\_i &= \\alpha + \\beta x\_i & \\text{This is the linear model}\\\\
\\alpha &\\sim \\text{Normal}(0,10) \\\\
\\beta &\\sim \\text{Normal}(0,1) \\\\
\\sigma &\\sim \\text{Uniform}(0,10)
\\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0Ay_i%20%26%5Csim%20%5Ctext%7BNormal%7D%28%5Cmu_i%2C%20%5Csigma%29%20%5C%5C%0A%5Cmu_i%20%26%3D%20%5Calpha%20%2B%20%5Cbeta%20x_i%20%26%20%5Ctext%7BThis%20is%20the%20linear%20model%7D%5C%5C%0A%5Calpha%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C10%29%20%5C%5C%0A%5Cbeta%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C1%29%20%5C%5C%0A%5Csigma%20%26%5Csim%20%5Ctext%7BUniform%7D%280%2C10%29%0A%5Cend%7Balign%2A%7D "\begin{align*}
y_i &\sim \text{Normal}(\mu_i, \sigma) \\
\mu_i &= \alpha + \beta x_i & \text{This is the linear model}\\
\alpha &\sim \text{Normal}(0,10) \\
\beta &\sim \text{Normal}(0,1) \\
\sigma &\sim \text{Uniform}(0,10)
\end{align*}")

**4E5.** In the model definition just above, how many parameters are in the posterior distribution?

There are **3** parameters in the posterior distribution, ![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\alpha"), ![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\beta"), and ![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma "\sigma").

Medium questions.
-----------------

**4M1.** For the model definition below, simulate observed heights from the prior (not the posterior).

![\\begin{align\*}
y\_i &\\sim \\text{Normal}(\\mu, \\sigma) \\\\
\\mu &\\sim \\text{Normal}(0, 10) \\\\
\\sigma &\\sim \\text{Uniform}(0,10)
\\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0Ay_i%20%26%5Csim%20%5Ctext%7BNormal%7D%28%5Cmu%2C%20%5Csigma%29%20%5C%5C%0A%5Cmu%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C%2010%29%20%5C%5C%0A%5Csigma%20%26%5Csim%20%5Ctext%7BUniform%7D%280%2C10%29%0A%5Cend%7Balign%2A%7D "\begin{align*}
y_i &\sim \text{Normal}(\mu, \sigma) \\
\mu &\sim \text{Normal}(0, 10) \\
\sigma &\sim \text{Uniform}(0,10)
\end{align*}")

``` r
n <- 10000
mu <- rnorm(n, 0, 10)
sigma <- runif(n, 0, 10)
y_prior <- rnorm(n, mu, sigma)
hist(y_prior)
```

![](chapter4_Ex_files/figure-markdown_github/unnamed-chunk-1-1.png)

**4M2.** Translate the model just above into a `map` formula.

``` r
flist <- alist(
            y ~ dnorm(mu, sigma),
            mu ~ dnorm(0, 10),
            sigma ~ dunif(0,10)
          ) 
```

**4M3.** Translate the `map` formula below into a mathematical model definition.

``` r
flist <- alist(
  y ~ dnorm( mu, sigma ),
  mu <- a + b*x,
  a ~ dnorm( 0, 50 ),
  b ~ dnorm( 0, 10 ),
  sigma ~ dunif( 0, 50 )
)
```

The mathematical definition:

![\\begin{align\*}
y\_i &\\sim \\text{Normal}(\\mu\_i, \\sigma) \\\\
\\mu\_i &= \\alpha + \\beta x\_i \\\\
\\alpha &\\sim \\text{Normal}(0,50) \\\\
\\beta &\\sim  \\text{Uniform}(0,10) \\\\
\\sigma &\\sim \\text{Uniform}(0,50) 
\\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0Ay_i%20%26%5Csim%20%5Ctext%7BNormal%7D%28%5Cmu_i%2C%20%5Csigma%29%20%5C%5C%0A%5Cmu_i%20%26%3D%20%5Calpha%20%2B%20%5Cbeta%20x_i%20%5C%5C%0A%5Calpha%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C50%29%20%5C%5C%0A%5Cbeta%20%26%5Csim%20%20%5Ctext%7BUniform%7D%280%2C10%29%20%5C%5C%0A%5Csigma%20%26%5Csim%20%5Ctext%7BUniform%7D%280%2C50%29%20%0A%5Cend%7Balign%2A%7D "\begin{align*}
y_i &\sim \text{Normal}(\mu_i, \sigma) \\
\mu_i &= \alpha + \beta x_i \\
\alpha &\sim \text{Normal}(0,50) \\
\beta &\sim  \text{Uniform}(0,10) \\
\sigma &\sim \text{Uniform}(0,50) 
\end{align*}")

**4M4.** A sample of students is measured for height each year for three years. You want to fit a linear regression, using year as a prediction. Write down the mathematical model definition.

![\\begin{align\*}
h\_i &\\sim \\text{Normal}(\\mu\_i, \\sigma) \\\\
\\mu\_i &= \\alpha + \\beta t\_i \\\\
\\alpha &\\sim \\text{Normal}(160, 50) \\\\
\\beta &\\sim \\text{Normal}(0, 10) \\\\
\\sigma &\\sim \\text{Uniform}(0, 50)
\\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0Ah_i%20%26%5Csim%20%5Ctext%7BNormal%7D%28%5Cmu_i%2C%20%5Csigma%29%20%5C%5C%0A%5Cmu_i%20%26%3D%20%5Calpha%20%2B%20%5Cbeta%20t_i%20%5C%5C%0A%5Calpha%20%26%5Csim%20%5Ctext%7BNormal%7D%28160%2C%2050%29%20%5C%5C%0A%5Cbeta%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C%2010%29%20%5C%5C%0A%5Csigma%20%26%5Csim%20%5Ctext%7BUniform%7D%280%2C%2050%29%0A%5Cend%7Balign%2A%7D "\begin{align*}
h_i &\sim \text{Normal}(\mu_i, \sigma) \\
\mu_i &= \alpha + \beta t_i \\
\alpha &\sim \text{Normal}(160, 50) \\
\beta &\sim \text{Normal}(0, 10) \\
\sigma &\sim \text{Uniform}(0, 50)
\end{align*}")

 Here, ![h\_i](https://latex.codecogs.com/png.latex?h_i "h_i") is the height and ![t\_i](https://latex.codecogs.com/png.latex?t_i "t_i") is the year of the ![i](https://latex.codecogs.com/png.latex?i "i")th observation. Since ![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\alpha") is the average height of a student at year zero, I picked a normal distribution with mean 160 (assuming an average height of 160cm) and standard deviation 50, this is relatively weak, leaving a wide range of possible heights. For ![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\beta"), I picked a normal distribution with mean 0 and standard deviation 10, meaning on average, a person grows 0cm per year with standard deviation 10cm, since I don't expect many people to grow or shrink more than 20cm per year.

**4M5.** Now suppose, the average in the first year was 120cm and that every student got taller each year. I will change my priors as follows:

![\\begin{align\*}
\\alpha &\\sim \\text{Normal}(120, 50) \\\\
\\beta &\\sim \\text{Uniform}(0, 20)
\\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0A%5Calpha%20%26%5Csim%20%5Ctext%7BNormal%7D%28120%2C%2050%29%20%5C%5C%0A%5Cbeta%20%26%5Csim%20%5Ctext%7BUniform%7D%280%2C%2020%29%0A%5Cend%7Balign%2A%7D "\begin{align*}
\alpha &\sim \text{Normal}(120, 50) \\
\beta &\sim \text{Uniform}(0, 20)
\end{align*}")

 I adjusted the mean for the average height accordingly and changed ![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\beta") to a uniform distribution, so that ![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\beta"), the indicator for growth per year, is greater or equal than zero. I still don't expect people to grow more than 20cm per year.

**4M6.** Now suppose, the variance among heights for students of the same age is never more than 64cm. I thus change my priors as follows:

![\\sigma \\sim \\text{Uniform}(0, 64).](https://latex.codecogs.com/png.latex?%5Csigma%20%5Csim%20%5Ctext%7BUniform%7D%280%2C%2064%29. "\sigma \sim \text{Uniform}(0, 64).")

Hard question.
--------------

**4H1.** !Kung census data: Provide predicted heights and 89% intervals (either HPDI or PI) for the following weights of individuals.

``` r
weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
```

For this, we first load the !Kung data from the `Howell1` data set and set up a model. I will use a linear model with priors as in the model definition given earlier in the chapter.

``` r
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18, ]
d2$weight.c <- d2$weight - mean(d2$weight)    # centering the weights

# fit model
model <- map(
  alist(
    height ~ dnorm( mu, sigma) ,
    mu <- a + b*weight.c,   
    a ~ dnorm( 156, 100) ,   # average height with weak prior
    b ~ dnorm( 0, 10),       # fairly uninformative prior
    sigma ~ dunif( 0, 50)
  ), 
  data=d2
)

precis( model)
```

    ##         Mean StdDev   5.5%  94.5%
    ## a     154.60   0.27 154.17 155.03
    ## b       0.91   0.04   0.84   0.97
    ## sigma   5.07   0.19   4.77   5.38

How to interpret the model: Since we centered the weights such that the mean of `weight.c` is zero, `a` corresponds to the average height. The value 0.91 for `b` means that a person 1kg heavier is expected to be 0.90cm taller. The estimate for ![\\sigma](https://latex.codecogs.com/png.latex?%5Csigma "\sigma"), `sigma` tells us about the width of the distribution of heights around the mean. Let's now use the model to predict the heights for the weight values given above. For this, we simulate heights for each given weight value. We do this by first obtaining a sample from the posterior distribution and then use this sample to draw samples from a Gaussian distribution. Note that we also need to center the given weights now.

``` r
weights.c <- weights - mean(d2$weight)
post <- extract.samples(model)
sim.height <- sapply( weights.c, function(weight) {
  rnorm(
    n = nrow(post),
    mean = post$a + post$b*weight,
    sd = post$sigma
  )
})

height.PI <- apply(sim.height, 2, PI, prob=0.89)
height.HPDI <- apply(sim.height, 2, HPDI, prob=0.89)
height.mean <- apply(sim.height, 2, mean)

pred_df <- data.frame("individual"=1:5, "weight"=weights, "exptected_height"=height.mean, 
                      "PI_89_lower"=height.PI[1,], "PI_89_upper"=height.PI[2,])
pred_df
```

    ##   individual weight exptected_height PI_89_lower PI_89_upper
    ## 1          1  46.95         156.3494    148.1657    164.5174
    ## 2          2  43.72         153.3438    145.0099    161.6660
    ## 3          3  64.78         172.4697    164.2334    180.7199
    ## 4          4  32.59         143.3905    135.3294    151.5512
    ## 5          5  54.63         163.3429    155.2578    171.5821

**4H2.** Select the rows from the `Howell1` data with age below 18 years.

1.  Fit a linear regression to these data, using `map`. I will use the same model as above.

``` r
d18 <- d[ d$age < 18, ]
d18$weight.c <- d18$weight - mean(d18$weight)   # centering the data

# fit the model

model18 <- map(
  alist(
    height ~ dnorm( mu, sigma) ,
    mu <- a + b*weight.c ,
    a ~ dnorm( 156, 100) ,
    b ~ dnorm( 0, 10) ,
    sigma ~ dunif(0, 50)
  ),
  data=d18
)
precis(model18)
```

    ##         Mean StdDev   5.5%  94.5%
    ## a     108.32   0.61 107.35 109.29
    ## b       2.72   0.07   2.61   2.83
    ## sigma   8.44   0.43   7.75   9.13

As above, since we centered the weights, the intercept `a` corresponds to the average height, which is here 108.3. This is much lower than in the model above (but expected since the individuals in this data set are younger). The slope `b` is interpreted such that for every 10kg heavier, an individual is expected to be 27cm taller. The standard deviation `sigma` in this model is higher than in the one above, suggesting a higher uncertainty in the predictions.

1.  Plot the raw data and superimpose the MAP regression line and 89% HPDI for the mean and for the predicted height. We first compute the regression line by generating a sequence over the whole range of (centered) weights for which we then sample from the posterior distribution to compute a sample of mu, of which we can then compute the mean and the 89% HPDI. We similarly compute the 89% HPDI for the predicted height (as also done in the question before).

``` r
weight.seq <- seq(from=-15, to=30, length.out = 30)             # generate weights (centered) over the whole range
post <- extract.samples(model18)                                # extract a posterior sample
# compute mu
mu.link <- function(weight.c) post$a + post$b*weight.c           # the function to compute mu, using the sample above
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)

# compute predicted height
sim.height <- sapply( weight.seq, function(weight) {
  rnorm(
    n = nrow(post),
    mean = post$a + post$b*weight,
    sd = post$sigma
  )
})

height.HPDI <- apply(sim.height, 2, HPDI, prob=0.89)
height.mean <- apply(sim.height, 2, mean)

# plot everything
plot(height ~ weight.c, data=d18, col=col.alpha(rangi2, 0.9), ylim=c(50, 180))   # the raw data
lines(weight.seq, mu.mean)                                      # the MAP regression line
shade( mu.HPDI, weight.seq)                                     # draw HPDI region around the regression line
shade( height.HPDI, weight.seq)                                 # draw HPDI region for the simulated heights
```

![](chapter4_Ex_files/figure-markdown_github/unnamed-chunk-8-1.png)

1.  What aspects of the model fit concern you? The linear model doesn't seem to be a very good fit for the data. It performs very poorly for the lower and higher values of weight. One possibility to improve the model could be to use a polynomial model (e.g. of 2nd order) instead.

**4H3.** A colleague exclaims: "Only the *logarithm* of body weight scales with height!" Let's try this out.

1.  Use the entire `Howell1` data frame using the following model:

    ![\\begin{align\*}
    h\_i &\\sim \\text{Normal}(\\mu\_i, \\sigma) \\\\
    \\mu\_i &= \\alpha + \\beta \\log(w\_i) \\\\
    \\alpha &\\sim \\text{Normal}(178, 100) \\\\
    \\beta &\\sim \\text{Normal}(0, 100) \\\\
    \\sigma &\\sim \\text{Uniform}(0, 50)
    \\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0Ah_i%20%26%5Csim%20%5Ctext%7BNormal%7D%28%5Cmu_i%2C%20%5Csigma%29%20%5C%5C%0A%5Cmu_i%20%26%3D%20%5Calpha%20%2B%20%5Cbeta%20%5Clog%28w_i%29%20%5C%5C%0A%5Calpha%20%26%5Csim%20%5Ctext%7BNormal%7D%28178%2C%20100%29%20%5C%5C%0A%5Cbeta%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C%20100%29%20%5C%5C%0A%5Csigma%20%26%5Csim%20%5Ctext%7BUniform%7D%280%2C%2050%29%0A%5Cend%7Balign%2A%7D "\begin{align*}
    h_i &\sim \text{Normal}(\mu_i, \sigma) \\
    \mu_i &= \alpha + \beta \log(w_i) \\
    \alpha &\sim \text{Normal}(178, 100) \\
    \beta &\sim \text{Normal}(0, 100) \\
    \sigma &\sim \text{Uniform}(0, 50)
    \end{align*}")

     Here the model description in R:

``` r
d <- Howell1
# fit the model

model.l <- map(
  alist(
    height ~ dnorm( mu, sigma) ,
    mu <- a + b*log(weight) ,
    a ~ dnorm( 178, 100) ,
    b ~ dnorm( 0, 100) ,                
    sigma ~ dunif(0, 50)
  ),
  data=d
)
precis(model.l)
```

    ##         Mean StdDev   5.5%  94.5%
    ## a     -23.79   1.34 -25.92 -21.66
    ## b      47.08   0.38  46.47  47.69
    ## sigma   5.13   0.16   4.89   5.38

Interpreting these results is a bit more difficult since we transformed the weights using the logarithm. Furthermore, the data is not centralized as before, so the intercept `a` corresponds to the average height of someone whose log weight is zero, i.e. whose weight is 1kg. How to interpret the `b` value? If we raise the weight by one unit, we get the following expression for mu:

![\\begin{align\*}
\\mu &= \\alpha + \\beta \\log(\\text{weight} + 1) 
\\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0A%5Cmu%20%26%3D%20%5Calpha%20%2B%20%5Cbeta%20%5Clog%28%5Ctext%7Bweight%7D%20%2B%201%29%20%0A%5Cend%7Balign%2A%7D "\begin{align*}
\mu &= \alpha + \beta \log(\text{weight} + 1) 
\end{align*}")

 Using some rules for logarithms, we get:

![\\begin{align\*}
\\mu &= \\alpha + \\beta \\log(\\text{weight}) + \\beta \\log(1 + \\frac{1}{\\text{weight}})
\\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0A%5Cmu%20%26%3D%20%5Calpha%20%2B%20%5Cbeta%20%5Clog%28%5Ctext%7Bweight%7D%29%20%2B%20%5Cbeta%20%5Clog%281%20%2B%20%5Cfrac%7B1%7D%7B%5Ctext%7Bweight%7D%7D%29%0A%5Cend%7Balign%2A%7D "\begin{align*}
\mu &= \alpha + \beta \log(\text{weight}) + \beta \log(1 + \frac{1}{\text{weight}})
\end{align*}")

 That is, an increase of one unit in the weight variable is associated with an increase of the mean ![\\mu](https://latex.codecogs.com/png.latex?%5Cmu "\mu") of ![\\beta \\log(1 + \\frac{1}{\\text{weight}})](https://latex.codecogs.com/png.latex?%5Cbeta%20%5Clog%281%20%2B%20%5Cfrac%7B1%7D%7B%5Ctext%7Bweight%7D%7D%29 "\beta \log(1 + \frac{1}{\text{weight}})"). I personally find that not very intuitive, so let's have a look at some plots as well.

``` r
weight.seq <- seq(from=2, to=65, length.out = 70)             # generate weights over the whole range
                                                              # min(d$weight) = 4.25, max(d$weight) = 62.99
post <- extract.samples(model.l)                              # extract a posterior sample
# compute mu
mu.link <- function(weight) post$a + post$b*log(weight)       # the function to compute mu, using the sample above
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)

# compute predicted height
sim.height <- sapply( weight.seq, function(weight) {
  rnorm(
    n = nrow(post),
    mean = post$a + post$b*log(weight),
    sd = post$sigma
  )
})

height.HPDI <- apply(sim.height, 2, HPDI, prob=0.89)
height.mean <- apply(sim.height, 2, mean)

# the plot
plot(height ~ log(weight), data=d, col=col.alpha(rangi2, 0.6))
lines(log(weight.seq), mu.mean)                                      # the MAP regression line
shade( mu.HPDI, log(weight.seq))                                     # draw HPDI region around the regression line
shade( height.HPDI, log(weight.seq))                                 # draw HPDI region for the simulated heights
```

![](chapter4_Ex_files/figure-markdown_github/unnamed-chunk-10-1.png)

Compared to the model above fit to only the children and also compared to the models earlier in the chapter using the full data set with polynomial regression, this model seems to perform quite well on the data.

1.  Let's make the same plot without using the logarithmic scale.

``` r
plot(height ~ weight, data=d, col=col.alpha(rangi2, 0.6))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade( height.HPDI, weight.seq)
```

![](chapter4_Ex_files/figure-markdown_github/unnamed-chunk-11-1.png)

Given the last two plots, I'd say the colleague was right: The logarithm of body weight scales very well with height.
