Binomial Regression
================
Corrie
October 4, 2018

Logistic Regression
-------------------

The chimpanzee data: Do chimpanzee pick the more social option?

``` r
library(rethinking)
data(chimpanzees)
d <- chimpanzees
```

The important variables are the variable `condition`, indicating if another chimpanzee sits opposite (1) the table or not (0) and the variable `prosocial_left` which indicates if the left lever is the more social option. These two variables will be used to predict if the chimpanzees pull the left lever or not (`pulled_left`).

The implied model is:

![\\begin{align\*}
L\_i &\\sim \\text{Binomial}(1, p\_i)\\\\
\\text{logit}(p\_i) &= \\alpha + (\\beta\_P + \\beta\_{PC}C\_i)P\_i \\\\
\\alpha &\\sim \\text{Normal}(0, 10) \\\\
\\beta\_P &\\sim \\text{Normal}(0, 10) \\\\
\\beta\_{PC} &\\sim \\text{Normal}(0, 10) 
\\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0AL_i%20%26%5Csim%20%5Ctext%7BBinomial%7D%281%2C%20p_i%29%5C%5C%0A%5Ctext%7Blogit%7D%28p_i%29%20%26%3D%20%5Calpha%20%2B%20%28%5Cbeta_P%20%2B%20%5Cbeta_%7BPC%7DC_i%29P_i%20%5C%5C%0A%5Calpha%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C%2010%29%20%5C%5C%0A%5Cbeta_P%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C%2010%29%20%5C%5C%0A%5Cbeta_%7BPC%7D%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C%2010%29%20%0A%5Cend%7Balign%2A%7D "\begin{align*}
L_i &\sim \text{Binomial}(1, p_i)\\
\text{logit}(p_i) &= \alpha + (\beta_P + \beta_{PC}C_i)P_i \\
\alpha &\sim \text{Normal}(0, 10) \\
\beta_P &\sim \text{Normal}(0, 10) \\
\beta_{PC} &\sim \text{Normal}(0, 10) 
\end{align*}")

We'll write this as a `map()` model but will start with two simpler models with less predictors, starting with one model that has only an intercept and no predictor variables:

``` r
m10.1 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 10)
  ),
  data=d
)
precis(m10.1)
```

      Mean StdDev 5.5% 94.5%
    a 0.32   0.09 0.18  0.46

This implies a MAP probability of pulling the left lever of

``` r
logistic(0.32)
```

    [1] 0.5793243

with a 89% interval of

``` r
logistic( c(0.18, 0.46))
```

    [1] 0.5448789 0.6130142

Next the models using the predictors:

``` r
m10.2 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + bp*prosoc_left,
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 10)
  ),
  data=d
)
m10.3 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + (bp + bpC*condition)*prosoc_left,
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data=d
)

(cp <- compare(m10.1, m10.2, m10.3) )
```

           WAIC pWAIC dWAIC weight   SE  dSE
    m10.2 680.5     2   0.0   0.69 9.20   NA
    m10.3 682.2     3   1.7   0.29 9.43 0.73
    m10.1 688.0     1   7.5   0.02 7.17 6.10

The model without the interaction seems to fare better even though the third model best reflects the structure of the experiment.

``` r
plot( cp , 
      xlim=c( min( cp@output$WAIC - cp@output$SE) , 
              max(cp@output$WAIC + cp@output$SE ) ) )
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-6-1.png)

Note also that the difference has a small standard error, so the order of the models wouldn't easily change.

Let's have a look at the third model to understand why it performs poorly compared with the second one:

``` r
precis(m10.3)
```

         Mean StdDev  5.5% 94.5%
    a    0.05   0.13 -0.15  0.25
    bp   0.61   0.23  0.25  0.97
    bpC -0.10   0.26 -0.53  0.32

``` r
plot( precis(m10.3) )
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-8-1.png)

The interaction variable has a rather wide posterior. Let's have a closer look at the parameter `bp` for the effect of the prosocial option. We have to distinguish between the absolute and relative effect.

Changing the predictor `prosoc_left` from 0 to 1 increases the log-odds of pulling the left-hand lever by 0.61. This implies the odds are multiplied by:

``` r
exp(0.61)
```

    [1] 1.840431

That is, the odds increase by 84%. This is the relative effect. The relative effect depend strongly on the other parameter als well. If we assume for example an ![\\alpha](https://latex.codecogs.com/png.latex?%5Calpha "\alpha") value of 4 then the probability for a pull, ignoring everything else:

``` r
logistic(4)
```

    [1] 0.9820138

is already quite high. Adding then the increase of 0.61 (relative increase of 84%) changes this to

``` r
logistic(4 + 0.61)
```

    [1] 0.9901462

In this example, the absolute effect would thus be small.

Let's plot the absolute effect:

``` r
# dummy data for predictions across treatments
d.pred <- data.frame(
  prosoc_left = c(0, 1, 0, 1), # right/left/right/left
  condition = c(0, 0, 1, 1)    # control/control/partner/partner
)

# build prediction ensemble
chimp.ensemble <- ensemble(m10.1, m10.2, m10.3, data=d.pred)

# summarize
pred.p <- apply(chimp.ensemble$link, 2, mean)
pred.p.PI <- apply(chimp.ensemble$link, 2, PI)
```

``` r
plot( 0, 0, type="n", xlab="prosoc_left/condition",
      ylab="proportion pulled left",
      ylim=c(0,1), xaxt="n", xlim=c(1,4) )
axis(1, at=1:4, labels=c("0/0", "1/0", "0/1", "1/1"))

p <- by( d$pulled_left,
    list(d$prosoc_left, d$condition, d$actor ), mean)

for ( chimp in 1:7) 
  lines( 1:4, as.vector(p[,,chimp]),
         col="royalblue4", lwd=1.5)

lines( 1:4, pred.p )
shade(pred.p.PI, 1:4)
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-13-1.png)

Compare the MAP model with a MCMC Stan model:

``` r
# clean NAs from the data
d2 <- d
d2$recipient <- NULL

# re-use map fit to get the formula
m10.3stan <- map2stan(m10.3, data=d2, iter=1e4, warmup=1000 )
precis(m10.3stan)
```

``` r
precis(m10.3stan)
```

         Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
    a    0.05   0.13      -0.15       0.25  5127    1
    bp   0.62   0.23       0.25       0.98  4138    1
    bpC -0.10   0.26      -0.52       0.32  4797    1

The numbers are almost identical with the MAP model and we can check the pairs plot to see that the posterior is indeed well approximated by a quadratic.

``` r
pairs(m10.3stan)
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-16-1.png)

We saw in the plot of the average proportions pulled left above that some chimpanzees have a preference for pulling the left or right lever. One even always pulled the left lever. A factor here might be the handedness of the chimp. One thing we can do, is to fit an intercept for each individula:

![\\begin{align\*}
L\_i &\\sim \\text{Binomial}(1, p\_i)\\\\
\\text{logit}(p\_i) &= \\alpha\_{\\text{ACTOR}\[i\]} + (\\beta\_P + \\beta\_{PC}C\_i)P\_i \\\\
\\alpha &\\sim \\text{Normal}(0, 10) \\\\
\\beta\_P &\\sim \\text{Normal}(0, 10) \\\\
\\beta\_{PC} &\\sim \\text{Normal}(0, 10) 
\\end{align\*}](https://latex.codecogs.com/png.latex?%5Cbegin%7Balign%2A%7D%0AL_i%20%26%5Csim%20%5Ctext%7BBinomial%7D%281%2C%20p_i%29%5C%5C%0A%5Ctext%7Blogit%7D%28p_i%29%20%26%3D%20%5Calpha_%7B%5Ctext%7BACTOR%7D%5Bi%5D%7D%20%2B%20%28%5Cbeta_P%20%2B%20%5Cbeta_%7BPC%7DC_i%29P_i%20%5C%5C%0A%5Calpha%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C%2010%29%20%5C%5C%0A%5Cbeta_P%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C%2010%29%20%5C%5C%0A%5Cbeta_%7BPC%7D%20%26%5Csim%20%5Ctext%7BNormal%7D%280%2C%2010%29%20%0A%5Cend%7Balign%2A%7D "\begin{align*}
L_i &\sim \text{Binomial}(1, p_i)\\
\text{logit}(p_i) &= \alpha_{\text{ACTOR}[i]} + (\beta_P + \beta_{PC}C_i)P_i \\
\alpha &\sim \text{Normal}(0, 10) \\
\beta_P &\sim \text{Normal}(0, 10) \\
\beta_{PC} &\sim \text{Normal}(0, 10) 
\end{align*}")

 We fit this with MCMC since it will turn out to have some skew in its posterior distribution.

``` r
m10.4 <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left,
    a[actor] ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data=d2, chains=2, iter=2500, warmup=500
)
```

Number of actors:

``` r
unique(d$actor)
```

    [1] 1 2 3 4 5 6 7

``` r
precis(m10.4, depth=2)
```

          Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
    a[1] -0.73   0.27      -1.17      -0.31  3365    1
    a[2] 10.79   5.17       3.35      18.02  1566    1
    a[3] -1.06   0.28      -1.48      -0.59  3205    1
    a[4] -1.05   0.28      -1.47      -0.57  2697    1
    a[5] -0.74   0.27      -1.17      -0.30  3095    1
    a[6]  0.23   0.27      -0.24       0.63  3253    1
    a[7]  1.81   0.39       1.21       2.45  3528    1
    bp    0.84   0.27       0.40       1.27  2005    1
    bpC  -0.14   0.31      -0.61       0.38  2902    1

This posterior is not entirely Gaussian:

``` r
post <- extract.samples( m10.4)
str(post)
```

    List of 3
     $ a  : num [1:4000, 1:7] -0.434 -1.233 -0.671 -1.206 -0.585 ...
     $ bp : num [1:4000(1d)] 0.523 0.906 1.16 1.205 0.594 ...
     $ bpC: num [1:4000(1d)] -0.356 -0.2 -0.413 -0.285 0.193 ...

``` r
dens(post$a[,2])
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-21-1.png)

Plotting the predictions for each actor:

``` r
par(mfrow=c(4, 2))
for (chimp in 1:7) {
  d.pred <- list(
    pulled_left = rep(0, 4),       # empty outcomes
    prosoc_left = c(0, 1, 0, 1),   # right/left/right/left
    condition = c(0, 0, 1, 1),     # control/control/partner/partner
    actor = rep(chimp, 4)
  )
  link.m10.4 <- link( m10.4, data=d.pred)
  pred.p <- apply( link.m10.4, 2, mean)
  pred.p.PI <- apply( link.m10.4, 2, PI )
  
  plot(0, 0, type="n", xlab="prosoc_left/condition",
       ylab="proportion pulled left",
       ylim=c(0,1), xaxt="n",
       xlim=c(1, 4), yaxp=c(0,1,2) )
  axis(1, at=1:4, labels=c("0/0", "1/0", "0/1", "1/1"))
  mtext(paste( "actor", chimp ))
  
  p <- by( d$pulled_left,
           list(d$prosoc_left, d$condition, d$actor), mean )
  lines( 1:4, as.vector(p[,,chimp]), col="royalblue4", lwd=2)
  
  lines(1:4, pred.p)
  shade(pred.p.PI, 1:4)
}
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-22-1.png)

Aggregated binomial
-------------------

### Chimpanzees

Instead of predicting the likelihood for a single pull (0 or 1), we can also predict the counts, i.e. how likely is it that an actor pulls left `x` times out of 18 trials.

``` r
data(chimpanzees)
d <- chimpanzees
d.aggregated <- aggregate( d$pulled_left,
                           list(prosoc_left=d$prosoc_left,
                                condition=d$condition,
                                actor=d$actor),
                           sum)
knitr::kable( head(d.aggregated,  8) )
```

|  prosoc\_left|  condition|  actor|    x|
|-------------:|----------:|------:|----:|
|             0|          0|      1|    6|
|             1|          0|      1|    9|
|             0|          1|      1|    5|
|             1|          1|      1|   10|
|             0|          0|      2|   18|
|             1|          0|      2|   18|
|             0|          1|      2|   18|
|             1|          1|      2|   18|

``` r
m10.5 <- map(
  alist(
    x ~ dbinom( 18, p ),
    logit(p) <- a + (bp + bpC*condition)*prosoc_left,
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data = d.aggregated
)
precis(m10.5)
```

         Mean StdDev  5.5% 94.5%
    a    0.05   0.13 -0.15  0.25
    bp   0.61   0.23  0.25  0.97
    bpC -0.10   0.26 -0.53  0.32

Compare with the same model with non-aggregated data:

``` r
precis(m10.3stan)
```

         Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
    a    0.05   0.13      -0.15       0.25  5127    1
    bp   0.62   0.23       0.25       0.98  4138    1
    bpC -0.10   0.26      -0.52       0.32  4797    1

We get the same estimates.

### Graduate school admissions

``` r
data("UCBadmit")
d <- UCBadmit
knitr::kable(d)
```

| dept | applicant.gender |  admit|  reject|  applications|
|:-----|:-----------------|------:|-------:|-------------:|
| A    | male             |    512|     313|           825|
| A    | female           |     89|      19|           108|
| B    | male             |    353|     207|           560|
| B    | female           |     17|       8|            25|
| C    | male             |    120|     205|           325|
| C    | female           |    202|     391|           593|
| D    | male             |    138|     279|           417|
| D    | female           |    131|     244|           375|
| E    | male             |     53|     138|           191|
| E    | female           |     94|     299|           393|
| F    | male             |     22|     351|           373|
| F    | female           |     24|     317|           341|

The data set contains only 12 rows but since it is aggregated, it actually represents 4526 applications. The goal is to evaluate whether the data contains evidence for gender bias in admission.

We will fit two models: One using gender as a predictor for `admit` and one modelling `admit` as a constant, ignoring gender.

``` r
d$male <- ifelse( d$applicant.gender == "male", 1, 0 )

m10.6 <- map(
  alist(
    admit ~ dbinom( applications, p ),
    logit(p) <- a + bm*male,
    a ~ dnorm(0, 10),
    bm ~ dnorm(0, 10)
  ),
  data=d
)

m10.7 <- map(
  alist(
    admit ~ dbinom( applications, p),
    logit(p) <- a,
    a ~ dnorm(0, 10)
  ),
  data=d
)
```

``` r
(cp <- compare( m10.6, m10.7 ) )
```

            WAIC pWAIC dWAIC weight    SE   dSE
    m10.6 5955.0   2.1   0.0      1 34.88    NA
    m10.7 6046.2   0.9  91.2      0 29.94 19.15

The model using male as a predictor performs better than without. The comparison suggests that gender actually matters a lot:

``` r
plot( cp , 
      xlim=c( min( cp@output$WAIC - cp@output$SE) , 
              max(cp@output$WAIC + cp@output$SE ) ) )
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-29-1.png)

In which way does it matter?

``` r
precis( m10.6 )
```

        Mean StdDev  5.5% 94.5%
    a  -0.83   0.05 -0.91 -0.75
    bm  0.61   0.06  0.51  0.71

Being male does improve the chances of being admitted. The relative difference is `exp(0.61) =` 1.8404314. This means that a male applicant's odds are 184% of a female applicant. Let's get the absolute scale, which is more important:

``` r
post <- extract.samples( m10.6 )
p.admit.male <- logistic( post$a + post$bm )
p.admit.female <- logistic( post$a )

diff.admit <- p.admit.male - p.admit.female

quantile( diff.admit, c(0.025, 0.5, 0.975 ))
```

         2.5%       50%     97.5% 
    0.1130350 0.1418460 0.1698431 

Thus the median estimate of the male advantage is about 14%.

``` r
dens( diff.admit )
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-32-1.png)

``` r
postcheck( m10.6, n=1e4 , col="royalblue4")

# draw lines connecting points from same dept
for ( i in 1:6 ){
  x <- 1 + 2*(i-1)
  y1 <- d$admit[x]/d$applications[x]       # male
  y2 <- d$admit[x+1]/d$applications[x+1]   # female
  lines( c(x, x+1), c(y1, y2), col="royalblue4", lwd=2)
  text(x+0.5, (y1 + y2)/2 + 0.05, d$dept[x], cex=0.8, col="royalblue4")
}
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-33-1.png)

The first point of a line is the admission rate for males while the second point of a line is the admission rate for females. The expeted predictions are the black open points and the black crosses indicate the 89% interval of the expectations. The plot shows that only two departments (C and E) and lower admission rates for females. How come we get such a bad prediction?

The problem: Male and female applicants don't apply to the same departments and departments have different admission rates. Department A has much higer admission rates than department F and female applicants apply more often to F than to A.

We will build a model that uses a unique intercept per department.

``` r
# make index
d$dept_id <- coerce_index( d$dept )

# model with unique intercept for each dept
m10.8 <- map(
  alist(
    admit ~ dbinom( applications, p),
    logit(p) <- a[dept_id],
    a[dept_id] ~ dnorm(0, 10)
  ),
  data=d
)

m10.9 <- map(
  alist(
    admit ~ dbinom( applications, p),
    logit(p) <- a[dept_id] + bm*male,
    a[dept_id] ~ dnorm(0, 10),
    bm ~ dnorm(0, 10)
  ),
  data=d
)
```

We then compare all three models:

``` r
(cp <- compare( m10.6, m10.7, m10.8, m10.9 ) )
```

            WAIC pWAIC dWAIC weight    SE   dSE
    m10.8 5201.0   6.0   0.0   0.59 56.99    NA
    m10.9 5201.8   7.1   0.8   0.41 57.22  2.47
    m10.6 5954.9   2.0 753.9   0.00 35.03 48.46
    m10.7 6046.2   0.9 845.2   0.00 29.93 52.31

As a plot:

``` r
plot( cp , 
      xlim=c( min( cp@output$WAIC - cp@output$SE) , 
              max(cp@output$WAIC + cp@output$SE ) ) )
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-36-1.png)

The two new models with the unique intercepts perform much better. Now, the model without `male` is ranked first but the difference between the first two models is tiny. Both models got about half the Akaike weight so you could call it a tie.

So how does gender now affect admission?

``` r
precis( m10.9, depth=2 )
```

          Mean StdDev  5.5% 94.5%
    a[1]  0.68   0.10  0.52  0.84
    a[2]  0.64   0.12  0.45  0.82
    a[3] -0.58   0.07 -0.70 -0.46
    a[4] -0.61   0.09 -0.75 -0.48
    a[5] -1.06   0.10 -1.22 -0.90
    a[6] -2.62   0.16 -2.88 -2.37
    bm   -0.10   0.08 -0.23  0.03

The estimate for `bm` has changed direction, meaning it now estimates that being male is a disadvantage! The estimate becomes `exp(-0.1) =` 0.9048374. So a male has about 90% the odds of admission as a female.

Let's do the posterior check again as before:

``` r
postcheck( m10.9, n=1e4 , col="royalblue4")

# draw lines connecting points from same dept
for ( i in 1:6 ){
  x <- 1 + 2*(i-1)
  y1 <- d$admit[x]/d$applications[x]       # male
  y2 <- d$admit[x+1]/d$applications[x+1]   # female
  lines( c(x, x+1), c(y1, y2), col="royalblue4", lwd=2)
  text(x+0.5, (y1 + y2)/2 + 0.05, d$dept[x], cex=0.8, col="royalblue4")
}
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-38-1.png)

The predictions fit much better than before.

Let's also check the quadratic approximation. In the example with the chimpanzees, unique intercepts were a problem for quadratic approximations, so let's check how the compare to a Stan model:

``` r
dstan <- d[, c("admit", "applications", "male", "dept_id")]
m10.9stan <- map2stan( m10.9, data=dstan,
                       chains=2, iter=2500, warmup=500)
precis(m10.9stan, depth=2)
```

``` r
pairs(m10.9stan)
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-40-1.png)

All the posterior distributions are pretty much Gaussian, a quadratic approximation thus gives good estimates.

Fitting binomial regression with `glm`
--------------------------------------

The following code yields similar results as the map approach for the aggregated binomial.

``` r
m10.7glm <- glm( cbind( admit, reject) ~ 1, data=d, family=binomial)
m10.6glm <- glm( cbind( admit, reject) ~ male, data=d, family=binomial)
m10.8glm <- glm( cbind( admit, reject) ~ dept, data=d, family=binomial)
m10.9glm <- glm( cbind( admit, reject) ~ male + dept, data=d, 
                 family=binomial )
precis(m10.9glm)
```

                 Mean StdDev  5.5% 94.5%
    (Intercept)  0.68   0.10  0.52  0.84
    male        -0.10   0.08 -0.23  0.03
    deptB       -0.04   0.11 -0.22  0.13
    deptC       -1.26   0.11 -1.43 -1.09
    deptD       -1.29   0.11 -1.46 -1.13
    deptE       -1.74   0.13 -1.94 -1.54
    deptF       -3.31   0.17 -3.58 -3.03

Compare with the `map()` model:

``` r
precis(m10.9stan, depth=2)
```

          Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
    a[1]  0.68   0.10       0.52       0.83  1950    1
    a[2]  0.64   0.12       0.45       0.82  2281    1
    a[3] -0.58   0.07      -0.70      -0.46  4000    1
    a[4] -0.61   0.09      -0.75      -0.47  2889    1
    a[5] -1.06   0.10      -1.21      -0.89  4000    1
    a[6] -2.64   0.16      -2.89      -2.39  4000    1
    bm   -0.10   0.08      -0.22       0.03  1635    1

Note that the departments are coded differently: the intercept in the `glm` model corresponds to `a[1]` in the Stan model and `deptB` in the `glm` model corresponds to the difference from department A to B, that is `a[2]-a[1]` in the Stan model.

To use `glm()` for a non-aggregated model:

``` r
m10.4glm <- glm(
  pulled_left ~ as.factor(actor) + prosoc_left*condition  -condition,
  data=chimpanzees, family=binomial
)
precis(m10.4glm)
```

                           Mean StdDev     5.5%   94.5%
    (Intercept)           -0.73   0.27    -1.16   -0.30
    as.factor(actor)2     18.95 754.98 -1187.65 1225.55
    as.factor(actor)3     -0.30   0.35    -0.86    0.25
    as.factor(actor)4     -0.30   0.35    -0.86    0.25
    as.factor(actor)5      0.00   0.34    -0.55    0.55
    as.factor(actor)6      0.94   0.35     0.38    1.50
    as.factor(actor)7      2.48   0.45     1.76    3.20
    prosoc_left            0.82   0.26     0.40    1.24
    prosoc_left:condition -0.13   0.30    -0.61    0.34

We need to use `-condition` to remove the main effect of condition.

We can also use `glimmer()` to get code corresponding to a `map` or `map2stan` model.

``` r
glimmer( pulled_left ~ prosoc_left*condition -condition,
         data=chimpanzees, family=binomial)
```

    alist(
        pulled_left ~ dbinom( 1 , p ),
        logit(p) <- Intercept +
            b_prosoc_left*prosoc_left +
            b_prosoc_left_X_condition*prosoc_left_X_condition,
        Intercept ~ dnorm(0,10),
        b_prosoc_left ~ dnorm(0,10),
        b_prosoc_left_X_condition ~ dnorm(0,10)
    )

Note that `glm` uses flat priors by default which can lead to nonsense estimates. Consider for example the following example:

``` r
# almost perfectly associated
y <- c( rep(0, 10), rep(1, 10))
x <- c(rep(-1, 9), rep(1, 11))
m.bad <- glm( y ~ x, data=list(x=x, y=y), family=binomial)
precis(m.bad)
```

                 Mean  StdDev     5.5%   94.5%
    (Intercept) -9.13 2955.06 -4731.89 4713.63
    x           11.43 2955.06 -4711.33 4734.19

The estimates would suggest there is no association between `x` and `y` even though there is a strong association. A weak prior helps:

``` r
m.good <- map(
  alist(
    y ~ dbinom(1, p),
    logit(p) <- a + b*x,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10)
  ),
  data=list(x=x, y=y)
)
precis(m.good)
```

       Mean StdDev  5.5% 94.5%
    a -1.73   2.78 -6.16  2.71
    b  4.02   2.78 -0.42  8.45

Since the uncertainty is not symmetric in this case, the quadratic assumption is misleading. Even better would be MCMC samples:

``` r
m.better <- map2stan( m.good)
pairs(m.better)
```

![](chapter10a_files/figure-markdown_github/unnamed-chunk-47-1.png)
