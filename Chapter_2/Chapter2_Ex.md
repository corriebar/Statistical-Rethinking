Chapter 2 - Exercises
================
Corrie
2020-04-20

These are my solutions to the practice questions of chapter 2, *Small
Words and Large Worlds*, of the book “Statistical Rethinking” (version
2) by Richard McElreath.

## Easy.

**2E1.** Which of the expressions below correspond to the statement:
*the probability of rain on Monday*?

1)  ~~Pr(rain)~~
2)  **Pr(rain | Monday)**
3)  ~~Pr(Monday | rain)~~
4)  **Pr(rain, Monday) / Pr(Monday)**

Statement (4) is equivalent to (2) by Bayes theorem using joint
probability.

**2E2.** Which of the following statements corresponds to the
expression: Pr(Monday | rain )?

1)  ~~The probability of rain on Monday.~~
2)  ~~The probability of rain, given that it is Monday.~~
3)  **The probability that it is Monday, given that it is raining.**
4)  ~~The probability that it is Monday and that it is raining.~~

**2E3.** Which of the expressions below correspond to the statement:
*the probability that it is Monday, given that it is raining*?

1)  **Pr(Monday | rain)**
2)  ~~Pr(rain | Monday)~~
3)  ~~Pr(rain | Monday) Pr(Monday)~~
4)  **Pr(rain | Monday) Pr(Monday) / Pr(rain)**
5)  ~~Pr(Monday | rain) Pr(rain) / Pr(Monday)~~

Statement (4) is equivalent to (1) by Bayes theorem.

**2E4.**

> “PROBABILITY DOES NOT EXIST”
> 
> <footer>
> 
> — de Finetti, 1973
> 
> </footer>

Discuss the globe tossing example from the chapter, in light of this
statement. What does it mean to say “the probability of water is 0.7”?

In the globe tossing example, it would theoretically be thinkable to use
the position of the globe when throwing, the way it was thrown
(direction, force etc) and the way it is caught again to exactly compute
where (water or land) the globe would land using physics and stuff. In
that sense, there is neither randomness involved nor probability.
Knowing all details, it is a deterministic process. However, since we’re
too lazy to do all these detailed computations and can’t be bothered to
correctly measure the amount of land versus water on the globe, we use
the construct of probability and randomness to find out the approximate
water/land proportions.

## Medium.

**2M1.** Recall the globe tossing model from the chapter. Compute and
plot the grid approximate posterior distributions for each of the
following sets of observations. Assume a uniform prior for \(p\).

1)  W, W, W
2)  W, W, W, L
3)  L, W, W, L, W, W, W

I use a small function to compute and plot the posterior computations:

``` r
compute_posterior <- function(w, n, prior) {
  num_pts = length(prior)
  p_grid = seq(from=0, to=1, length.out=num_pts)
  likelihood <- dbinom(w, size=n, prob=p_grid)
  
  # compute product of likelihood and prior (unstandardized posterior)
  unstd.posterior <- likelihood * prior
  
  # standardize posterior
  posterior <- unstd.posterior / sum(unstd.posterior)
  
  plot(p_grid, posterior, type="l", xlab="probability of water", ylab="posterior probability")
}
```

Using a uniform prior:

``` r
uniform_prior <- rep(1, 1000)
```

![](Chapter2_Ex_files/figure-gfm/unnamed-chunk-3-1.svg)<!-- -->

**2M2.** Now assume a prior for \(p\) that is equal to zero when
\(p < 0.5\) and a positive constant when \(p \ge 0.5\).

``` r
p_grid = seq(from=0, to=1, length.out=1000)
step_prior <- ifelse(p_grid < 0.5, 0, 2)
```

![](Chapter2_Ex_files/figure-gfm/unnamed-chunk-5-1.svg)<!-- -->

**2M3.** Suppose there are two globes, one for Earth and one for Mars.
The Earth globe is 70% covered in water and the mars globe is 100% land.
One of these globes was tossed in the air and produced a “land”
observation. Each globe was equally likely to be tossed. Compute the
posterior probability that the globe was the Earth, conditional on
seeing “land”.

We can compute the probability by simply simulating a bunch of tosses:

  - Are we throwing the Earth or Mars globe? Use `rbinom()` with
    probability 0.5.
  - Generate a vector with the corresponding probabilities for Earth and
    Mars.
  - Use this probability vector to sample some tosses.
  - Restricting to tosses that landed on “land”, how many came from the
    Earth globe?

<!-- end list -->

``` r
trials <- 100000
earth <- rbinom(trials, size=1, p=0.5)
water_prob <- ifelse(earth == 1, 0.7, 0)
water <- rbinom(trials, size=1, p=water_prob)
df <- data.frame( earth = earth,
                  water = water )
df %>%
  filter(water == 0) %>%
  summarize(prob_earth = mean(earth))
```

| prob\_earth |
| ----------: |
|   0.2302113 |

**2M4.** Suppose you hae a deck with only three cards. Each card has two
sides, and each side is either black or white. One card has two black
sides, the second card has one black, one white side and the third card
has two white sides. Now these cards are shuffled in a bag, one is drawn
and placed flat on a table. A black side is showing up. What is the
color of the other side?

The exercise asks to use the counting method, as used in the chapter,
but I will instead use a simulation approach:

  - Randomly draw one card from a bag a few times using `sample()`.
  - Depending on the card, the probability that the top facing side is
    black is either 1, 0.5 or 0.
  - Sample some draws.
  - Restricting to draws where the top side is black, how many of them
    where from card 1 (the card with two black sides)?

<!-- end list -->

``` r
trials <- 10000
card <- sample(1:3, size=trials, replace = TRUE )
black_prob <- case_when(card == 1 ~ 1,
                     card == 2 ~ 0.5,
                     card == 3 ~ 0)
black <- rbinom(trials, size=1, prob=black_prob)
df <- data.frame( card = card,
                  black = black )

df %>%
  filter( black == 1) %>%
  summarize( prob_other_side_black = mean( card == 1))
```

| prob\_other\_side\_black |
| -----------------------: |
|                 0.664843 |

The probability that the other side is also black is 2/3 (~66%).

**2M5.** Now suppose there are four cards: B/B, B/W, W/W, and another
B/B. Again, a card is drawn from the back and a black side appears. What
is the probability that the other side is black?

I’m using a simulation approach again by adding another card with a
probability of 1 to have a black side.

``` r
trials <- 10000
card <- sample(1:4, size=trials, replace = TRUE )
black_prob <- case_when(card == 1 ~ 1,
                     card == 2 ~ 0.5,
                     card == 3 ~ 0,
                     card == 4 ~ 1)
black <- rbinom(trials, size=1, prob=black_prob)
df <- data.frame( card = card,
                  black = black )

df %>%
  filter( black == 1) %>%
  summarize( prob_other_side_black = mean( card %in% c(1, 4) ))
```

| prob\_other\_side\_black |
| -----------------------: |
|                0.8077229 |

The probability that the other side is also black is ~80%.

**2M6.** Imagine that black ink is heavy, and so cards with black sides
are heavier that cards with white sides. Assume again there are three
cards: B/B, B/W, and W/W and assume that for every way to pull B/B there
are two ways to pull B/W and 3 ways to pull the W/W card. Again, a card
is pulled and a black side appears face up. What is the probability that
the other side is black?

Using the simulation approach again, we now have to adjust how we sample
from the three cards:

``` r
trials <- 10000
sample_prob <- c(1, 2, 3)
sample_prob <- sample_prob / sum(sample_prob)
card <- sample(1:3, size=trials, replace = TRUE, prob = sample_prob )
black_prob <- case_when(card == 1 ~ 1,
                     card == 2 ~ 0.5,
                     card == 3 ~ 0)
black <- rbinom(trials, size=1, prob=black_prob)
df <- data.frame( card = card,
                  black = black )

df %>%
  filter( black == 1) %>%
  summarize( prob_other_side_black = mean( card == 1))
```

| prob\_other\_side\_black |
| -----------------------: |
|                0.5137615 |

Now, the probability that the other side is also black is around 50%.

**2M7.** Assume again the original card problem. This time, before
looking at the other side, we draw another card from the bag and lay it
face up on the table. The face that is shown on the new card is white.
What is the probability that the first card is black on the other side?

This simulation is a bit more complex, since we need to draw two cards.
To do so, I list all card combos that can appear when drawing two cards
(each combo is equally likely) and sample from these card combos. For
each card combo, I then compute the probability that the first card is
facing up black and that the second card is facing up white. Then I
restrict to draws where the first card is black and the second is white
and count for how many of these the first card is black on both sides.

``` r
trials <- 100000
card_combos <- c("12", "13", "21", "23", "31", "32")
cards <- sample(card_combos, size=trials, replace = TRUE )
black_prob <- case_when(str_starts(cards, "1") ~ 1,
                     str_starts(cards, "2") ~ 0.5,
                     str_starts(cards, "3") ~ 0)

white_prob <- case_when(str_ends(cards, "1") ~ 0,
                     str_ends(cards, "2") ~ 0.5,
                     str_ends(cards, "3") ~ 1)
first_black <- rbinom(trials, size=1, prob=black_prob)
second_white <- rbinom(trials, size=1, prob=white_prob)
df <- data.frame( first_card = substr(cards, 1, 1) ,
                  second_card = substr(cards, 2, 2),
                  first_black = first_black,
                  second_white = second_white)

df %>%
  filter( first_black == 1 &
            second_white == 1) %>%
  summarize( prob_other_side_black = mean( first_card == "1"))
```

| prob\_other\_side\_black |
| -----------------------: |
|                0.7502931 |

The probability that the first card is black on both sides is then 75%.

## Hard.

**2H1.** Suppose there are two species of panda bear. Both are equally
common, they look exactly the same and there’s no genetic test yet. They
only differ in family size: Species A gives birth to twins 10% of the
time and Species B births twins 20% of the time (otherwise it’s single
kids). Imagine you have a female panda that just gave birth to twins.
What is the probability that her next birth will also be twins?

Let’s simulate:

  - First, randomly sample the species and assign their probabilities.
  - Then we sample the first births,
  - and the second births.
  - Last, we take the proportions of twins in the second births, given
    that the first one is twins.

<!-- end list -->

``` r
n <- 1e6
species <- sample( c("A", "B"), size=n, replace = TRUE)
twin_prob <- ifelse(species == "A", 0.1, 0.2 )
birth1 <- rbinom(n, size=1, prob=twin_prob)
birth2 <- rbinom(n, size=1, prob=twin_prob)

df <- data.frame(species = species,
                 birth1 = birth1,
                 birth2 = birth2)

df %>% filter( birth1 == 1 ) %>%
  summarise(prob_next_birth_twins = mean( birth2 ))
```

| prob\_next\_birth\_twins |
| -----------------------: |
|                0.1675398 |

The probability that the next birth will be twins again is around 16%.
Note that this is not in any way making a statement about the
probability which species it is. But since the probability is either 10%
or 20% for species A and B respectively, we expect the posterior
probability to lie between these two values. Furthermore, since we
already observed one twin birth, it makes sense that the posterior
probability is closer to 20% (implying that it is more likely that this
panda is from species A).

**2H2.** Now compute the probability that the panda we have is from
species A, given that we observed one twin birth.

We can use the data generated above to answer this question:

``` r
df %>%
  filter( birth1 == 1 ) %>%
  summarise( prob_species_A = mean( species == "A"))
```

| prob\_species\_A |
| ---------------: |
|        0.3355596 |

The probability the panda is from species A, having observed one twin
birth, is around 33%. Since we already observed one twin birth, the
probability that the panda is from species B increased.

**2H3.** Suppose the same panda mother has a second birth, this time to
a singleton infant. What is the probability that this pandas is from
species A?

``` r
df %>%
  filter( birth1 == 1 &
            birth2 == 0 ) %>%
  summarise( prob_species_A = mean( species == "A"))
```

| prob\_species\_A |
| ---------------: |
|        0.3623072 |

The probability increased slightly to ~35%. Since single births are the
more common event, it doesn’t change the probability as much.

**2H4.** Suppose now, there is a new genetic test that can identify the
species of our mother panda. The test is imperfect though:

  - The probability it correctly identifies a species A panda is 0.8.
  - The probability it correctly identifies a species B panda is 0.65.

The test for our mother panda is positive for species A. Ignoring the
information from the births, what is the probability our panda is
species A?

``` r
test_A_prob <- ifelse(species == "A", 0.8, 1 - 0.65)
test_A <- rbinom(n, size=1, prob=test_A_prob)
df <- df %>%
  mutate(test_A = test_A) 

df %>%
  filter(test_A == 1 ) %>%
  summarise(prob_species_A = mean( species == "A"))
```

| prob\_species\_A |
| ---------------: |
|        0.6958369 |

Without considering the births, the probability our panda is species A
after testing positive for species A is around 69%.

What is the probability she is species A considering both the births and
the test result?

``` r
df %>%
  filter(birth1 == 1 &
           birth2 == 0 &
           test_A == 1 ) %>%
  summarise(prob_species_A = mean( species == "A" ))
```

| prob\_species\_A |
| ---------------: |
|        0.5624583 |

The probability our panda is species A considering both the test and the
births is around 56%.

<small>[Full
code.](https://github.com/corriebar/Statistical-Rethinking/blob/master/Chapter_2/Chapter2_Ex.Rmd)<small>
