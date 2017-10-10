# ST 516 - Homework 2 by Paul ReFalo  ========

# 1. (6 points) Simulation and estimation in R
# (a) The R function runif() generates random numbers from the continuous Uniform distribution. 
# For example, runif(1, min = 0, max = 1) generates one random number from a Uniform(0,1) distribution, 
# i.e. one random number between 0 and 1 with uniform probability.

# ***************************************************************************************************
# 1a. Generates a random number in Uniform distribution for one variable from 0 to 1
# an example
runif(1, min = 0, max = 1)

# another example
runif(1, min = -1, max = 1)

# ***************************************************************************************************

# i. Create a histogram of 10000 draws from a Uniform(0, 1) distribution.
draws10000 <- runif(10000, min = 0, max = 1)   # let x equal 10000 draws from 0 to 1 of uniform distribution
hist(draws10000, plot = TRUE, main = "Histogram of 10000 draws from 0 to 1", xlab = "Range")

# ***************************************************************************************************

# ii. Estimate the probability a random variable with this distribution is between 0.5 and 0.75

events <- 100000

x <- runif(events, min = 0.5, max = 0.75)
hist(x, plot = TRUE, main = "Estimate probability from 0.5 to 0.75", xlab = "Range")

# The average value should be close to the mid-point or 0.50 + (0.75 - 0.50) / 2 = 0.625 
# and it is: one result with 100000 events was 0.6247866
pEstimate <- sum(x) / events
pEstimate

# ***************************************************************************************************

# (b) The rbinom() function in R simulates random variables from a Binomial Distribution. 
# Recall a Binomial random variable is the number of successes in n trials where the probability of success is π.
# In rbinom() n is specified using the size argument and π by the prob argument. Confusingly, rbinom() has an argument called n, 
# but this how many times we would like to simulate from a Binomial(n, π) not the value of n.
# So,forexample,rbinom(n = 1, size = 10, prob = 0.5)willsimulateonerandomvariable that has a Binomial(10, 0.5) distribution, 
# whereas rbinom(n = 10, size = 1, prob = 0.5) will simulate 10 random variables each with a Binomial(1, 0.5) distribution.
# 
# In the U.S. having a baby girl isn’t as equally likely as having a baby boy, 
# in fact if we think of “having a baby girl” as a success in a single “having a baby” trial, 
# then the probability of success is about 0.49 (see CIA World FactBook).

# 1b

# i. Estimate the probability that for a family who has two children both will be girls 
# (Hint: Estimate P (X = 2) when X ∼ Binomial(2, 0.49)).
observations <- 10000
n <- rbinom(n = observations, size = 2, prob = 0.49)
bothFemaleEstimate <- sum(n == 2) / observations
bothFemaleEstimate
hist((n), plot = TRUE, main = "X ~ Binomial(2, 0.49)", xlab = "# of female children")

# This estimate (one run gave 0.2367) is close to the calculated probability by the product rule
# for independent observations P(A and B) = P(A) * P(B) = 0.49 * 0.49 = 0.2401

# ***************************************************************************************************

# ii. Estimate the probability that for a family who has four children all are girls.
observations <- 10000
n4 <- rbinom(n = observations, size = 4, prob = 0.49)
fourFemalseEstimate <- sum(n4 == 4) / observations
fourFemalseEstimate
hist((n4), plot = TRUE, main = "X ~ Binomial(4, 0.49)", xlab = "# of female children")

# This estimate (one run gave 0.0574) is close to the calculated probability by the product rule
# for independent observations P(A and B and C and D) = P(A) * P(B) * P(C) * P(D) 
# P(A and B and C and D) = 0.49 * 0.49 * 0.49 * 0.49 = 0.05764801

# ***************************************************************************************************

# iii. Estimate the probability that for a family with two children one of whom is a boy the other child is a girl 
# (Hint: This is tricky because it is a conditional probability. 
# Start by just using realizations where one child is a boy (i.e. X = 0, or 1), 
# what proportion have a boy and a girl (i.e. X = 1)?)

observations <- 100000
n2 <- rbinom(n = observations, size = 2, prob = 0.49)
oneBoyOneGirl <- sum(n2 == 1) / observations
oneBoyOneGirl
hist((n2), plot = TRUE, main = "X ~ Binomial(2, 0.49)", xlab = "# of female children")

# This estimate (one run gave 0.4955) is close to the calculated probability for conditional probability
# P(G) = probability of having a girl is given as 0.49 so P(B), the probability of having a boy is 1 - P(G) = 0.51
# Since the events are independent
# If the boy came first:
# P(G|B) = P(G and B) / P(B) = P(G) * P(B) / P(B) = P(G) = 0.49
# If the girl came first:
# P(B|G) = P(B and G) / P(G) = P(B) * P(G) / P(G) = P(B) = 0.51
# So the probability of having a boy and a girl given there is a boy is just shy of 0.50 
# which makes sense considering we know having a girl is slightly less likely than having a boy

# ***************************************************************************************************

# iv. Why can’t we estimate the probability that a randomly chosen family has at least one girl?
# What additional information would we need to estimate this?

# We can't do this because the size for the rbinom() is not known.  To do this, we would need
# to know the  number of children for the randomly selected family.

# ***************************************************************************************************