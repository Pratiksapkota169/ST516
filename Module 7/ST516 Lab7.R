# ST 516 - Lab 7 by Paul ReFalo  ========
library("ggplot2") 

pval <- function(n = 25){ # Default sample size of 25
  x <- rnorm(n, mean = 0, sd = 1) # Generate N(0,1) sample
  test <- t.test(x, mu = 0) # Run t-test on sample, with null hypoth. H_0: mu = 0 
  test$p.value # Extract and return p-value
}
pval() # Run function to confirm it is working

p_values <- replicate(100000, pval()) # 100,000 replications 
mean(p_values < 0.05) # What proportion are less than 0.05?

qplot(p_values, binwidth = 0.05, alpha = I(0.2), boundary = 0)  # boundary = 0, forces bars to start at zero

# 2-sample t-test, both samples are size = 10.
x <- rbeta(10, shape1 = 0.5, shape2 = 0.5) # Beta(0.5, 0.5) sample 
y <- runif(10, 0, 1) # Uniform(0,1) sample
t.test(x, y, mu = 0, paired = FALSE, var.equal = FALSE)

pval2 <- function(n1 = 25, n2 = 25){ # Arguments are sample sizes.
  x <- rbeta(n1, shape1 = 0.5, shape2 = 0.5) # Sample 1 from Beta(0.5, 0.5) 
  y <- runif(n2, min = 0, max = 1) # Sample 2 from Uniform(0,1)
  test <- t.test(x, y, mu = 0, paired = FALSE, var.equal = FALSE) 
  test$p.value
}

set.seed(1770) # Pierre-Simon Laplace calculated p-values in the 1770s! 
sim <- replicate(100000, pval2(5, 5)) # Samples of size 5
mean(sim < 0.05) # Rejection rate for level alpha = 0.05

mean(sim < 0.3) # Do we have 30% of p-vals less than 0.3?

qplot(sim, binwidth = 0.05, alpha = I(0.2), boundary = 0) + xlab("P-values")


sim <- replicate(100000, pval2(5, 50)) # Now 50 draws from U(0,1) 
qplot(sim, binwidth = 0.05, alpha = I(0.2), boundary = 0) + xlab("P-values")

sim <- replicate(100000, pval2(25, 25)) # Now size 25 draws from both 
qplot(sim, binwidth = 0.05, alpha = I(0.2), boundary = 0) + xlab("P-values")