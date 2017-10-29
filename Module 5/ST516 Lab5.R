# ST 516 - Lab 5 by Paul ReFalo  ========
library("ggplot2") 

set.seed(1908) # William Gosset invents t-test
x <- rnorm(40, 5, 5) # Generate 40 N(5,5) random numbers 
x # show those numbers

t.test(x, mu = 3, conf.level = 0.95)

prop.test(x = 20, n = 100, p = 0.25, conf.level = 0.95, correct = FALSE)

b <- binom.test(3, 20, p = 0.25, conf.level = 0.95) 
b
