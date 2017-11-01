# ST 516 - Lab 6 by Paul ReFalo  ========
library("ggplot2") 

set.seed(1964) # Nike was founded in 1964 
shoeA <- rnorm(25, mean = 50, sd = 20) 
shoeB <- rnorm(30, mean = 60, sd = 15)

t.test(shoeA, shoeB, mu = 0, alternative = "two.sided", paired = FALSE, var.equal = FALSE)

set.seed(1809) # Carl Friedrich Gauss helps establish the Normal distribution in 1809 
means <- rnorm(32, 50, 25) # Create 32 distinct means; one for each couple
wife <- rnorm(32, mean = means, sd = 15)
husband <- rnorm(32, mean = means, sd = 15) # Give each married couple same mean

t.test(wife, husband, mu = 0, alternative = "two.sided", paired = TRUE)