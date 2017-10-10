# ST 516 - Lab 2 by Paul ReFalo  ========
library(ggplot2)

roll <- function(bones = 1:6) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}

numberOfRolls <- 10000
rolls <- replicate(numberOfRolls, roll()) # Roll the dice 10 times! 
rolls # Show the results, stored in the "rolls" vector

qplot(rolls, binwidth = 1) # Histogram of our roll outcomes

rolls == 7 # Gives a logical vector of "TRUE" (lucky 7) and "FALSE" (not lucky 7)
sum(rolls == 7) # R knows to count "TRUE" as 1, "FALSE" as 0
result <- sum(rolls == 7) / numberOfRolls # Divide by 10 to get a proportion
result

oneSixth <- 1/6
oneSixth

delta <- abs(result - oneSixth)
delta
# The difference, or delta, between the arithmatic value of 1/6 and our result is quite small but not exact.  
# This demonstrates that this process is random since it differs from the arithmatic value of 1/6.
# This is the Law of large numbers.  As our sample size gets larger, the result converges on 1/6

5 > 2
5 < 7

x <- c(1, 2, 3, 4, 5, 6, 7, 8)
x > 4
x[x > 4] # return elements of x that are greater than four

x[x > 4 & x <= 6] # Two conditions, linked by "&"

x[x > 6 | x < 3] # Two conditions, linked by "|"

