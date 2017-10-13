# ST 516 - Lab 3 by Paul ReFalo  ========
library("ggplot2")

rgamma(10, 5, 1) # Generate 10 Gamma(5,1) random variables

y1 <- rgamma(1000, 5, 1) # Generate 1000 Gamma(5, 1) random variables
# Create a set of x, y points to draw the pdf of Gamma(5, 1)
x1 <- seq(0, 15, length = 1000) 
y2 <- dgamma(x1, 5, 1)
qplot(y1, binwidth = 0.5) +
  # add the pdf to histogram
  geom_line(aes(y = 1000*0.5*y2, x = x1), size = 2, color = "blue")

draws <- rgamma(10, 5, 1) # Sample of size 10 from Gamma(5,1), stored as "draws" 
draws # Show the sample

avg <- mean(draws) # store the mean of the sample of 10 
avg # show the mean

mean(rgamma(10,5,1)) # Same process, all in one line of code!

x <- replicate(10000, mean(rgamma(10, 5, 1))) # Sample size = 10; 10,000 replications 
qplot(x, binwidth=.05, xlim=c(3, 7)) # Create a histogram of results

x <- replicate(10000, mean(rgamma(50, 5, 1))) # n = 50; 10,000 replications 
qplot(x, binwidth = 0.05, xlim=c(3,7)) # Create histogram

x <- replicate(10000, mean(rgamma(100, 5, 1))) # n = 100; 10,000 replications 
qplot(x, binwidth = 0.05, xlim=c(3,7)) # Create histogram

x1 <- seq(4, 6, length = 1000); y1 <- dnorm(x1, 5, sqrt(5)/10) 
qplot(x, binwidth = .05, xlim=c(3,7)) +
  geom_line(aes(y = 10000*0.05*y1, x = x1), size = 1.5, color = "blue")



