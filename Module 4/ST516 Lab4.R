# ST 516 - Lab 4 by Paul ReFalo  ========
library("ggplot2") 

download.file("https://gist.github.com/cwickham/36654bf1483eccca8885e08eacb8df8c/raw", "nitrogen.csv")

nitrogen <- read.csv('nitrogen.csv', header = TRUE)

head(nitrogen) # Take a look at data

x <- nitrogen[ , 1] # Define x as *all rows, column 1* of "nitrogen"
qplot(x, binwidth = 0.1)

# pg 178, OpenIntro
# Calculate z-statistic
Z <- (mean(x) - 2.5)/(sd(x)/sqrt(36))
Z

# Calculate p-value (two-sided test)
P <- 2 * pnorm(abs(Z), mean = 0, sd = 1, lower.tail = FALSE) 
P

lower_bound <- mean(x) - qnorm(0.975) * sd(x)/sqrt(36) 
upper_bound <- mean(x) + qnorm(0.975) * sd(x)/sqrt(36) 
lower_bound
upper_bound