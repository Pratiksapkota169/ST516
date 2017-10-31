# ST 516 - Homework 5 by Paul ReFalo  ========
library("ggplot2") 

# Question 1
x <- seq(from = 0, to = 10, by = 0.01) # x-axis values
y <- dexp(x, rate = 1) # distribution function curve
qplot(x, y, geom = "area") # Plot Exponential(1)

set.seed(1908) # William Gosset invents t-test
x2 <- rexp(1000, rate = 1)
x2 # show those numbers
t.test(x2, mu = 2, conf.level = 0.95) # two-sided by default

# Write a summary that includes an interpretation of the p-value and 95% confidence interval.

# Calculate a z-statistic (continue to use the sample SD, not population SD), and a p-value based on the normal distribution.
Z <- (mean(x2) - 2)/(sd(x2)/sqrt(length(x2)))
Z

sd_x2 <- sd(x2)
sd_x2

# Calculate p-value (two-sided test)
P <- 2 * pnorm(abs(Z), mean = 0, sd = sd_x2, lower.tail = FALSE) 
P

# You should find the test statistic is the same for both tests:
#  • Why is the p-value different?  Sample size too low so CLT does not apply -> use exact instead
#  • Which is more appropriate in real life, where the population standard deviation is usually
# unknown? t.test() because sd is not an argument for this calculation

# Question 2
x_q2 <- rbinom(1, 20, p = 0.25)
x_q2

A <- prop.test(x_q2, n = 20, p = 0.25, conf.level = 0.95, correct = FALSE)
A
A$conf.int[1] < 0.25 & 0.25 < A$conf.int[2]
#str(A)
E <- binom.test(x_q2, n = 20, p = 0.25, conf.level = 0.95) 
E
E$conf.int[1] < 0.25 & 0.25 < E$conf.int[2]

approx <- function(n) {
  x_q2 <- rbinom(1, n, p = 0.25)
  A <- prop.test(x_q2, n, p = 0.25, conf.level = 0.95, correct = FALSE)
  #print(A)
  if (A$conf.int[1] <= 0.25 & 0.25 <= A$conf.int[2]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

exact <- function(n) {
  x_q2 <- rbinom(1, n, p = 0.25)
  E <- binom.test(x_q2, n, p = 0.25, conf.level = 0.95) 
  #print(E)
  if (E$conf.int[1] <= 0.25 & 0.25 <= E$conf.int[2]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

approxResult <- approx(20)
exactResult <- exact(20)

approxResult
exactResult

approxReplicates <- replicate(10000, approx(20))
exactReplicates <- replicate(10000, exact(20))

mean(approxReplicates)
mean(exactReplicates)

approxReplicates <- replicate(10000, approx(100))
exactReplicates <- replicate(10000, exact(100))

mean(approxReplicates)
mean(exactReplicates)

# Question 4

bp <- c(115, 134, 131, 143, 130, 154, 119, 137, 155, 130, 110, 138)
var <- ( sum( (bp - mean(bp) )^2 ) ) / length(bp - 1)
bpMean <- mean(bp)
t <- (bpMean - 129) / sqrt(var/length(bp))
t

# Question 5

rain <- c(5.335, 5.345, 5.380, 5.520, 5.360, 6.285, 5.510, 5.340,
          5.395, 5.305, 5.190, 5.455, 5.350, 5.125, 5.340, 5.305,
          5.315, 5.330, 5.115, 5.265)
rain <- sort(rain)

# t test just for fun
var <- ( sum( (rain - mean(rain) )^2 ) ) / length(rain - 1)
rainMean <- mean(rain)
tRain <- (rainMean - 5.4) / sqrt(var/length(rain))
tRain

obsBelow <- sum(rain < 5.4)
obsRange <- c(1:20)
Z <- ((obsBelow/length(rain)) - 0.5) / sqrt(0.5*0.5/length(rain))
Z
Z <- ((obsRange/length(rain)) - 0.5) / sqrt(0.5*0.5/length(rain))
Z

# Zα/2 = 1.96 for 95% CI

ZinRange95 <- abs(Z) < 1.96
ZinRange95

CIforRain95 <- rain[6:14]

CImin95 <- min(CIforRain95)
CImax95 <- max(CIforRain95)
CImin95
CImax95

# Zα/2 = 2.57 for 99% CI

ZinRange99 <- abs(Z) < 2.57
ZinRange99

CIforRain99 <- rain[5:15]

CImin99 <- min(CIforRain99)
CImax99 <- max(CIforRain99)
CImin99
CImax99
