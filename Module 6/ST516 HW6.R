# ST 516 - Homework 6 by Paul ReFalo  ========
library("ggplot2") 
library(dplyr) # you might need to install this one

set.seed(1810)
A <- rnorm(10)
B <- rnorm(10)
C <- 0.5 + (0.8 * A) + (sqrt(1 - 0.8^2) * B)
B <- 0.5 + B

A
B
C
mean(A)
mean(B)
mean(C)

Abar <- mean(A)
Asd <- sd(A)
Avar <- var(A)
Bbar <- mean(B)
Bsd <- sd(B)
Bvar <- var(B)

t.test(A, B, mu = 0, paired = FALSE, conf.level = 0.95) # two-sided by default
# manually calculate the t statistic and pvalue
t <- (Abar - Bbar) / sqrt((Avar/10) + (Bvar/10))
t
df <- 17.11 # from t-test
Pvalue <- 2 * pt(t, df) # multiply by two for two-sided test
Pvalue

t.test(A, B, mu = 0, paired = TRUE, conf.level = 0.95) # two-sided by default

diffs <- A - B
t.test(diffs)  # should be the same as above and it is

SEdiff <- sd(diffs) / sqrt(10)

tPaired <- (mean(diffs) - 0) / (SEdiff)
tPaired

dfDiff <- 9 # from t-test
PvalueDiff <- 2 * pt(tPaired, dfDiff) # multiply by two for two-sided test
PvalueDiff

# Part C

t.test(A, C, mu = 0, paired = FALSE, conf.level = 0.95) # two-sided by default
t.test(A, C, mu = 0, paired = TRUE, conf.level = 0.95) # two-sided by default


# Put our samples in a data frame
q1_data <- data.frame(
  obs = rep(1:10, 3),
  value = c(A, B, C),
  group = rep(c("A", "B", "C"), each = 10)
)
# Histograms for each sample
qplot(value, data = q1_data) + facet_wrap(~ group, ncol = 1)
# Relationship between pairs of observations, A & B
qplot(group, value, data = filter(q1_data, group != "C"),
      group = obs, geom = c("point", "line"))
# Relationship between pairs of observations, A & C
qplot(group, value, data = filter(q1_data, group != "B"),
      group = obs, geom = c("point", "line"))


# Question 2
SE <- c(15, 19, 45, 35, 67, 13, 33)
SF <- c(16, 18, 60, 54, 70, 11, 34)
tDiffs <- SF - SE
tDiffs

t.test(SE, SF, mu = 0, paired = TRUE, conf.level = 0.95) # two-sided by default

# Question 3

RxnA <- c(456, 222, 567, 344, 222, 334, 543, 447)
RxnB <- c(343, 242, 990, 222, 344, 455, 600, 323)

RxnDiffs <- RxnA - RxnB
RxnDiffs

t.test(RxnA, RxnB, mu = 0, paired = FALSE, conf.level = 0.95) # two-sided by default


#