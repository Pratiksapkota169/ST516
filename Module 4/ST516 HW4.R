# ST 516 - Homework 4 by Paul ReFalo  ========

library("openintro")

gifted       # print in its entirety
head(gifted) # the first six rows
#?gifted      # read the documentation for this dataset

mIQmean <- mean(gifted$motheriq)
mIQmean

mean(gifted$motheriq)

hist(gifted$motheriq)

stdev <- sd(gifted$motheriq)
SE <- stdev / sqrt(36)
SE

pointEstimate <- 1.96 * SE
pointEstimate


Z <- (mIQmean - 100)/(stdev/sqrt(36)) 
Z

#  1 - pnorm(sample average, mu, SE) would get you the upper tail area

#pval <- (1 - pnorm(108.1667, 100, 1.08))
pval <- 1 - pnorm(16.75) # z value, one-sided or upper tail
pval


#
