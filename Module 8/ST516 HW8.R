# ST 516 - HW 8 by Paul ReFalo  ========
library("ggplot2") 
library("coin")

#1a
protein <- c(12.06, 11.16, 11.35, 11.89, 12.49, 12.19, 11.89, 12.47, 12.42,
             11.57, 12.2, 11.04, 12.17, 12.82, 11.81, 11.86, 11.75, 11.82,
             12.17, 11.63, 11.54, 12.76, 12.2, 12.13, 12.08, 12.56, 12.77,
             13.12, 12.15, 12.07, 11.48, 11.61, 12.28, 12.38, 11.67, 11.67,
             11.55, 12.16, 12.92, 11.85, 12.53, 12.29, 12.06, 12.06, 12.01,
             12.81, 11.78, 11.66, 11.4, 12.33, 12.21, 11.93, 12.71, 11.65,
             12.32, 12.52, 11.84, 12.56, 13.72, 11.29)
protein
length(protein)

#1b
proteinSD <- sd(protein)
proteinSD

#1c
boots <- function(bootStrapData){ # Arguments are data set
  b <- sample(bootStrapData, replace = TRUE)
  sd(b)
}

boots(protein)
#1d
bootsRep <- replicate(10000, boots(protein)) 
qplot(bootsRep, binwidth = .01)

#1e
boots95 <- quantile(bootsRep, probs = c(.025, 0.975))
boots95

#1f
# At the 95% Confidence Interval we fail to reject the null hypothesis because
# the sd value of 0.5 is within the CI.

#2a
strap <- bootstrap(protein, 10000, sd)

#2b
str(strap)
qplot(strap$thetastar, binwidth = .01)

#2c
strap95 <- quantile(strap$thetastar, probs = c(.025, 0.975))
strap95
# At the 95% Confidence Interval we fail to reject the null hypothesis because
# the sd value of 0.5 is within the CI.

#4
sinus <- c(7, 14, 21, 20, 19, 30, 22, 18)
mean(sinus)
median(sinus)
# just checking with R
w <- wilcox.test(sinus, mu = 21, alternative = "two.sided")  # p-value = 0.31
str(w)
w
S <- 6 + 1.5


#5
times <- c(3.6, 3.3, 1.2, 3.6, 2.3, 5.6, 3.4, 3.5, 3.1, 2.6)
mean(times)
median(times)
wilcox.test(times, mu = 3, alternative = "two.sided")  # p-value = 0.507



#####################################################################
# data from the lecture - just checking values from the lecture here
d <- c(1.2, 6.9, 9.1, 14.1, 2.5, 3.1, 3.8, -5.6, -4.6)
mean(d)
var(d)
f <- c(1.8, 3.9, 6.1, 11.1, 0.5, 0.1, 0.8, 8.6, 7.6)
mean(f)
var(f)
# End lecture data analysis