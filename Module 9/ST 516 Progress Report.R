# ST 516 - HW 9 Progress Report by Paul ReFalo  ========
library("ggplot2") 
library(plyr)
require(reshape2)

yrbss_2003 <- readRDS("yrbss_2003.rds")
yrbss_2013 <- readRDS("yrbss_2013.rds")

set.seed(8226) 
n_sim <- 1000 
pop_sd <- sd(yrbss_2013$bmi)
pop_mean <- mean(yrbss_2013$bmi)

get_means <- function(n, n_sim, pop_sd, pop_mean) { 
  replicate(n_sim, mean(rnorm(n, sd = pop_sd, mean = pop_mean)))
}

ns <- c(10, 100, 1000)
means <- lapply(ns, get_means, n_sim = n_sim, pop_sd = pop_sd, pop_mean = pop_mean) 

# ========  Graph Distribution of results  ============
bmi10 <- unlist(means[1])
bmi100 <- unlist(means[2])
bmi1000 <- unlist(means[3])

df <- data.frame(bmi = c(bmi10, bmi100, bmi1000), yy = rep(letters[1:3], each = 1000))

ggplot(dat,aes(x = bmi)) + 
  geom_histogram(data = subset(df, yy == 'a'), fill = "red", alpha = 0.75, binwidth = 0.1) +
  geom_histogram(data = subset(df, yy == 'b'), fill = "blue", alpha = 0.7, binwidth = 0.1) +
  geom_histogram(data = subset(df, yy == 'c'), fill = "green", alpha = 0.65, binwidth = 0.1) 


spread_sampdist <- sapply(means, sd)  # get SD of means vector containing means for n = 10, 100, 1000
spread_sampdist

true_se <- pop_sd/sqrt(ns)
rbind(round(spread_sampdist, 3), round(true_se, 3))

meansTally <- sapply(means, mean) # get mean of these data sets
meansTally

str(yrbss_2003)
str(yrbss_2013)

bmi2003 <- yrbss_2003$bmi
hist(bmi2003)

bmi2013 <- yrbss_2013$bmi
hist(bmi2013)
