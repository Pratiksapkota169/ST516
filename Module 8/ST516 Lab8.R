# ST 516 - Lab 8 by Paul ReFalo  ========
library("ggplot2") 
library("coin")

cold <- c(1,1,1,3) # number of o-ring incidents in launches below 18 degrees C 
warm <- c(rep(0, 17), 1, 1, 2) # ditto above 18 degrees C

T <- (mean(cold) - mean(warm))/sqrt(var(cold)/4 + var(warm)/20) # t-statistic
T

t.test(cold, warm)$statistic

sample(1:24, size = 4, replace = FALSE)

o_ring_data <- c(cold, warm) 
o_ring_data

o_ring_data[c(12, 5, 8, 23)]

o_ring_data[-c(12, 5, 8, 23)]

cold_indices <- sample(1:24, size = 4, replace = FALSE) 
cold_perm <- o_ring_data[cold_indices]
warm_perm <- o_ring_data[-cold_indices] 
t.test(cold_perm, warm_perm)$statistic

perm_o_ring <- function(){
  cold_indices <- sample(1:24, size = 4, replace = FALSE) 
  cold_perm <- o_ring_data[cold_indices]
  warm_perm <- o_ring_data[-cold_indices]
  t.test(cold_perm, warm_perm)$statistic }

perm_o_ring()

set.seed(1986) # Challenger exploded in 1986
perms <- replicate(100000, perm_o_ring()) # randomly permute, calculate t-stat 100,000 times 
mean(perms > T) # Proportion of t-stats more extreme than observed

qplot(perms, binwidth = .5) + # Histogram of randomly assigned group t-stats
  geom_vline(xintercept = T, color = "blue", size = 1) # vertical line = observed t-stat


o_ring_data

groups <- factor(c(rep("cold",4), rep("warm", 20))) # Create group label vector str(groups)
oneway_test(o_ring_data ~ groups, distribution = "exact")


perm_o_ring <- function(x, n1){
  n <- length(x)
  cold_indices <- sample(1:n, size = n1, replace = FALSE) 
  cold_perm <- x[cold_indices]
  warm_perm <- x[-cold_indices]
  t.test(cold_perm, warm_perm)$statistic
}


perm_o_ring(o_ring_data, 4)

perm_tstat <- function(x, n1){
  n <- length(x)
  grp1_indices <- sample(1:n, size = n1, replace = FALSE) 
  grp1_perm <- x[grp1_indices]
  grp2_perm <- x[-grp1_indices]
  t.test(grp1_perm, grp2_perm)$statistic
}
perm_tstat(o_ring_data, 4)



