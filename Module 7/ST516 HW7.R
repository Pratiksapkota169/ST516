# ST 516 - Homework 7 by Paul ReFalo  ========
library("ggplot2") 
library("reshape2")
library(dplyr)

# Question 1
x <- seq(from = 0, to = 10, by = 0.01) # x-axis values
y <- dgamma(x, shape = 2, rate = 2, log = FALSE) # distribution function curve

qplot(x, y, geom = "line") # Plot Exponential(1)

gammaInterval <- function(n = 25) {
  g <- rgamma(n, shape = 2, rate = 2)
  Gresults <- t.test(g, conf.level = 0.95)

  if (Gresults$conf.int[1] <= 1 & 1 <= Gresults$conf.int[2]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#gammaTest <- gammaInterval(100)
#gammaTest

simGamma5 <- mean(replicate(100000, gammaInterval(5))) # Samples of size 5
simGamma5

simGamma25 <- mean(replicate(100000, gammaInterval(25))) # Samples of size 25
simGamma25

simGamma50 <- mean(replicate(100000, gammaInterval(50))) # Samples of size 25
simGamma50

simGamma100 <- mean(replicate(100000, gammaInterval(100))) # Samples of size 100
simGamma100


# Plotting experientation for Conf Interval data above

gammaInterval2 <- function(n = 25) {
  g <- rgamma(n, shape = 2, rate = 2)
  Gresults <- t.test(g, conf.level = 0.95)
  #gMean <- mean(g) #unname(Gresults$estimate[1])
  gList <- list(Gresults$conf.int[1], Gresults$conf.int[2])
  return(gList)
}

rep5 <- replicate(1000, gammaInterval2(5))
rep5 <- unlist(rep5, recursive = TRUE, use.names = TRUE)
rep5
rep25 <- replicate(1000, gammaInterval2(25))
rep25 <- unlist(rep25, recursive = TRUE, use.names = TRUE)
rep25

rep50 <- replicate(1000, gammaInterval2(50))
rep50 <- unlist(rep50, recursive = TRUE, use.names = TRUE)
rep50

rep100 <- replicate(1000, gammaInterval2(100))
rep100 <- unlist(rep100, recursive = TRUE, use.names = TRUE)
rep100

df <- data.frame(replicates = 1:2000,
                 n5 = rep5,
                 n25 = rep25,
                 n50 = rep50,
                 n100 = rep100)

df <- melt(df ,  id.vars = 'replicates', variable.name = 'series')
ggplot(df, aes(replicates,value)) + geom_point(aes(colour = series)) + ylab("Confidence Interval") + xlab("Replicates with Upper & Lower value")




