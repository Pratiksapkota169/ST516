# ST 516 - Homework 3 by Paul ReFalo  ========
library("ggplot2")

# Question 1 ***********************************
# Dice Game: Roll 1 = -$50; Roll ~1 = +$15

play <- function(silent = FALSE){
  die_roll <- sample(c(1, 2, 3, 4, 5, 6), # Roll outcomes
                     prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6), # assign equal probs
                     size = 1) # Roll once
  payout <- ifelse(die_roll == 1, -50, 15) 
  # Roll 1 --> Payout = -50
  # Roll 2,3,4,5,6 --> Payout = 15
  if(!silent) {
    message(paste0("You rolled a ", die_roll, ". Your payout is $", payout, "."))
  }                 
  payout
}

play(silent=TRUE) # play once as a test

xValues <- 1:1000
loopResults <- c()

# Take the mean of 30 games and replicate that many times, assign to results and plot
results <- replicate(1000, mean(replicate(30, play(silent=TRUE))))
qplot(results, binwidth = 1, xlim = c(0,10))

# Try a different plot of x, y values

for (n in xValues) {
  loopResults[n] <- mean(replicate(n, play(silent=TRUE)))
}
plot(xValues, loopResults, xlab="Games played", ylab="Mean Return for n games played", main="Games Played v Returns")
s = sd(loopResults)
abline(h=0, col = "blue")
abline(h = mean(results) + (3*s), col = "red")
abline(h = mean(results) - (3*s), col = "red")

print("Print the mean of results and of loopResults *******")
mean(results)
mean(loopResults)
var(results)

# The average here is ~ 4.17 for one roll.  The get this answer, I played 30 times, took the avg and did that 1000 times.
# So in the end, I played 30,000 games.  I also tried larger numbers and arrived at a similar value.
# I would certainly be willing to play this game since the mean result is a positive value.  I would want to play as many games
# as possible as the distribution narrows for more games played (see graph 2 'Games Played v Returns')

# Question 2 ***********************************


sample_size <- 100
p <- replicate(10000, mean(rpois(sample_size, 12)))

sd_p <- sd(p)
x1 <- seq(11, 13, length = 1000)
y1 <- dnorm(x1, 12, sqrt(12)/10) 


qplot(p, binwidth = .05, xlim=c(10,14)) +
  geom_line(aes(y = 10000*0.05*y1, x = x1), size = 1.5, color = "blue")


# End of file

