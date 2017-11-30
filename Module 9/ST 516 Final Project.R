# ST 516 - Final Project by Paul ReFalo  ========
library("ggplot2") 
library(plyr)
require(reshape2)

# Import yrbss for 2003 and 2013 from local rds files
yrbss_2003 <- readRDS("yrbss_2003.rds")
yrbss_2013 <- readRDS("yrbss_2013.rds")

set.seed(8226) # set seed
# ====  Global Variables  =====
n_sim <- 1000   # set number of replicates
pop_sd2003 <- sd(yrbss_2003$bmi)  # population sd for yrbss_2013bmi
pop_mean2003 <- mean(yrbss_2003$bmi) # population mean for yrbss_2013bmi
pop_sd2013 <- sd(yrbss_2013$bmi)  # population sd for yrbss_2013bmi
pop_mean2013 <- mean(yrbss_2013$bmi) # population mean for yrbss_2013bmi
ns <- c(10, 100, 1000)  # replicate sample sizes vector
nsMedian <- c(5, 10, 100)  # replicates for median differences in bmi from 2013 to 2003

# ===== Functions to get n_sim replicates for n samples =====
get_means <- function(n, n_sim, pop_sd2013, pop_mean2013) { 
  replicate(n_sim, unlist(mean(rnorm(n, sd = pop_sd2013, mean = pop_mean2013))))
}

get_quantile <- function(n, n_sim, pop_sd2013, pop_mean2013) { 
  replicate(n_sim, unname(quantile(rnorm(n, sd = pop_sd2013, mean = pop_mean2013), 0.25)))
}

get_min <- function(n, n_sim, pop_sd2013, pop_mean2013) { 
  replicate(n_sim, min(rnorm(n, sd = pop_sd2013, mean = pop_mean2013)))
}

get_medianDelta <- function(n, n_sim, pop_sd2013, pop_mean2003, pop_mean2013) { 
  replicate(n_sim, median(rnorm(n, sd = pop_sd2013, mean = pop_mean2013)) - 
              median(rnorm(n, sd = pop_mean2003, mean = pop_mean2003)))
}


# ======== Call Functions ========
# Use lapply to call get_means, get_quantile, and get_min functions and store results
means <- lapply(ns, get_means, n_sim = n_sim, pop_sd2013 = pop_sd2013, pop_mean2013 = pop_mean2013) 
quantile25 <- lapply(ns, get_quantile, n_sim = n_sim, pop_sd2013 = pop_sd2013, pop_mean2013 = pop_mean2013) 
mins <- lapply(ns, get_min, n_sim = n_sim, pop_sd2013 = pop_sd2013, pop_mean2013 = pop_mean2013) 
mediansDelta <- lapply(nsMedian, get_medianDelta, n_sim = n_sim, pop_sd2013 = pop_sd2013,
                       pop_mean2003 = pop_mean2003, pop_mean2013 = pop_mean2013) 

# ========  Plot BMI Means Distribution of results  ============

# make dfMeans for plotting
dfMeans <- data.frame(bmi = c(means[[1]], means[[2]], means[[3]]), 
                      yy = rep(letters[1:3], each = 1000)) # c(bmi10, bmi100, bmi1000)

ggplot(dat,aes(x = bmi)) + 
  geom_histogram(data = subset(dfMeans, yy == 'a'), fill = "red", alpha = 0.85, binwidth = 0.1) +
  geom_histogram(data = subset(dfMeans, yy == 'b'), fill = "cadetblue4", alpha = 0.85, binwidth = 0.1) +
  geom_histogram(data = subset(dfMeans, yy == 'c'), fill = "green", alpha = 0.75, binwidth = 0.1) +
  xlab('BMI Means') + ylab('Frequency in 1000 replicates') + ggtitle('1000 Replicates of BMI Means') 

dfMeans2 <- data.frame(bmi = c(means[[1]], means[[2]], means[[3]]), 
                       yy = rep(c('n = 10', 'n = 100', 'n = 1000'), each = 1000)) # c(bmi10, bmi100, bmi1000)

ggplot(data = dfMeans2, aes(x = bmi)) + geom_histogram(binwidth = 0.1, fill = 'blue') + 
  facet_wrap(~yy, nrow = 3)  + 
  xlab('BMI Means') + ylab('Frequency in 1000 replicates') + ggtitle('1000 Replicates of BMI Means') 


# ========  Plot 25% Quantile Distribution of results  ============

# make dfQ25 for plotting
dfQ25 <- data.frame(Q25 = c(quantile25[[1]], quantile25[[2]], quantile25[[3]]), 
                    yy = rep(letters[1:3], each = 1000), each = 1000)

ggplot(dat,aes(x = Q25)) + 
  geom_histogram(data = subset(dfQ25, yy == 'a'), fill = "red", alpha = 0.85, binwidth = 0.1) +
  geom_histogram(data = subset(dfQ25, yy == 'b'), fill = "cadetblue4", alpha = 0.85, binwidth = 0.1) +
  geom_histogram(data = subset(dfQ25, yy == 'c'), fill = "green", alpha = 0.75, binwidth = 0.1) +
  xlab('BMI 25% Quantiles') + ylab('Frequency in 1000 replicates') + ggtitle('1000 Replicates of BMI 25% Quantiles')

ggplot(data = dfQ25, aes(x = Q25)) + geom_histogram(binwidth = 0.1, fill = 'blue') + 
  facet_wrap(~yy, nrow = 3)  + 
  xlab('BMI 25% Quantile') + ylab('Frequency in 1000 replicates') + ggtitle('1000 Replicates of BMI 25% Quantiles') 


# ========  Plot Minimum Distribution of results  ============

# make dfMin for plotting
dfMin <- data.frame(bmiMin = c(mins[[1]], mins[[2]], mins[[3]]), 
                    yy = rep(letters[1:3], each = 1000)) 

ggplot(dat,aes(x = bmiMin)) + 
  geom_histogram(data = subset(dfMin, yy == 'a'), fill = "red", alpha = 0.75, binwidth = 0.3) +
  geom_histogram(data = subset(dfMin, yy == 'b'), fill = "cadetblue4", alpha = 0.7, binwidth = 0.3) +
  geom_histogram(data = subset(dfMin, yy == 'c'), fill = "green", alpha = 0.65, binwidth = 0.3) +
  xlab('BMI Minimums') + ylab('Frequency in 1000 replicates') + ggtitle('1000 Replicates of BMI Minimums')

dfMin2 <- data.frame(bmiMin = c(mins[[1]], mins[[2]], mins[[3]]), 
                     yy = rep(c('n = 10', 'n = 100', 'n = 1000'), each = 1000)) 

ggplot(data = dfMin2, aes(x = bmiMin)) + geom_histogram(binwidth = 0.1, fill = 'blue') + 
  facet_wrap(~yy, nrow = 3)  + 
  xlab('BMI Minimums') + ylab('Frequency in 1000 replicates') + ggtitle('1000 Replicates of BMI Minimums') 

# ========  Plot Median Difference from 2013 to 2003 Distribution of results  ============

# make dfMedianDiff for plotting
dfMedianDiff <- data.frame(bmiMedianDiff = c(mediansDelta[[1]], mediansDelta[[2]], mediansDelta[[3]]), 
                           yy = rep(letters[1:3], each = 1000)) 

ggplot(dat,aes(x = bmiMedianDiff)) + 
  geom_histogram(data = subset(dfMedianDiff, yy == 'a'), fill = "red", alpha = 0.75, binwidth = 0.7) +
  geom_histogram(data = subset(dfMedianDiff, yy == 'b'), fill = "cadetblue4", alpha = 0.7, binwidth = 0.7) +
  geom_histogram(data = subset(dfMedianDiff, yy == 'c'), fill = "green", alpha = 0.65, binwidth = 0.7) +
  xlab('BMI Median Differences between 2013 and 2003') + ylab('Frequency in 1000 replicates') + 
  ggtitle('1000 Replicates of BMI Median Differences')

dfMedianDiff2 <- data.frame(bmiMedianDiff = c(mediansDelta[[1]], mediansDelta[[2]], mediansDelta[[3]]), 
                            yy = rep(c('n = 5', 'n = 10', 'n = 100'), each = 1000)) 

ggplot(data = dfMedianDiff2, aes(x = bmiMedianDiff)) + geom_histogram(binwidth = 0.2, fill = 'blue') + 
  facet_wrap(~yy, nrow = 3)  + 
  xlab('BMI Medians') + ylab('Frequency in 1000 replicates') + ggtitle('1000 Replicates of BMI Medians') 

# For 1d also report mean and standard deviation for Median Differences 2013 to 2003

# This measure of center (median) doesn't change much, not sensitive to n but distribution is
meanMedianDiff5 <- mean(mediansDelta[[1]])
meanMedianDiff5
meanMedianDiff10 <- mean(mediansDelta[[2]])
meanMedianDiff10
meanMedianDiff100 <- mean(mediansDelta[[3]])
meanMedianDiff100

sdMedianDiff5 <- sd(mediansDelta[[1]])
sdMedianDiff5
sdMedianDiff10 <- sd(mediansDelta[[2]])
sdMedianDiff10
sdMedianDiff100 <- sd(mediansDelta[[3]])
sdMedianDiff100

mediansDelta[[1]]

######## Means analysis
spread_sampdist <- sapply(means, sd)  # get SD of means vector containing means for n = 10, 100, 1000
spread_sampdist

true_se <- pop_sd/sqrt(ns)
rbind(round(spread_sampdist, 3), round(true_se, 3))

meansTally <- sapply(means, mean) # get mean of these data sets
meansTally

######## Quantile 25
spread_sampdistQ25 <- sapply(quantile25, sd)  # get SD of means vector containing means for n = 10, 100, 1000
spread_sampdistQ25

true_se <- pop_sd2013/sqrt(ns)
true_se
rbind(round(spread_sampdistQ25, 3), round(true_se, 3))

q25Tally <- sapply(quantile25, mean) # get mean of these data sets
q25Tally

######## Minimums
spread_sampdistMin <- sapply(mins, sd)  # get SD of means vector containing means for n = 10, 100, 1000
spread_sampdistMin

true_se <- pop_sd/sqrt(ns)
rbind(round(spread_sampdistMin, 3), round(true_se, 3))

minTally <- sapply(mins, mean) # get mean of these data sets
minTally

########

str(yrbss_2003)
str(yrbss_2013)

bmi2003 <- yrbss_2003$bmi
hist(bmi2003)

bmi2013 <- yrbss_2013$bmi
hist(bmi2013)

# Question 2A
# How has the BMI of high-school students changed between 2003 and 2013? 
# Are high-schoolers getting more overweight?

meanBMI2003 <- mean(yrbss_2003$bmi)
meanBMI2003
medianBMI2003 <- median(yrbss_2003$bmi)
medianBMI2003
sdBMI2003 <- sd(yrbss_2003$bmi)
sdBMI2003
var(yrbss_2003$bmi)

meanBMI2013 <- mean(yrbss_2013$bmi)
meanBMI2013
medianBMI2013 <- median(yrbss_2013$bmi)
medianBMI2013
sdBMI2013 <- sd(yrbss_2013$bmi)
sdBMI2013
var(yrbss_2013$bmi)

dfMeansByYear <- data.frame(yrbss_2003$bmi, yrbss_2013$bmi)
head(dfMeansByYear)
ggplot(melt(df), aes(value, fill = variable)) + geom_histogram(position = "dodge")


hist(yrbss_2003$bmi)  # no evidence of strong skew (slight skew to the right)
hist(yrbss_2013$bmi)  # no evidence of strong skew (slight skew to the right)
boxplot(yrbss_2003$bmi, yrbss_2013$bmi)

t.test(yrbss_2003$bmi, yrbss_2013$bmi, mu = 0, paired = FALSE, conf.level = 0.95) # two-sided by default
# t.test results df = 25990, p-value = 0.000175 with CI -0.344057 -0.107974

boxplot(yrbss_2003$bmi, yrbss_2013$bmi, names = c('2003', '2013'), ylab = 'BMI', main = 'BMI Data from 2003 and 2013')

ggplot() + 
  geom_histogram(aes(x=yrbss_2003$bmi), binwidth = 0.5, alpha = 0.4, fill = 2) +
  geom_histogram(aes(x=yrbss_2013$bmi), binwidth = 0.5, alpha = 0.3, fill = 'blue') +
  xlab('BMI 2003 (red) and 2013 (blue)') + ylab('Frequency in Sample Population')

# Question 2B
# Are male high-schoolers more likely to smoke than female high-schoolers in 2013? 

yrbss_2003$q33
yrbss_2003$sex
males2013 <- data.frame(select(filter(yrbss_2013, sex == 'Male')), yrbss_2013$q33)
females2013 <- select(filter(yrbss_2013, sex == 'Female'))
males2013
females2013
str(males2013)
print(males2013)
head(males2013)

xy <- subset.data.frame(yrbss_2013, sex == 'Male') # 6414 observations (males)
xyTotal <- nrow(xy)
xyTotal
xyNA <- sum(is.na(xy$q33))
xyRespondents <- xyTotal - xyNA
xySmokers <- subset.data.frame(xy, q33 != '0 days')
xyPortionRespondents <- xyRespondents/xyTotal
xyPortionRespondents

xyNA
xyRespondents
nrow(xySmokers) # 999 Male smokers out of 6128 Male respondents

xx <- subset.data.frame(yrbss_2013, sex == 'Female') # 6166 x observations (females)
xxTotal <- nrow(xx)
xxTotal
xxNA <- sum(is.na(xx$q33))
xxRespondents <- xxTotal - xxNA
xxSmokers <- subset.data.frame(xx, q33 != '0 days')
xxPortionRespondents <- xxRespondents/xxTotal
xxPortionRespondents

xxNA
xxRespondents
nrow(xxSmokers) # 753 Female smokers out of 5983 Female respondents

ysSmokers <- c(999, 753)
nsSmokers <- c(6128, 5983)
prop.test(ysSmokers, nsSmokers)


gender=c(rep("Male" , 2) , rep("Female" , 2) )
key=rep(c("Response Rate" , "Smoking Rate") , 2)
percent=c(95.5, 16.3, 97.0, 12.6) 
data=data.frame(gender,key,percent)

# Grouped
ggplot(data, aes(fill=key, y=percent, x=gender)) + 
  geom_bar(position="dodge", stat="identity") + xlab('Rates by Gender') + ylab('Percent')

# Question 2C
# How much TV do highschoolers watch?
yrbss_2003$q81

tv2003rows <- nrow(yrbss_2003)
tv2003rows  # 14057

tv2003NA <- sum(is.na(yrbss_2003$q81))  # get NA's
tv2003NA  # 424 out of 14057

# Then recode the old field into the new one for the specified rows
yrbss_2003$tv <- NA
yrbss_2003$tv[yrbss_2003$q81=='No TV on average school day'] <- 0
yrbss_2003$tv[yrbss_2003$q81=='Less than 1 hour per day'] <- 0.5
yrbss_2003$tv[yrbss_2003$q81=='1 hour per day'] <- 1
yrbss_2003$tv[yrbss_2003$q81=='2 hours per day'] <- 2
yrbss_2003$tv[yrbss_2003$q81=='3 hours per day'] <- 3
yrbss_2003$tv[yrbss_2003$q81=='4 hours per day'] <- 4
yrbss_2003$tv[yrbss_2003$q81=='5 or more hours per day'] <- 5.5

yrbss_2003$tv

tv2003 <- yrbss_2003$tv
tv2003 <- tv2003[!is.na(tv2003)]  # remove NA's
tv2003
median(tv2003)  # 2
mean(tv2003)  # 2.47249 but makes little sense because > 5 cannot be quantified

# =======  Repeat analysis for ybrss_2013  =========
tv2013rows <- nrow(yrbss_2013)
tv2013rows  # 12580

tv2013NA <- sum(is.na(yrbss_2013$q81))  # get NA's
tv2013NA  # 271 out of 12580

# Then recode the old field into the new one for the specified rows
yrbss_2013$tv <- NA   # create a new column 'tv'
yrbss_2013$tv[yrbss_2013$q81=='No TV on average school day'] <- 0
yrbss_2013$tv[yrbss_2013$q81=='Less than 1 hour per day'] <- 0.5
yrbss_2013$tv[yrbss_2013$q81=='1 hour per day'] <- 1
yrbss_2013$tv[yrbss_2013$q81=='2 hours per day'] <- 2
yrbss_2013$tv[yrbss_2013$q81=='3 hours per day'] <- 3
yrbss_2013$tv[yrbss_2013$q81=='4 hours per day'] <- 4
yrbss_2013$tv[yrbss_2013$q81=='5 or more hours per day'] <- 5.5

yrbss_2013$tv

tv2013 <- yrbss_2013$tv
tv2013 <- tv2013[!is.na(tv2013)]  # remove NA's
tv2013

median(tv2013)  # 2
mean(tv2013)  # 2.07421 but makes little sense because > 5 cannot be quantified

get_median95 <- function(n, dataVector) {
  tvRange <- round(c(n/2 - 1.96*sqrt(n)/2, n/2 + 1.96*sqrt(n)/2 + 1))
  print(tvRange)
  dataVector <- sort(dataVector) 
  return(dataVector[tvRange])
}




# median, mean and CI for 2003
tv2003CI <- get_median95(tv2003rows, tv2003) # [6912 7146] 
tv2003CI # [2 2]

t.test(tv2003, mu = mean(tv2003), paired = FALSE, conf.level = 0.95) # two-sided by default
# t.test results df = 13630, p-value = 1 with CI 2.44329 2.50170, mean = 2.47249

# median, mean and CI for 2013
tv2013CI <- get_median95(tv2013rows, tv2013) # [6180 6401] 
tv2013CI # [2 2]

t.test(tv2013, mu = mean(tv2013), paired = FALSE, conf.level = 0.95) # two-sided by default
# t.test results df = 12310, p-value = 1 with CI 2.04389 2.10454, mean = 2.07421


# Plot TV hours results
year=c(rep("2003" , 2) , rep("2013" , 2) )
key=rep(c("Mean" , "Median") , 2)
hours=c(2.47, 2, 2.07, 2) 
data=data.frame(year,key,hours)

# Grouped
ggplot(data, aes(fill=key, y=hours, x=year)) + 
  geom_bar(position="dodge", stat="identity") + xlab('Year') + ylab('Hours') + ggtitle('Hours of TV Watched by Year')

ggplot() + 
  geom_histogram(aes(x=tv2003), binwidth = 0.5, fill = 'blue') +
  xlab('Hours of TV 2003') + ylab('Frequency')

ggplot() + 
  geom_histogram(aes(x=tv2013), binwidth = 0.5, fill = 'blue') +
  xlab('Hours of TV 2013') + ylab('Frequency')
