# ST 516 - Homework 1 ========

# This file will help guide you through the first problem on Homework 1.
# Text after a hash (#) without a quote is interpreted as a code comment.
# Anything else is interpreted as R code.


# Problem 1 ========

# Part (a)  ========

# Example
# Addition: three plus four
3 + 4

# Your turn...
# Exponentiation: Three to the second power 
3 ** 2
# Used ** binary operator for exponential, not ^

# Part (b)  ========

# Example 
example1 <- 10
example1 
example2 <- "twenty"
example2
class(example1)
class(example2)

# Your turn...
one <- 31.28 # numeric
two <- FALSE # logical
three <- "string" # character
# Don't forget two and three!

class(one)
# Now you do the next two
class(two)
class(three)


# Part (c)  ========

# Example
example <- c(1, 3, 2, 4)
names(example) <- c("odd1", "odd2", "even1", "even2")
example

# Your turn...
numbers <- c(31282, 5, 1980, 27)
names(numbers) <- c("Date", "George", "Year", "Trout")
sum(numbers[c("George", "Trout")]) #subset by name
sum(numbers[c(2, 4)]) # subset by index
head(numbers) #view head of matrix numbers, just checking

# Part (d)  ========

# Example 
prod(example) # prod() gives the product of the elements of a vector
example[c(2,4)] # This gives the 2nd and 4th elements of the "example" vector
example < 3

# Your turn...
numbers > 100


# Now restart your R session. Highlight all of your code, and click Run. 
# Did it run without any errors?
# Yes, no error this time
