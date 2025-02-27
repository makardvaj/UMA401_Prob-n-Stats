# Q1 (a). Suppose there is a chest of coins with 20 gold, 30 silver and 50 bronze coins.
# You randomly draw 10 coins from this chest. Write an R code which will give us the
# one outcome of the sample space for this experiment. (use of sample(): an in-built
#function in R).

coins <- c(rep("gold", 20), rep("silver", 30), rep("bronze", 50))
## rep(a, x) makes (replicates) 'x' copies of 'a'
random_draw <- sample(coins, 10, replace = FALSE)
print(random_draw)

# Q1 (b). In a surgical procedure, the chances of success and failure are 90% and 10%
# respectively. Generate an outcome for the next 10 surgical procedures performed.
# (use of prob(): an option to be passed in the sample function)

outcomes <- c("Success", "Failure")
probabilities <- c(0.9, 0.1)
surgical_outcomes <- sample(outcomes, 10, replace = TRUE, prob = probabilities)
## replacement is TRUE because the overall probability is unconditional,
## and hence, remains the same
print(surgical_outcomes)

# Q2. A room has n people, and each has an equal chance of being born on any of the 365
# days of the year. (For simplicity, we’ll ignore leap years). What is the probability
# that two people in the room have the same birthday?
# (a) Use an R simulation to estimate this for various n.
# (b) Find the smallest value of n for which the probability of a match is greater than
# 0.5.

# Q2 (a).
birthday_simulation <- function(n, trials = 10000) {
    match_count <- 0
    
    for(i in 1:trials) {
        birthdays <- sample(1:365, n, replace=TRUE)
        if (length(unique(birthdays)) < n) {
            match_count <- match_count + 1
        }
    }
    
    return(match_count / trials)
}

n_values <- c(5, 10, 20, 23, 30, 40, 50)
probabilities <- sapply(n_values, birthday_simulation)
results <- data.frame(n = n_values, Probability = probabilities)
print(results)

# Q2 (b).
find_min_n <- function(trials = 10000) {
    n <- 1
    while (TRUE) {
        prob <- birthday_simulation(n, trials)
        if (prob > 0.5) {
            return(n)
        }
        n <- n + 1
    }
}

min_n <- find_min_n()
cat("The smallest value of 'n', for which, the probability of a match is greater than 0.5, is : ", min_n, "\n")

## Q3.Write an R function for computing conditional probability. Call this function to do
# the following problem:
# suppose the probability of the weather being cloudy is 40%. Also suppose the prob-
# ability of rain on a given day is 20% and that the probability of clouds on a rainy day
# is 85%. If it’s cloudy outside on a given day, what is the probability that it will rain
# that day?

conditional_probability <- function(P_A, P_B, P_B_given_A) {
    P_A_given_B <- (P_B_given_A * P_A) / P_B
    return(P_A_given_B)
}

P_A <- 0.20
P_B <- 0.40
P_C_given_A <- 0.85

P_A_given_B <- conditional_probability(P_A, P_B, P_B_given_A)
cat("Chance that it rains today : ", P_A_given_B, "\n")

# Q4. The iris dataset is a built-in dataset in R that contains measurements on 4 different
# attributes (in centimeters) for 150 flowers from 3 different species. Load this dataset
# and do the following:
data(iris)
# (a) Print first few rows of this dataset.
head(iris)
# (b) Find the structure of this dataset.
str(iris)
# (c) Find the range of the data regarding the sepal length of flowers.
range(iris$Sepal.Length)
# (d) Find the mean of the sepal length.
mean(iris$Sepal.Length)
# (e) Find the median of the sepal length.
median(iris$Sepal.Length)
# (f) Find the first and the third quartiles and hence the interquartile range.
quantiles <- quantile(iris$Sepal.Length, probs = c(0.25, 0.75))
IQR_value <- IQR(iris$Sepal.Length)
quantiles
IQR_value
# (g) Find the standard deviation and variance.
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
# (h) Try doing the above exercises for sepal.width, petal.length and petal.width.
# Sepal.Width
range(iris$Sepal.Width)
mean(iris$Sepal.Width)
median(iris$Sepal.Width)
quantile(iris$Sepal.Width, probs = c(0.25, 0.75))
IQR(iris$Sepal.Width)
sd(iris$Sepal.Width)
var(iris$Sepal.Width)

# Petal.Length
range(iris$Petal.Length)
mean(iris$Petal.Length)
median(iris$Petal.Length)
quantile(iris$Petal.Length, probs = c(0.25, 0.75))
IQR(iris$Petal.Length)
sd(iris$Petal.Length)
var(iris$Petal.Length)

# Petal.Width
range(iris$Petal.Width)
mean(iris$Petal.Width)
median(iris$Petal.Width)
quantile(iris$Petal.Width, probs = c(0.25, 0.75))
IQR(iris$Petal.Width)
sd(iris$Petal.Width)
var(iris$Petal.Width)
# (i) Use the built-in function summary on the dataset Iris.
summary(iris)

## Q5. R does not have a standard in-built function to calculate mode. So we create a user
# function to calculate mode of a data set in R. This function takes the vector as input
# and gives the mode value as output.
calculate_mode <- function(v) {
    unique_values <- unique(v)
    freq <- tabulate(match(v, unique_values))
    mode_value <- unique_values[which.max(freq)]
    return(mode_value)
}

data_vector <- c(1, 2, 2, 3, 4, 4, 4, 5, 6)
mode_value <- calculate_mode(data_vector)
cat("The mode of the data set is : ", mode_value, "\n")
