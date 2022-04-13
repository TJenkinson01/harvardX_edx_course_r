set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  # First line creates a list of 1000 0's and 1's  
  y <- rbinom(n, 1, p)
  # f_0 Creates a list of 1000 normally distributed values between 
  # -2.686312 and 3,302766 where the mean is 0 and sd is 1
  f_0 <- rnorm(n, mu_0, sigma_0)
  # f_1 creates a list of 1000 normally distributed values between
  # -1.435958 and 4.842957 where the mean is 2 and sd is 1
  f_1 <- rnorm(n, mu_1, sigma_1)
  # x creates a list of 1000 values ranging between -2.538682 and 4.842957 -
  # whenever y == 1, then the value is taken from f_1, else the value is f_0
  x <- ifelse(y == 1, f_1, f_0)
  # Creates a list of 526(???) index entries to create the test set
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  # Creates train and test sets using the test_index
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

# Runs the function above to create the data set. The result is 2 data sets, one
# with 1000 0s and 1s and the other with the values that went into the train and
# test sets. The train and test sets each have 500 values of x predicting y.
#
# The mean of x in the train set is 0.9616811
# The mean of x in the test set is 0.8432318
#
# They  look like those shown below. One notices that if the value of x is larger
# than about 1.86, the prediction is 1, else 0.

dat <- make_data()

# We have now defined a variable x that is predictive of a binary outcome y. It
# results in 2 curves: a 0 curve that peaks at around -0.8 and a 1 curve that
# peaks around 2 or so. The curves range from -3.0 to +4.25

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

# The assignment: Generate 25 different datasets changing the difference between
# the two classes using delta <- seq(0, 3, len=25) and plot accuracy vs mu_1.

mu_1 <- seq(0, 3, len=25)

# Establish the 25 different values (differences)
delta <- seq(0, 3, len=25)

delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)
