#Bayes theory

#     P(A | B) = P(B | A) x (P(A)/P(B))


# Assume a patient comes into the doctor’s office to test whether they have a particular disease.

# The test is positive 85% of the time when tested on a patient with the disease (high sensitivity) 
# The test is negative 90% of the time when tested on a healthy patient (high specificity)
# The disease is prevalent in about 2% of the community

# Using Bayes' theorem, calculate the probability that you have the disease if the test is positive.

# P(pos | disease) = 0.85

# P(neg | healthy) = 0.90

# P(disease) = 0.02




# We have a hypothetical population of 1 million individuals with the following conditional probabilities as described below:

set.seed(1, sample.kind = "Rounding") # using R 3.6 or later

disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))

test <- rep(NA, 1e6)

test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))


# prob of positive test:

mean(test)

# prob of having disease given negative test:

mean(disease[test==0])

# prob of having disease if test is positive:

pospos <- mean(disease[test==1])

# Compare the prevalence of disease in people who test positive to the overall prevalence of disease.

# If a patient's test is positive, by how many times does that increase their risk of having the disease?
# (First calculate the probability of having the disease given a positive test, then divide by the probability of having the disease.)

pospos/0.02
