library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


set.seed(2, sample.kind="Rounding") 

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train <- iris[-test_index,]
test <- iris[test_index,]

summary <- iris %>% group_by(Species) %>%
  summarise(mean(Sepal.Length), sd(Sepal.Length), mean(Sepal.Width), sd(Sepal.Width), mean(Petal.Length), sd(Petal.Length), mean(Petal.Width), sd(Petal.Width))

view(summary)


f <- function(x){
  rv <- seq(min(x), max(x), by=0.1) #rv = ranged values
  sapply(rv,
         function(i){
           y_hat <- ifelse(x > i,'virginica','versicolor')
           mean(y_hat == test$Species)} #here we can find the accuracy 
  )}

predictions <- apply(test[,-5],MARGIN = 2, FUN = f)


sapply(predictions,max) 

predictions <- f(train[,3]) #f is previously created function
rv <- seq(min(train[,3]),max(train[,3]),by=0.1) #rv = ranged values
cutoffs <-rv[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)



plot(iris,pch=21,bg=iris$Species)


petalLR <- seq(min(train$Petal.Length),max(train$Petal.Length),by=0.1) #PetalLR = Petal Length Range
petalWR <- seq(min(train$Petal.Width),max(train$Petal.Width),by=0.1) #PetalWR = Petal Width Range

length_predictions <- sapply(petalLR,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})

length_cutoff <- petalLR[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWR,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})

width_cutoff <- petalWR[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)




