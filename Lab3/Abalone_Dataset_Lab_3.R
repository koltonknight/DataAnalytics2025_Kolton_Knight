###################
##### Abalone #####
###################

library(class)
library(caret)

# read dataset
abalone <- read.csv("C:/Users/kolto/Documents/Data Analytics/abalone_dataset.csv")

dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


## alternative way of setting age.group
dataset$age.group[dataset$rings<=8] <- "young"
dataset$age.group[dataset$rings>8 & dataset$rings<=11] <- "adult"
dataset$age.group[dataset$rings>11 & dataset$rings<=35] <- "old"

# sample create list of 105 (70% of 150) numbers randomly sampled from 1-150
n = 4176
s_abalone <- sample(n,n*.8)

## create train & test sets based on sampled indexes 
dataset.train <- dataset[s_abalone,]

dataset.test <- dataset[-s_abalone,]

# simple estimate of k
k = round(sqrt(n))


########################
# Train & Evaluate knn #
########################

## train model & predict in one step ('knn' function from 'class' library) using length, diameter, and height
knn.predicted <- knn(train = dataset.train[,2:4], test = dataset.test[,2:4], cl = dataset.train$age.group, k = 65)

# create contingency table/ confusion matrix 
contingency.table <- table(knn.predicted, dataset.test$age.group, dnn=list('predicted','actual'))

contingency.table

# calculate classification accuracy
sum(diag(contingency.table))/length(dataset.test$age.group)

## train 2nd model & predict in one step using whole weight, shucked weight, viscera weight, and shell weight
knn.predicted2 <- knn(train = dataset.train[,5:8], test = dataset.test[,5:8], cl = dataset.train$age.group, k = 65)

# create contingency table/ confusion matrix 
contingency.table2 <- table(knn.predicted2, dataset.test$age.group, dnn=list('predicted','actual'))

contingency.table2

# calculate classification accuracy
sum(diag(contingency.table2))/length(dataset.test$age.group)

##################
# Find optimal k #
##################


## train knn models for multiple values of k and plot accuracies

# list of k
k.list <- c(61,63,65,67,69,71,73)

# empty list for accuracy
accuracy.list <- c()

# loop: train&predict model for each k, compute accuracy and append it to list
for (k in k.list) {
  
  knn.predicted <- knn(train = dataset.train[,5:8], test = dataset.test[,5:8], cl = dataset.train$age.group, k = k)
  
  contingency.table <- table(knn.predicted, dataset.test$age.group, dnn=list('predicted','actual'))
  
  accuracy <- sum(diag(contingency.table))/length(dataset.test$age.group)
  
  accuracy.list <- c(accuracy.list,accuracy)
  
}


# plot acccuracy with k, limiting values of y axis between .9 & 1
plot(k.list,accuracy.list,type = "b")

## Alternatively:

## train and evaluate multiple knn models to find optimal k
knn.model <- train(dataset[,5:8], dataset$age.group, method = "knn", tuneLength = 10, trControl = trainControl(method = "cv"))

# print model outputs
print(knn.model)

#First Method returns 61 as optimal K with accuracy > .66
#Second Method returns 19 as optimal K with accuracy > .68

#KMeans

## run kmeans with best performing features
k = 3
abalone.km <- kmeans(dataset[,5:8], centers = k)


## get and plot clustering output 

assigned.clusters <- as.factor(abalone.km$cluster)

ggplot(dataset, aes(x = viscera_wieght, y = shell_weight, colour = as.factor(assigned.clusters))) +
  geom_point()


## WCSS: total within cluster sum of squares
abalone.km$tot.withinss

abalone.km$cluster


## run tests with multiple k values and plot WCSS
k.list <- c(2,3,4,5,6)

wcss.list <- c()

for (k in k.list) {
  
  abalone.km <- kmeans(dataset[,5:8], centers = k)
  
  wcss <- abalone.km$tot.withinss
  
  wcss.list <- c(wcss.list,wcss)
  
  ## get and plot clustering output 
  assigned.clusters <- as.factor(abalone.km$cluster)
  
  ggplot(dataset, aes(x = shell_weight, y = viscera_wieght, colour = assigned.clusters)) +
    geom_point()
  
}

plot(k.list,wcss.list,type = "b")

#optimal k is 3

#### END ####