#libraries
library(caret)
library(class)
library(ggfortify)

#read in data
wine <- read.csv("C:/Users/kolto/Documents/Data Analytics/Lab 4/wine.data", header = FALSE)

colnames(wine) <- c("Class", "Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")

x <- wine[,2:14]

#Compute PCs and plot
principal_components <- princomp(x, cor = TRUE, score = TRUE)

summary(principal_components)

principal_components$loadings

wine$Class <- as.factor(wine$Class)

autoplot(principal_components, data = wine, colour = 'Class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

#Variables that contribute the least to 1st PC (Ash, Color Intensity)

x <- x[, -c(3, 10)]

#Compute PCs and plot round 2
principal_components <- princomp(x, cor = TRUE, score = TRUE)

summary(principal_components)

principal_components$loadings

autoplot(principal_components, data = wine, colour = 'Class',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

#KNN using original data
n = 178
s_wine <- sample(n,n*.8)

dataset.train <- wine[s_wine,]

dataset.test <- wine[-s_wine,]

# simple estimate of k
k = round(sqrt(n))

#train model
knn.predicted <- knn(train = dataset.train, test = dataset.test, cl = dataset.train$Class, k = 13)

#evaluation
contingency.table <- table(knn.predicted, dataset.test$Class, dnn=list('predicted','actual'))

contingency.table

precision_1 <- contingency.table[1,1]/sum(contingency.table[1,])

precision_2 <- contingency.table[2,2]/sum(contingency.table[2,])

precision_3 <- contingency.table[3,3]/sum(contingency.table[3,])

recall_1 <- contingency.table[1,1]/sum(contingency.table[,1])

recall_2 <- contingency.table[2,2]/sum(contingency.table[,2])

recall_3 <- contingency.table[3,3]/sum(contingency.table[,3])

F1_1 <- 2*((recall_1 * precision_1)/(recall_1+precision_1))

F1_2 <- 2*((recall_2 * precision_2)/(recall_2+precision_2))

F1_3 <- 2*((recall_3 * precision_3)/(recall_3+precision_3))

#KNN using first 3 PCs
n = 178
s_wine <- sample(n,n*.8)

dataset.train <- wine[s_wine,]

dataset.test <- wine[-s_wine,]

principal_components <- princomp(dataset.train[,2:14], cor = TRUE, score = TRUE)

train_pca <- data.frame(principal_components$scores[,1:3])
test_pca <- predict(principal_components, newdata=dataset.test[,2:14])[,1:3]

#add cl back
train_pca$Class <- dataset.train$Class
test_pca <- data.frame(test_pca)
test_pca$Test <- dataset.test$Class

# simple estimate of k
k = round(sqrt(n))

#train model
knn.predicted <- knn(train = train_pca, test = test_pca, cl = train_pca$Class, k = 13)

#evaluation
contingency.table <- table(knn.predicted, dataset.test$Class, dnn=list('predicted','actual'))

contingency.table

precision_1 <- contingency.table[1,1]/sum(contingency.table[1,])

precision_2 <- contingency.table[2,2]/sum(contingency.table[2,])

precision_3 <- contingency.table[3,3]/sum(contingency.table[3,])

recall_1 <- contingency.table[1,1]/sum(contingency.table[,1])

recall_2 <- contingency.table[2,2]/sum(contingency.table[,2])

recall_3 <- contingency.table[3,3]/sum(contingency.table[,3])

F1_1 <- 2*((recall_1 * precision_1)/(recall_1+precision_1))

F1_2 <- 2*((recall_2 * precision_2)/(recall_2+precision_2))

F1_3 <- 2*((recall_3 * precision_3)/(recall_3+precision_3))
