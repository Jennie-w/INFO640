install.packages("factoextra")
install.packages("rpart") 
install.packages("rpart.plot")
install.package("rattle")
install.packages("RColorBrewer")

library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)
library(RcolorBrewer)
library(class)
library(factoextra)

data("txhousing")
?txhousing

summary(txhousing)
head(txhousing)

lm_housing <- lm(median ~ listings, data=txhousing)

unseen <- data.frame(listings = 800) 
predict(lm_housing, unseen)

summary(lm_housing)


street <- read.csv("Downloads/info640-master/Datasets/streeteasy.csv") 
summary(street)

street_select6 <- street %>% select(rent, bedrooms,bathrooms, size_sqft, min_to_subway,floor, building_age_yrs)
lm_street_select6 <- lm(rent ~ ., data=street_select6)
summary(lm_street_select6)

#Decision Trees(Classification)
train <- read.csv("Downloads/info640-master/Datasets/titanic/train.csv")
test <- read.csv("Downloads/info640-master/Datasets/titanic/test.csv")
str(test)
str(train)

train <- train %>% select(Pclass, Sex, Age, Survived)
test <- test %>% select(Pclass, Sex, Age, Survived)

tree <- rpart(Survived ~Pclass + Age + Sex, train, method="class")
fancyRpartPlot(tree)

pred <- predict(tree, test, type="class")
pred

conf <- table(test$Survived, pred)
conf

TP <- conf[2,2]
FP <- conf[1,2]
TN <- conf[1,1]
FN <- conf[2,1]

acc <- sum(TP, TN)/sum(TP, TN, FP, FN)
acc

prec <- TP/sum(TP,FP)
prec

rec <- TP/sum(TP, FN)
rec

acc <- sum(diag(conf)/sum(conf))
acc

#Classfication KNN

train <- drop_na(train)
test <- drop_na(test)

train_labels <- train$Survived
test_labels <- train$Survived

knn_train <- train 
knn_test <- test

knn_train$Sex <- as.factor(knn_train$Sex) 
knn_test$Sex <- as.factor(knn_test$Sex)

knn_train$Sex <- as.factor(gsub("male", "1", knn_train$Sex)) 
knn_train$Sex <- as.factor(gsub("female", "0", knn_train$Sex))

knn_test$Sex <- as.factor(gsub("male", "1", knn_test$Sex)) 
knn_test$Sex <- as.factor(gsub("female", "0", knn_test$Sex))

knn_train$Sex <- as.numeric(knn_train$Sex) 
knn_test$Sex <- as.numeric(knn_test$Sex)

knn_train$Survived <- NULL 
knn_test$Survived <- NULL

min_class <- min(knn_train$Pclass)
max_class <- max(knn_train$Pclass)
knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class) 
knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)

min_age <- min(knn_train$Age)
max_age <- max(knn_train$Age)
knn_train$Age <- (knn_train$Age - min_age) / (max_age - min_age) 
knn_test$Age <- (knn_test$Age - min_age)/(max_age - min_age)

k_pred <- knn(train=knn_train, test=knn_test, cl=train_labels, k=5) 
k_pred
length(k_pred)
length(test_labels)

conf <- table(test_labels, k_pred)
conf

my_titanic <- data.frame("Age" = .6, "Pclass" = .5, "Sex" = 0)
new_k_pred <- knn(train=knn_train, test=my_titanic, cl=train_labels, k=5) 
new_k_pred

#Clustering

set.seed(1234) 
data(iris)

my_iris <- iris[,1:4] 
head(my_iris)
species <- iris$Species

kmeans_iris <- kmeans(my_iris, centers=3, nstart=10) 
kmeans_iris

table(species, kmeans_iris$cluster)

plot(Petal.Length ~ Petal.Width, data = my_iris, col = kmeans_iris$cluster)

fviz_cluster(kmeans_iris, data = my_iris)






