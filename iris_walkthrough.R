install.packages("caret", dependencies = c("Depends", "Suggests"))

library(caret)
data("iris")

dataset <- iris

# create a training set using 80% of the data
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# save 20% of our data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% for training and testing
dataset<-dataset[validation_index,]

# Do some eda on the data

# view the dimensions
dim(dataset)

# look at the classes of each variable
sapply(dataset, class)

# look at the first 5 rows of data
head(dataset)

# since the class variable is a factor, we can look at its levels
levels(dataset$Species)

# Summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage = percentage)

# we can use summary function to quickly see the numerical distribution of our data
summary(dataset)

# Next, look at some basic plots of the data

# split the input and output 
x <- dataset[, 1:4]
y <- dataset[, 5]

# make a boxplot for each attribute on one image
par(mfrow = c(1,4))
  for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# also do a basic barplot of the class breakdown(not very useful here)

# next we can examine scatterplots for every pair of attributes and color the points by class
# allowing us a quick and broad breakdown of the data
featurePlot(x=x, y=y, plot='ellipse')


# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# and finally density plots
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# using a 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# building five commonplace machine learning models
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

  
#Selecting the best of those models


# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
1
2
3
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# comparing the accuracy of models with a dotplot
dotplot(results)

# printing the results of the best one
print(fit.lda)

# making predictions with the lda model on the Validation data


# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)