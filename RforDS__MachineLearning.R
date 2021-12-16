# get the data
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
colnames(housing) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PRATIO","B","LSTAT","MDEV")
# summarize the data and see the variables 
str(housing)
dim(housing)
summary(housing)
plot(housing)

# variable correlations 
# install.packages("corrplot")
library(corrplot)
corrplot(cor(housing), method = "number")

# partitioning the dataset 
housing <- housing[order(housing$MDEV),]
# install.packages("caret")
library(caret)
set.seed(3277)
trainingIndices <- createDataPartition(housing$MDEV, p=0.75, list = FALSE)
housingTraining <- housing[trainingIndices,] 
housingTesting <- housing[-trainingIndices,]
nrow(housingTesting)
nrow(housingTraining)

# linear regression 
formula <- MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PRATIO + B + LSTAT
formula2 <- MDEV ~ CRIM + TAX + PRATIO + B 

Linearmodel <- lm(formula, data = housingTraining)
Linearmodel2 <- lm(formula2, data = housingTraining)

# Compare models 
anova(Linearmodel, Linearmodel2)

predicted <- predict(Linearmodel, newdata = housingTesting)
summary(predicted)
summary(housingTesting$MDEV)
linearplot <- plot(predicted, housingTesting$MDEV)

sumofsquares <- function(x){
  return(sum(x^2))
}

diff <- predicted - housingTesting$MDEV
sumofsquares(diff)

# logistic regression 
lr <- glm(formula, data = housingTraining)
summary(lr)
predicted_lr <- predict(lr, newdata = housingTesting)

summary(predicted_lr)
plot(predicted_lr, housingTesting$MDEV)
diff_lr <- predicted_lr - housingTesting$MDEV
sumofsquares(diff_lr)

# residuals 
# In this plots, each point is one house, 
# where the predicted value made by the model is on the x-axis and the accuracy of the prediction is on the y-axis.
# The distance from the line at 0 is how bad the prediction was for that value
plot(resid(Linearmodel))
plot(resid(lr))

# least square regression

x <- housingTesting$MDEV
Y <- predicted 

b1 <- sum((x-mean(x))*(Y-mean(Y)))/sum((x-mean(x))^2)
b0 <- mean(Y)-b1*mean(x)

c(b0, b1)

plot(x, Y)
abline(c(b0, b1), col='blue', lwd=2)

# relative importance
# calculated using RELAIMPO package 
# install.packages("relaimpo")
# The relative importance of the variables used in the model
# will tell us which variables are providing the most effect on your results
library(relaimpo)
# lmg: the coefficient of the variable from the model
# last: looks at what the effect of adding this variable into the model
# First: looks at the variable as if none of the other variables were present in the model
# Pratt: product of the standard coefficient and the correlation
calc.relimp(Linearmodel, type = c("lmg", "last", "first", "pratt"), rela=TRUE)
# Bootstrap 
boot <- boot.relimp(Linearmodel, b = 10, type = c("lmg","last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 



# Stepwise regression 
# adding/removing variables from the regression model
# adjusting for the effect of doing so
# continuing to evaluate each of the variables involved
library(MASS)
step <- stepAIC(Linearmodel, direction="both")


# K-nearest neighbor CLASSIFICATION
# using CLASS package 
# using TRAINING data 

library(class)
knnModel <- knn(train = housingTraining, test = housingTesting, cl = housingTraining$MDEV)
summary(knnModel)
plot(knnModel)
plot(housingTesting$MDEV)

# Naïve Bayes 
# determining classifiers based on probability, assuming the features are independent (the assumption is the naïve part!)
# the idea is that median value is the product of all the associated data
library(e1071)

nb <- naiveBayes(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PRATIO + B + LSTAT, data = housingTraining)

# We can examine the parameters generated for the effect of each variable as follows:
nb$tables$TAX

# The apriori value of the Naïve Bayes result contains the class distribution for the dependent variable
# very similar to the previous knn model result: again, we have the tight overlap in the middle with both tails skewed
plot(nb$apriori)

# SVM 
# Supervised learning -> Classification 

# install.packages("kernlab")
# pima diabetes dataset downloaded from Kaggle
pima <- read.csv("C:/Users/azimz/OneDrive - University of Maryland School of Medicine/OLSON LAB/Data Science Resources/diabetes.csv") 
summary(pima)
colnames(pima) <- c("pregnancies","glucose","bp","triceps","insulin"
                    ,"bmi","pedigree","age","class")

# Split the dataset to train and test 
library(caret)
set.seed(3277)
pimaIndices <- createDataPartition(pima$class, p=0.75, list = FALSE)
pimaTraining <- pima[pimaIndices,]
pimaTesting <- pima[-pimaIndices,]

# calculate the SVM model from the training data
library(kernlab)
# bootstrap 
bootControl <- trainControl(number = 200)
# SVM model fitting 
svmFit <- train(pimaTraining[, -9], pimaTraining[, 9], method = "svmRadial", tuneLength = 5, trControl = bootControl, scaled = FALSE)
svmFit
svmFit$finalModel

predicted_svm <- predict(svmFit$finalModel, newdata = pimaTesting[, -9])
table(pred = predicted_svm, true = pimaTesting[, 9])
plot(pimaTesting$class, predicted_svm)

# K-means clustering 

irisIndices <- createDataPartition(iris$Species, p=0.75, list = FALSE)
irisTraining <- iris[irisIndices, ]
irisTesting <- iris[- irisIndices,]

bootControl_km <- trainControl(number = 20)
km_model <- kmeans(irisTraining[, 1:4], 3)
km_model

# visualize clusters 
library(ggpubr)
library(factoextra)
fviz_cluster(km_model, data = irisTraining[, 1:4])
# evaluate the model
table(irisTraining$Species, km_model$cluster)


# using our remaining test data, we can predict which cluster the test data will be applied to
# We can use the clue package for testing out the k-means model
# install.packages("clue")
library(clue)
cl_predict(km_model, irisTesting[, -5])
irisTesting[, -5]

# Decision tree
# There are several decision tree packages available in R
# For this example, we use the housing regression data

# A package to produce decision trees from regression data is rpart
# install.packages("rpart")
library(rpart)
library(caret)
set.seed(3277)

trainingIndices_DT <- createDataPartition(housing$MDEV, p=0.75, list = FALSE)
housingTraining_DT <- housing[trainingIndices_DT, ]
housingTesting_DT <- housing[-trainingIndices_DT, ]

housingFit_DT <- rpart(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + B + AGE + DIS + RAD + TAX + PRATIO + B + LSTAT, method="anova", data=housingTraining)

# plot the tree
# regular plot
plot(housingFit_DT)
text(housingFit_DT, use.n = TRUE, all = TRUE, cex = 0.7, col = 'blue')
# fancy plot
fancyRpartPlot(housingFit_DT, main = "Housing Price", sub = "Fancy plot")

# generate the predicted values using the test data
predict_DDT <- predict(housingFit_DT, newdata = housingTesting_DT)

# verify the correctness of the model -> using the sum of squares
diff_DT <- predict_DDT - housingTesting_DT$MDEV
# using our previously defined sumofsquares function 
sumofsquares(diff_DT)

# Neural network 
# install.packages("neuralnet")
library(neuralnet)

# model 
nnet <- neuralnet(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PRATIO + B + LSTAT,housingTraining) 
plot(nnet, rep = "best")

# evaluate the neural net performance 
results <- compute(nnet, housingTesting_DT[, -14])
diff_nnet <- results$net.result - housingTesting_DT$MDEV
sumofsquares(diff_nnet)


# RandomForest 
# install.packages("randomForest")
library(randomForest)

forestFit <- randomForest(MDEV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PRATIO + B + LSTAT, data=housingTraining)
forestPredict <- predict(forestFit, newdata = housingTesting)

# evaluate the random forest performance 
diff_RF <- forestPredict - housingTesting$MDEV
sumofsquares(diff_RF)


# install.packages("Metrics")
library(Metrics)










