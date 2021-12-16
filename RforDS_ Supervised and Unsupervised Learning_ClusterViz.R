# Supervised and Unsupervised learning 
# Packages 
library(rattle)
library(rpart.plot)
library(caret)
library(kknn)
library(kernlab)
library(e1071)
library(MCMCpack)
library(randomForest)
library(FactoMineR)

# Supervised 

# Decision Tree 
summary(weather)

# create a subset with (RISK_MM) removed 
weather2 <- subset(weather, select = -c(RISK_MM))

model <- rpart(formula = RainTomorrow ~ ., data = weather2, method = "class")
summary(model)

# Plotting the decision tree 
# regular plot
plot(model)
text(model, use.n = TRUE, all = TRUE, cex = 0.7, list = FALSE)

# Fancy plot
fancyRpartPlot(model, main = "Rain Tomorrow", sub = "Chapter 12")

# Regression 
# We can use a regression to predict our target value by producing a regression model from our predictor variables
forestfires <- read.csv("forestfires.csv")
str(forestfires)
model_reg <- lm(formula = area ~ month + temp + wind + rain, data = forestfires)
summary(model_reg)

par(mfrow = c(2,2))
plot(model_reg)

# Neural Network 
# In a neural network, it is assumed that there is a complex relationship between the predictor variables and the target variable
bupa <- read.csv("bupa.data")
colnames(bupa) <- c("mcv","alkphos","alamine","aspartate","glutamyl","drinks","selector")
summary(bupa)
library(neuralnet)
model_nn <- neuralnet(selector ~ mcv + alkphos + alamine + aspartate + glutamyl + drinks, data = bupa, linear.output = FALSE, hidden = 3)

model_nn$result.matrix
plot(model_nn)

# Unsupervised learning 
# Cluster analysis

wheat <- read.table("seeds_dataset.txt")
str(wheat)
colnames(wheat) <- c("area", "perimeter", "compactness", "length","width", "asymmetry", "groove", "undefined")

# fit a kmeans model 
fit <- kmeans(wheat, 2)
fit

fviz_cluster(fit, data = wheat)

# Density estimation 
# Density estimation is used to provide an estimate of the probability density function of a random variable
sunspot <- read.csv("Sunspot.month.csv")
colnames(sunspot) <- c("X", "time", "sunspot.month")

d <- density(sunspot$sunspot.month)
plot(d)

# We can use the density to estimate additional periods
N <- 1000
sunspot.new <- rnorm(N, sample(sunspot$sunspot.month, size = N, replace = TRUE))
lines(density(sunspot.new), col="red")








