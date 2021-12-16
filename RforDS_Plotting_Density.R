data <- iris
colnames(data)
summary(data)
# Regression line
plot(data$Petal.Length ~ data$Sepal.Length)
reg1 <- lm(data$Petal.Length ~ data$Sepal.Length)
abline(reg1, col="red")

abline(lm(data$Petal.Length ~ data$Sepal.Length), col="red")
# Lowess line 
# A lowess line is a smoothed line calculated using locally weighted polynomial regression
plot(data$Petal.Length ~ data$Sepal.Length)
lines(lowess(data$Sepal.Length, data$Petal.Length), col="blue")

# steps diagram
plot(data$Sepal.Length,data$Petal.Length, type = "s")
# histogram 
plot(data$Sepal.Length, data$Petal.Length, type = "h")

# Scatterplot
library(car)
scatterplot(data$Sepal.Length, data$Petal.Length, col = "orange", pch = 1)
scatterplotMatrix(data, regLine = list(col="green"), pch = 1,
                  smooth = list(col.smooth="black", col.spread="red"))

# Density scatterplot 
library(hexbin)
bin<-hexbin(data$Sepal.Length, data$Petal.Length, xbins = 10)
summary(bin)
plot(bin, xlab = "Sepal Length", ylab = "Petal Length")






