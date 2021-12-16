# Anomaly detection 
str(iris)
plot(iris$Sepal.Length)
# outliers 
boxplot(iris$Sepal.Length~iris$Species, xlab = "Species", ylab = "Sepal Length")


# write a function to calculate outliers 
data <- iris
outliers <- function(data, low, high){
  outs <- subset(data, data$Sepal.Length<low | data$Sepal.Length>high)
  return(outs)
}
outliers(data, 4.5, 7.5)

# use DMWR package to identify outliers
# remotes::install_github("cran/DMwR")
library(DMwR)
# first remove categorical variables 
nospecies <- data[, 1:4]
scores <- lofactor(nospecies, k=3)
scores
plot(density(scores))

