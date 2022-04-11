#download libraries
library(dplyr)

#upload clean data into R
data <- read.csv("main_data_cleaned_v4.csv", header = T)

#checking for classification
str(data)

#remove unnecessary column
data <- data[ -c(1:2) ]


#convert orderbrush, followers cat, verified, shop rating to be as factor
data$Orderbrush <- as.factor(data$Orderbrush)
data$Shop.Followers.Cat <- as.factor(data$Shop.Followers.Cat)
data$Verified <- as.factor(data$Verified)
data$Shop.Rating <- as.factor(data$Shop.Rating)

#check for linearity
model <- plot(data, col="navy", main="Matrix Scatterplot")
plot(data)

#Linear regression to predict y: shop rating vs order brush
model1 <- lm(Shop.Rating.Value~.,
             data = data
)
summary(model)

lm(data$Shop.Rating.value~.,data)
