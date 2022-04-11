install.packages("foreign")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("e1071")
install.packages("corrplot")
install.packages("randomForest")
install.packages("naivebayes")
install.packages("caret")
install.packages("vcd")
install.packages("gridExtra")
install.packages("ggeffects")
install.packages("smotefamily")
setwd("~/R Studio Projects/TBA2104_Predictive_Analytics_Project")
library(foreign)
library(ggplot2)
library(dplyr)
library(e1071)
library(corrplot)
library(randomForest)
library(naivebayes)
library(caret)
library(vcd)
library(gridExtra)
library(ggeffects)
library(smotefamily)

data <- read.csv("main_data_cleaned_v4.csv", header = TRUE)

# data preprocess
data$Orderbrush <- as.factor(data$Orderbrush)
data$Verified <- as.factor(data$Verified)
data$Shop.Followers.Cat <- as.factor(data$Shop.Followers.Cat)
data$Shop.Rating <- as.factor(data$Shop.Rating)

str(data)

numData <- data[,3:15]

set.seed(122)
train_index <- createDataPartition(numData$Orderbrush, p=0.75, list= FALSE)
train <- numData[train_index, ] # 75% of the data
test <- numData[-train_index, ] # 25% of the data

train_wo_cat <- subset(train, select = -c(Shop.Followers.Cat, Verified, Shop.Rating))
test_wo_cat <- subset(test, select = -c(Shop.Followers.Cat, Verified, Shop.Rating))

# Drop non-numeric data 

train_numeric <- subset(train, select = -c(Shop.Followers.Cat, Verified, Shop.Rating))
test_numeric <- subset(test, select = -c(Shop.Followers.Cat, Verified, Shop.Rating))

train_numeric$Orderbrush <- as.numeric(train_numeric$Orderbrush) - 1

# 0 is non fraud, 1 is fraud
table(train_numeric$Orderbrush)
# perform smote on train data 

train_smote <- SMOTE(X = train_numeric, target=train_numeric$Orderbrush, dup_size = 10)
train_smote_data <- train_smote$data
train_smote_data$Orderbrush <- as.factor(train_smote_data$Orderbrush)
levels(train_smote_data$Orderbrush) <- c("No", "Yes")
train_smote_data <- subset(train_smote_data, select = -c(class))
table(train_smote_data$Orderbrush)

train_smote_data <- preProcess(train_smote_data, method = c("center", "scale"))

# RF SMOTE



rf_model <- randomForest(formula = Orderbrush ~ ., data=train_smote_data )
p_rf <- predict(rf_model, newdata=test_numeric)
confusionMatrix(test_numeric$Orderbrush, p_rf)

varImpPlot(rf_model)
varImp(rf_model)


# RF - CARET
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10,
                     verboseIter = FALSE,
                     sampling = "down")

random_f_model <- caret::train(Orderbrush ~ .,
                                   data = train_wo_cat,
                                   method = "rf",
                                   preProcess = c("range"),
                                   trControl = ctrl)

p_rf2 <- predict(random_f_model, newdata=test_wo_cat)
confusionMatrix(test_wo_cat$Orderbrush, p_rf2, mode = "prec_recall", positive="Yes")


