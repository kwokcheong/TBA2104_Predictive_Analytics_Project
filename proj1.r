library(dplyr)
library(randomForest)
library(e1071)
library(corrplot)
library(naivebayes)
library(readxl)
library(xlsx)

data <- read.csv('Main_data.csv', header = TRUE, na.strings=c('NIL'))
clean_data <- na.omit(data)

# examine data
# Check if any missing values
sum(is.na(clean_data))
# check data
head(clean_data)

# Refactor data for usage
replace(clean_data$Shop.Response.Rate, clean_data$Shop.Response.Rate > 100, 100)
clean_data$Shop.Response.Rate[clean_data$Shop.Response.Rate > 100] <- 100
max(clean_data$Shop.Response.Rate)

# Export data for sharing
# write.xlsx(clean_data,file="main_data_cleaned.xlsx", sheetName="Order_brush",col.names = TRUE)

data <- read_excel('main_data_cleaned.xlsx')


# reformatting data
data <- subset(data, select = -c(Shop.Followers.Cat, Shop.Rating))

data <- rename(data, c("Verified" = "Verified..Yes.No." , "Shop.Rating" = "Shop.Rating..1.5.", "Bad.Comments" = "Comments.w.rating..3.Count", "Normal.Comments" = "Comments.w.ratings...3..4", "Good.Comments" = "Comments.w.ratings")) 