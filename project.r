library(ggplot2)
library(httr)
library(jsonlite)
library("writexl")

######## URLS TO USE ###########
# alternate url: https://shopee.sg/api/v4/product/get_shop_info?shopid=194330690
orderBrush <- read.csv("order_brush_order.csv")
shopIds <- unique(orderBrush$shopid)

# initialize the empty data frame
df_shopId <- data.frame(matrix(ncol = 10, nrow = 0))
names(df_shopId) <- c('id', 'shop_name', 'is_shopee_verified', 'is_preferred_plus_seller', 'rating_star', 'response_rate', 'follower_count', 'rating_bad', 'rating_good', 'rating_normal')

# This function gets the whole data from the API ENDPOINT
apiCallShopId <- function(shop_id){
  res = GET("https://shopee.sg/api/v4/shop/get_shop_detail?sort_sold_out=0", query = list(shopid = shop_id))
  data <- fromJSON(rawToChar(res$content))
  jsonData <- data$data
  return(jsonData)
}

count <- 0
for(i in shopIds){
  data <- apiCallShopId(i)
  df <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(df) <- c('id', 'shop_name', 'is_shopee_verified', 'is_preferred_plus_seller', 'rating_star', 'response_rate', 'follower_count', 'rating_bad', 'rating_good', 'rating_normal')
  df <- c(i, data$name, data$is_shopee_verified, data$is_preferred_plus_seller, data$rating_star, data$response_rate, data$follower_count, data$rating_bad, data$rating_good, data$rating_normal)
  df_shopId <- rbind(df_shopId, df)
  names(df_shopId) <- c('id', 'shop_name', 'is_shopee_verified', 'is_preferred_plus_seller', 'rating_star', 'response_rate', 'follower_count', 'rating_bad', 'rating_good', 'rating_normal')
  count <- count + 1
  print(count)
}

write_xlsx(df_shopId,"C:\\Users\\wongk\\Documents\\R Studio Projects\\TBA2104_Predictive_Analytics_Project\\shopid_data.xlsx")


#https://shopee.sg/api/v4/shop/get_categories?limit=20&offset=0&shopid=194330690
apiCallShopCategory <- function(shop_id){
  res = GET("https://shopee.sg/api/v4/shop/get_categories?limit=20&offset=0", query = list(shopid = shop_id))
  data <- fromJSON(rawToChar(res$content))
  jsonData <- data$data
  return(jsonData)
}

df_shopCategory <- data.frame(matrix(ncol = 2, nrow = 0))
names(df_shopId) <- c('id', 'shop_categories')


count <- 0
for(i in shopIds){
  data <- apiCallShopCategory(i)
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df) <- c('id', 'shop_categories')
  df <- c(i, data$shop_categories$display_name)
  df_shopCategory <- rbind(df_shopCategory, df)
  names(df_shopCategory) <- c('id', 'shop_categories')
  count <- count + 1
  print(count)
}

hello <- "hi"

