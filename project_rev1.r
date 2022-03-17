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

#The following added by Amy

#concatenate
orderBrush1 <- orderBrush
orderBrush1$shopid_userid <- paste(orderBrush1$shopid,orderBrush1$userid, sep = "")
orderBrush1$event_time <- as.POSIXlt(orderBrush1$event_time)

#retrieve hour
hour <- as.data.frame(orderBrush1$event_time)
colnames(hour) <- c('hours')
hour$hours <- format(as.POSIXct(hour$hours), format = "%H")

#bind hour to the 
orderBrush1 <- cbind(orderBrush1, hour)

#order the column shopid_userid by ascending order
orderBrush1 <- orderBrush1[order(orderBrush1$shopid_userid),]

#find duplicated shopid_userid
orderBrush1$duplicate <- duplicated(orderBrush1$shopid_userid) | duplicated(orderBrush1$shopid_userid, fromLast = TRUE)

#find those with multiple order place within an hour
#.......

#Excel formula
#for line 1
#=IF(AND(E2=E3,AND((F2-F3)>=-1,(F2-F3)<=1)),1,0)
#for line 2
#=IF(OR(AND(E3=E4,AND((F3-F4)>=-1,(F3-F4)<=1)),AND(E3=E2,AND((F3-F2)>=-1,(F3-F2)<=1))),1,0)

