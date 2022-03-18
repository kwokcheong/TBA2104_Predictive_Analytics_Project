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

#concatenate shopid and userid
orderBrush1 <- orderBrush
orderBrush1$shopid_userid <- paste(orderBrush1$shopid,orderBrush1$userid, sep = "")

#set event_time as date and time format
orderBrush1$event_time <- as.POSIXlt(orderBrush1$event_time)

#order the column shopid_userid by ascending order
orderBrush1 <- orderBrush1[order(orderBrush1$shopid_userid),]

#find duplicated shopid_userid
orderBrush1$duplicate <- duplicated(orderBrush1$shopid_userid) | duplicated(orderBrush1$shopid_userid, fromLast = TRUE)

#find those shopid_userid that with multiple orders place
orderBrush_multipleorder <- orderBrush1[orderBrush1$duplicate == TRUE,]

#find frequency of order for the shopid_userid
freq_orderBrush_multipleorder <- as.data.frame(table(orderBrush_multipleorder$shopid_userid))
colnames(freq_orderBrush_multipleorder) <- c("shopid_userid", "frequency")

orderBrush_multipleorder_freq <- merge(x = orderBrush_multipleorder, y = freq_orderBrush_multipleorder,
                                       by = "shopid_userid")

#find those with more than 3 orders
orderBrush_multipleorder_freq <- orderBrush_multipleorder_freq[orderBrush_multipleorder_freq$frequency >= 3,]

#find the time different between each row (in seconds), assign zero to first row
orderBrush_multipleorder_freq$diff_time <- c(0, diff(orderBrush_multipleorder_freq$event_time))

#identify those order that place within an hour (3600 seconds)
#1 mean order place within an hour
#0 mean order place not wihitn an hour
for(i in 1:length(orderBrush_multipleorder_freq$diff_time)){
  if( orderBrush_multipleorder_freq$diff_time[i] >= -3600 && orderBrush_multipleorder_freq$diff_time[i] <= 3600){
    orderBrush_multipleorder_freq$brushing[i] <- 1
  } else {
    orderBrush_multipleorder_freq$brushing[i] <- 0
  }
}

#only orders that being identify as brushing
orderBrush2 <- orderBrush_multipleorder_freq[orderBrush_multipleorder_freq$brushing == 1,]

#find the frequency of shopid_userid
freq_orderBrush2 <- as.data.frame(table(orderBrush2$shopid_userid))
colnames(freq_orderBrush2) <- c("shopid_userid", "frequency")

freq_orderBrush2 <- merge(x = orderBrush2, y = freq_orderBrush2,
                                       by = "shopid_userid")

#find order with frequency more than equal to 2
#this is because those with frequency 1 should not be brushing
freq_orderBrush2 <- freq_orderBrush2[freq_orderBrush2$frequency.y >= 2,]

#total 743 group of order brushing 
unique(freq_orderBrush2$shopid_userid)

#total 388 shops that suspect to have order brushing 
unique(freq_orderBrush2$shopid)

#total 673 users that suspect to have order brushing
unique(freq_orderBrush2$userid)
