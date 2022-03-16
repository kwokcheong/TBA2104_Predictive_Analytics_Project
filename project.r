library(ggplot2)
library(httr)
library(jsonlite)

######## URLS TO USE ###########
# alternate url: https://shopee.sg/api/v4/product/get_shop_info?shopid=194330690


orderBrush <- read.csv("order_brush_order.csv")
shopIds <- orderBrush$shopid

# initialize the empty data frame
df_shopId <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_shopId) <- c('id', 'shop_name')

# This function gets the whole data from the API ENDPOINT
apiCallShopId <- function(shop_id){
  res = GET("https://shopee.sg/api/v4/shop/get_shop_detail?sort_sold_out=0", query = list(shopid = shop_id))
  data <- fromJSON(rawToChar(res$content))
  jsonData <- data$data
  return(jsonData)
}

# This function takes in the list we got from the apiCallShopId to get the name
getShopName <- function(data){
  account <- data$account
  if(!is.null(account)){
    return(data$account$username)
  }
  return(NULL)
}

apiCall <- apiCallShopId(194330690)

shop_name <- apiCall$account$username


for(i in shopIds){
  data <- apiCallShopId(i)
  name <- getShopName(data)
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df) <- c('id', 'shop_name')
  df <- c(i, name)
  rbind(df_shopId, df)
}