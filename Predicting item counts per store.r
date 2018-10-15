
library(stats)
library(tidyverse)
library(astsa)
library(xts)
library(Ecdat)
library(forecast)
library(sqldf)

sales_train <- read.csv("C:file path/sales_train_v2.csv")
sample_submission <- read.csv("file path/sample_submission.csv")
test_data <- read.csv("file path/test.csv")

head(sales_train)
head(sample_submission)
head(test_data)

sales_train[sales_train$date == "" | sales_train$date == "NA" | sales_train$date == "na"]
sales_train[sales_train$date_block_num == "" | sales_train$date_block_num == "NA"| sales_train$date_block_num == "na"]
sales_train[sales_train$shop_id == "" | sales_train$shop_id == "NA" | sales_train$shop_id == "na"]
sales_train[sales_train$item_id == "" | sales_train$item_id == "NA" | sales_train$item_id == "na"]
sales_train[sales_train$item_price == "" | sales_train$item_price == "NA"| sales_train$item_price == "na"]
sales_train[sales_train$item_cnt_day == "" | sales_train$item_cnt_day == "NA" | sales_train$item_cnt_day == "na"]

train <- aggregate(sales_train$item_cnt_day, by = list(shop_id = sales_train$shop_id,item_id = sales_train$item_id, month = sales_train$date_block_num),FUN= sum)

head(train)

colnames(train)[4]<- "item_cnt_month"

head(train)

is.ts(train)   
series <- ts(train)
is.ts(series)

arima(series[,"item_cnt_month"], order = c(0,0,0))
mean(series[,"item_cnt_month"])
var(series[,"item_cnt_month"])

plot(series[,"item_cnt_month"])
acf(series[,"item_cnt_month"])

sarima(series[,"item_cnt_month"], p= 1,d=0,q=0)
sarima(series[,"item_cnt_month"],p=0,d=0,q=1)

AR_sales <- arima(series[,"item_cnt_month"], order = c(1, 0, 0))

ts.plot(series[,"item_cnt_month"])
AR_sales_fitted <- series[,"item_cnt_month"] - residuals(AR_sales)
points(AR_sales_fitted, type = "l", col = "red", lty = 2)

train_2 <- train
predictions <- data.frame(shop_id = NA,item_cnt_month = NA)
while(nrow(train_2) != 0)
{ 
    key <- train_2$shop_id[1]
    sales_list <- select(train_2, item_cnt_month,month,shop_id) %>% filter(shop_id==key)
    series <- ts(sales_list)
    AR_items <- arima(series[,"item_cnt_month"], order = c(1, 0, 0)) 
    A <- predict(AR_items) 
    predictions <- rbind(predictions,c("shop_id" = key, "item_cnt_month" = A$pred))
    train_2<-train_2[!(train_2$shop_id==key),]
}
predictions <- na.omit(predictions)

head(predictions)
