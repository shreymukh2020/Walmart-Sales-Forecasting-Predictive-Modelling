#######################################################################################
#PROPHET MODELLING

library("zoo")
library('astsa')
library("readr")
library("forecast")
library("dplyr")
library("pracma")
library("xts")
library("marima")
library("tidyverse")
library("zoo")
library("prophet")
library("data.table")
library("ggplot2")
library("reshape2")


data <- read.csv("/Users/pinkienguyen/Documents/GSU - MSA:MAS/MSA 8200/Mini Project/walmart-recruiting-store-sales-forecasting/train.csv")
data <- data %>% group_by(Date) %>% summarize(sales = sum(Weekly_Sales))

#Renaming the columns to use in the Prophet model
data = rename(data, ds = Date)
data = rename(data, y = sales)

#Splitting the test and training datasets 80:20.
train = data[1:114,]
test = data[115:143,]


acf(train$y, lag.max = 120)

#Prophet Model training
P = prophet(train, n.changepoints = 5)

future <- make_future_dataframe(P, periods = 29, freq = 'week')
forecast <- predict(P, future)

#Plotting Prophet Model components
prophet_plot_components(P, forecast)

#Plotting the forecasted sales
plot(P, forecast, xlab="Week", ylab="Sales") +
  add_changepoints_to_plot(P, threshold = 0.1, cp_color = "red", cp_linetype="dashed", trend = TRUE)

#Calculating MSPE
MSPE = mean(abs((tail(forecast$yhat,29)-test$y)/test$y))*100
MSPE


#Model Experimentation
#1. Introducing Weekly Seasonality into the model
P2 = prophet(train, n.changepoints = 5, weekly.seasonality = TRUE)
forecast2 <- predict(P2, future)

#Plotting the forecasted sales
plot(P2, forecast2, xlab="Week", ylab="Sales") +
  add_changepoints_to_plot(P2, threshold = 0.1, cp_color = "red", cp_linetype="dashed", trend = TRUE)

#Calculating MSPE
MSPE2 = mean(abs((tail(forecast2$yhat,29)-test$y)/test$y))*100
MSPE2

#2. Modeling Holiday Effect 
data = read_csv("/Users/pinkienguyen/Documents/GSU - MSA:MAS/MSA 8200/Mini Project/walmart-recruiting-store-sales-forecasting/train.csv")

holidays = data.frame(holiday = "holiday",
                      ds = as.Date(c(filter(data, IsHoliday==TRUE)$Date)))

data2 = data %>% group_by(Date) %>% summarize(sales = sum(Weekly_Sales))

data2 = rename(data2, ds = Date)
data2 = rename(data2, y = sales)

#Splitting datasets 80:20.
train = head(data2, 114)
test = tail(data2, 29)

#Model Training
P3 = prophet(train, n.changepoints = 5, holidays = holidays)
forecast3 <- predict(P3, future)

#Plotting the forecasted sales
plot(P3, forecast3, xlab="Week", ylab="Sales") +
  add_changepoints_to_plot(P3, threshold = 0.1, cp_color = "red", cp_linetype="dashed", trend = TRUE)

#Calculating MSPE
MSPE3 = mean(abs((tail(forecast3$yhat,29)-test$y)/test$y))*100
MSPE3


################################
#FORECASTING SALES for 14 weeks with chosen parameters beyond available data
P4 = prophet(data2, n.changepoints = 5)
future1 <- make_future_dataframe(P, periods = 43, freq = 'week')
forecast4 <- predict(P4, future1)

#Plotting the forecasted sales for the next 14 weeks
plot(P4, forecast4, xlab="Week", ylab="Sales") +
  add_changepoints_to_plot(P4, threshold = 0.1, cp_color = "red", cp_linetype="dashed", trend = TRUE)

#########################################################################################################