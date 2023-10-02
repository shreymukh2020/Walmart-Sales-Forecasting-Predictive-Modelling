

### This code is for the part 3 where we discuss aboutusing alternate approach
## of dividing the dataset and modeling for each different store to mitigate the correlation between type of stores
## The Code is divided in to three parts a) EDA b) Sarima without additional variables c) With Variables
##### WALMART STORE SALES FORECASTING - EDA
#####
########################################################

require("zoo")
library(dplyr)
setwd("/Users/namitsrivastava/Documents/Documents/GSU/Predictive Analysis/walmart-recruiting-store-sales-forecasting")

#Data extraction

train = read.csv("train.csv")
str(train)

train.Store_Date = train %>% group_by(Store,Date) %>% summarise(Weekly_Sales_Store = sum(Weekly_Sales))
str(train.Store_Date)
head(train.Store_Date,5)
max(train.Store_Date$Date) #"2012-10-26"
min(train.Store_Date$Date) #"2010-02-05"

#features has info about store-weekly other features
features = read.csv("features.csv")
str(features)
max(features$Date) #"2013-07-26"
min(features$Date) #"2010-02-05"

#apply left join on train data with features data to get more variables
data <- merge(train.Store_Date, features, by.x = c("Store", "Date"), 
              by.y = c("Store","Date"), all.x = TRUE, all.y = FALSE)
str(data)

#store has info about store type and size
stores = read.csv("stores.csv")
str(stores)
max(features$Date) #"2013-07-26"
min(features$Date) #"2010-02-05"

#apply left join on data with stores data to get more variables
data <- merge(data, stores, by.x = c("Store"), 
              by.y = c("Store"), all.x = TRUE, all.y = FALSE)
str(data)


#find the summary of data
summary(data)
#MarkDown(s) column has lot of NA values in it

names(data)
#lets remove these columns with lot of NA's from our dataset as they dont give much information
#and combining all store data to a week level. i.e take data at Date level
data.subset <- data[c("Store","Date","Weekly_Sales_Store","Temperature",
                      "Fuel_Price","CPI","Unemployment")]

#data need to be aggregated at Date date level
data.week <- data.subset %>% group_by(Date) %>% summarise(Weekly_Sales_allStore = sum(Weekly_Sales_Store),
                                                          Weekly_Temperature_allStore = mean(Temperature)
)


#get count of store based on type

count.store_type = data %>% group_by(Type) %>% summarise(count_Store = n())
count.store_type

store_sample = data %>% group_by(Type,Store) %>% summarise(count_Store = n())
store_sample
write.csv(x=store_sample, file="store_sample.csv",row.names = FALSE)

library(ggplot2);
ggplot(count.store_type, aes(as.factor(Type), count_Store)) +
  geom_bar(stat = "identity") + 
  labs(y = "Store count", x = "Type");

#Type A has highest store count


#lets plot store against size
#even correlation plot shows significant correlation between them
plot(data$Weekly_Sales_Store, data$Size,xlab="Weekly store sales", ylab="Size")

#lets pick one/two store per type in random from the data
#Type A: Store 31
#Type B: Store 35
#Type C: Store 30

#subset data for these store
#store 31
store_31 <- data.subset[data.subset$Store == 31,]
dim(store_31) #143   7
write.csv(x=store_31, file="store_20.csv",row.names = FALSE)

#store 35
store_35 <- data.subset[data.subset$Store == 35,]
dim(store_35) #143   7
write.csv(x=store_35, file="store_35.csv",row.names = FALSE)

#Store 30
store_30 <- data.subset[data.subset$Store == 30,]
dim(store_30) #143   7
write.csv(x=store_30, file="store_30.csv",row.names = FALSE)



###########################################################################
#######################################################
#####
##### WALMART STORE SALES FORECASTING - SARIMA without additional variables
#####
########################################################


#A)	Split data to train and test
#B)	Check acf and pacf for the store data and find right models
#C)	Convert to time series data
#D)	Looked at the various components of timeseries
#E)	Observed some seasonality and decreasing trend
#F)	Removed trend in timeseries
#G)	Used transformed data to find appropriate model


setwd("/Users/namitsrivastava/Documents/Documents/GSU/Predictive Analysis/walmart-recruiting-store-sales-forecasting")
#read data of stores
store_31 = read.csv("store_31.csv")
dim(store_20)

store_35 = read.csv("store_35.csv")
dim(store_3)

store_30 = read.csv("store_30.csv")
dim(store_30)

#####################################################################
## STORE 31 Type A
#####################################################################
# split data to train and test
# check acf and pacf for the store data and find right models
# Taking only Weekly_Sales_store column for this analysis
train_store31 = store_31$Weekly_Sales_Store[1:110]
test_store31  = store_31$Weekly_Sales_Store[111:143]

#convert to time series data
train_store31.ts = ts(train_store31,frequency = 52)
plot(train_store31.ts)

#lets take look at various components of timeseries
plot(decompose(train_store31.ts))
# we observe some seasonality and increasing trend

acf2(train_store31.ts, max.lag = 100)

#lets try to remove trend in timeseries
train_store31.ts.transformed = diff(log(train_store31.ts))
plot(train_store31.ts.transformed)
plot(decompose(train_store31.ts.transformed))

acf2(train_store31.ts.transformed, max.lag = 100)


#the data looks better, lets try to remove seasonality
train_store31.ts.decompose = decompose(train_store31.ts)
train_store31.ts.noSeasonal = train_store31.ts - train_store31.ts.decompose$seasonal
plot(train_store31.ts.noSeasonal)
plot(decompose(train_store31.ts.noSeasonal))


acf2(train_store31.ts.noSeasonal, max.lag = 100)
#though the data looks more random now, acf and pacf graph gives no clue of what model I should use

#lets use original data to find appropriate model
acf2(train_store31.ts, max.lag = 100)
#no seasonal part (s=0), 


#Model diagnostic
#regular ar(1)
#arma(1,0)*(0,0)
sarima(train_store31.ts,1,0,0,0,0,0,0)
#most of the pvalue are below or on the blue line, so its not good model

#regular ma(1)
#arma(0,1)*(0,0)
sarima(train_store31.ts,0,0,1,0,0,0,0)
#most of the pvalue are below or on the blue line, so its not good model

#arma(1,1) no seasonal
#arma(1,1)*(0,0)
sarima(train_store31.ts,1,0,1,0,0,0,0)
#p value in Ljung box improved, so far this is the better model we got

library(forecast)
auto.arima(train_store31.ts, trace=TRUE)

#lets also try a model with only seasonal diff component
#and lets take diff of seasonal component
#arma(0,0,0)*(0,1,0)
sarima(train_store31.ts,0,0,0,0,1,0,0)
#pvalues are above blue line, so no correlation between residuals
#but QQ plot shows huge deviation

#Model fitting
#lets fit the data on #arma(1,1)*(0,0) - better model
fit <- Arima(train_store31.ts,c(1,0,1),seasonal=list(order=c(0,0,0),period=52))

#PREDICTION
fit2 <- Arima(test_store31,c(1,0,1),seasonal=list(order=c(0,0,0),period=52),model=fit)
onestep <- fitted(fit2)
plot(onestep)
print(paste("error of prediction",mean(test_store31-as.vector(onestep)^2)))

#lets use arima function to predict
#PREDICTION
predict = predict(arima(test_store31, order = c(1,0,1)), n.ahead = 33)
error1 = test_store31 - predict$pred
error1

#MAPE
mape1 = 100*mean((abs(error1)/test_store31)^2)
print(paste("MSPE",mape1))

mspe=100* mean(((test_store31$Weekly_Sales_Store - predict$pred)/test_store31$Weekly_Sales_Store)^2)
print(paste('MSPE value for the sales of store 31 is:', round(mspe,3),'%'))

#####################################################################
## STORE 35 Type B
#####################################################################
train_store35 = store_35$Weekly_Sales_Store[1:110]
test_store35  = store_35$Weekly_Sales_Store[111:143]

#convert to time series data
train_store35.ts = ts(train_store35,frequency = 52)
plot(train_store35.ts)

#lets take look at various components of timeseries
plot(decompose(train_store35.ts))
# we observe some seasonality and increasing trend

acf2(train_store35.ts, max.lag = 100)

#lets try to remove trend in timeseries
train_store35.ts.transformed = diff(log(train_store35.ts))
plot(train_store35.ts.transformed)
plot(decompose(train_store35.ts.transformed))

acf2(train_store35.ts.transformed, max.lag = 100)

#the data looks better, lets try to remove seasonality
train_store35.ts.decompose = decompose(train_store35.ts)
train_store35.ts.noSeasonal = train_store35.ts - train_store35.ts.decompose$seasonal
plot(train_store35.ts.noSeasonal)
plot(decompose(train_store35.ts.noSeasonal))
#data looks more random now

acf2(train_store35.ts.noSeasonal, max.lag = 100)
#though the data looks more random now, acf and pacf graph gives no clue of what model I should use

#lets use original data to find appropriate model
acf2(train_store35.ts, max.lag = 100)
#no seasonal part (s=0), 


#Model diagnostic
#regular ar(1)
#arma(1,0)*(0,0)
sarima(train_store35.ts,1,0,0,0,0,0,0)
#most of the pvalue are below or on the blue line, so its not good model

#regular ma(4)
#arma(0,4)*(0,0)
sarima(train_store35.ts,0,0,4,0,0,0,0)
#most of the pvalue are above the blue line, so its good model

#arma(1,4) no seasonal
#arma(1,4)*(0,0)
sarima(train_store35.ts,1,0,4,0,0,0,0)
#p value in Ljung box improved, this is a good model
#but we want to use only the simple model so lets use ma(4) to predict values

library(forecast)
auto.arima(train_store35.ts, trace=TRUE)

#lets also try a model with only seasonal diff component
#and lets take diff of seasonal component along with ar(1)
#arma(1,0,4)*(0,1,0)
sarima(train_store35.ts,1,0,4,1,0,1,0)
#pvalues are above blue line, so no correlation between residuals
#but QQ plot shows huge deviation

#Model fitting
#lets fit the data on #arma(0,4)*(0,0) - better model
fit <- Arima(train_store35.ts,c(0,0,4),seasonal=list(order=c(0,0,0),period=52))

#PREDICTION
fit2 <- Arima(test_store35,c(1,0,4),seasonal=list(order=c(1,0,0),period=52),model=fit)
onestep <- fitted(fit2)
plot(onestep)
print(paste("error of prediction",mean(test_store35-as.vector(onestep)^2)))

#lets use arima function to predict
#PREDICTION
predict = predict(arima(test_store35, order = c(0,0,4)), n.ahead = 33)
error1 = test_store35 - predict$pred
error1

#MAPE
mape1 = 100*mean((abs(error1)/test_store35)^2)
print(paste("MSPE",mape1))


mspe=100* mean(((test_store30$Weekly_Sales_Store - result_30$pred)/test_store30$Weekly_Sales_Store)^2)
print(paste('MSPE value for the sales of store 35 is:', round(mspe,3),'%'))
#####################################################################
## STORE 30 Type C
#####################################################################
# split data to train and test
# check acf and pacf for the store data and find right models
train_store30 = store_30$Weekly_Sales_Store[1:110]
test_store30  = store_30$Weekly_Sales_Store[111:143]

#convert to time series data
train_store30.ts = ts(train_store30,frequency = 52)
plot(train_store30.ts)

#lets take look at various components of timeseries
plot(decompose(train_store30.ts))
# we observe some seasonality and decreasing trend

acf2(train_store30.ts, max.lag = 100)

#lets try to remove trend in timeseries
train_store30.ts.transformed = diff(log(train_store30.ts))
plot(train_store30.ts.transformed)
plot(decompose(train_store30.ts.transformed))

acf2(train_store30.ts.transformed, max.lag = 100)

#the data looks better, lets try to remove seasonality
train_store30.ts.decompose = decompose(train_store30.ts)
train_store30.ts.noSeasonal = train_store30.ts - train_store30.ts.decompose$seasonal
plot(train_store30.ts.noSeasonal)
plot(decompose(train_store3.ts.noSeasonal))
#data looks more random now

acf2(train_store30.ts.noSeasonal, max.lag = 100)
#though the data looks more random now, acf and pacf graph gives no clue of what model I should use

#lets use transformed data to find appropriate model
acf2(train_store30.ts.transformed, max.lag = 100)
#no seasonal part (s=0), 
#pacf cut of at lag 3, regular ar(3)


#Model diagnostic
#regular ar(3)
#arma(3,0)*(0,0)
sarima(train_store30.ts,3,0,0,0,0,0,0)
#most of the pvalue are below or on the blue line, so its not good model

#regular ma(1)
#arma(0,1)*(0,0)
sarima(train_store30.ts,0,0,1,0,0,0,0)
#most of the pvalue are below or on the blue line, so its not good model

#arma(3,1) no seasonal
#arma(3,1)*(0,0)
sarima(train_store3.ts,3,0,1,0,0,0,0)
#p value in Ljung box improved, this is a good model

library(forecast)
auto.arima(train_store30.ts, trace=TRUE)

#lets also try a model with only seasonal diff component
#and lets take diff of seasonal component along with arma(3,1)
#arma(3,0,1)*(0,1,0)
sarima(train_store30.ts,3,0,1,0,1,0,0)
#pvalues are above blue line, so no correlation between residuals
#but QQ plot shows huge deviation

#Model fitting
#lets fit the data on #arma(3,0,1)*(0,0,0) - better model
fit <- Arima(train_store30.ts,c(3,0,1),seasonal=list(order=c(0,0,0),period=52))

#PREDICTION
fit2 <- Arima(test_store30,c(3,0,1),seasonal=list(order=c(0,0,0),period=52),model=fit)
onestep <- fitted(fit2)
plot(onestep)
print(paste("error of prediction",mean(test_store30-as.vector(onestep)^2)))

#lets use arima function to predict
#PREDICTION
predict = predict(arima(test_store30, order = c(3,0,1)), n.ahead = 33)
error1 = test_store30 - predict$pred
error1

#MAPE
mape1 = 100*mean((abs(error1)/test_store30)^2)
print(paste("MSPE",mape1))

mspe=100* mean(((test_store30$Weekly_Sales_Store - result_30$pred)/test_store30$Weekly_Sales_Store)^2)
print(paste('MSPE value for the sales of store 35 is:', round(mspe,3),'%'))


####################

library(astsa)

#importing data
store_35=read.csv("store_35.csv")
store_20=read.csv("store_20.csv")
store_30=read.csv("store_30.csv")

head(store_3)

#handling date column
store_35$Date <- as.Date(store_35$Date , format = "%Y-%m-%d")
store_20$Date <- as.Date(store_20$Date , format = "%Y-%m-%d")
store_30$Date <- as.Date(store_30$Date , format = "%Y-%m-%d")

#Split test train
#store35
train_store35 = store_35[1:110, ]
test_store35  = store_35[111:143, ]
#store31
train_store31 = store_20[1:110, ]
test_store31  = store_20[111:143, ]
#store30
train_store30 = store_30[1:110, ]
test_store30  = store_30[111:143, ]


############################################
################ store 35 Type B ##################

#fitting LR model
fit_3 <- lm(Weekly_Sales_Store ~ Date + Temperature + Fuel_Price + CPI + Unemployment, data=train_store35)
summary(fit_3) 
#Findings: We found that only 'Temperature' attribute is significant for making predictions.

#understanding ACF_PACF of residual
acf2(fit_3$residuals)

#diagnosis
sarima(train_store35$Weekly_Sales_Store,5,0,5,0,0,0,S=7,xreg=cbind(train_store35$Temperature))

#predictions
result_3=sarima.for(train_store35$Weekly_Sales_Store, n.ahead = 33,5,0,5,0,0,0,S=7,
                    newxreg=cbind(train_store35$Temperature))

#mspe
mspe=100*mean(((test_store35$Weekly_Sales_Store - result_3$pred)/test_store35$Weekly_Sales_Store)^2)
print(paste('MSPE value for the sales of store 35 is:', round(mspe,3)))




############################################
################ store 31 Type A ##################

#fitting LR model
fit_31 <- lm(Weekly_Sales_Store ~ Date + Temperature + Fuel_Price + CPI + Unemployment, 
             data=train_store31)
summary(fit_31)
#Findings: We found that only 'Temperature' attribute is significant for making predictions.

#understanding ACF_PACF of residual
acf2(fit_31$residuals)

#diagnosis
sarima(train_store31$Weekly_Sales_Store,5,0,5,0,0,0,S=7,xreg=cbind(train_store31$Temperature))

#predictions
result_31=sarima.for(train_store31$Weekly_Sales_Store, n.ahead = 33,5,0,5,0,0,0,S=7,
                     newxreg=cbind(train_store31$Temperature))

#mspe
mspe=100*mean(((test_store31$Weekly_Sales_Store - result_31$pred)/test_store31$Weekly_Sales_Store)^2)
print(paste('MSPE value for the sales of store 31 is:', round(mspe,3),'%'))







############################################
################ store 30 ##################

#fitting LR model
fit_30 <- lm(Weekly_Sales_Store ~ Date + Temperature + Fuel_Price + CPI + Unemployment, 
             data=train_store30)
summary(fit_30)

#understanding ACF_PACF of residual
acf2(fit_30$residuals)

#diagnosis
sarima(train_store30$Weekly_Sales_Store,7,0,7,0,0,0,S=7,
       xreg=cbind(train_store30$Temperature, train_store30$Date, train_store30$Unemployment))

#predictions
result_30=sarima.for(train_store30$Weekly_Sales_Store, n.ahead = 33,5,0,5,0,0,0,S=7,
                     newxreg=cbind(train_store30$Temperature, train_store30$Date, train_store30$Unemployment))

#mspe
mspe=100* mean(((test_store30$Weekly_Sales_Store - result_30$pred)/test_store30$Weekly_Sales_Store)^2)
print(paste('MSPE value for the sales of store 30 is:', round(mspe,3),'%'))





result_3=sarima.for(train_store35$Weekly_Sales_Store, n.ahead = 33,5,0,5,0,0,0,S=7,
                    newxreg=cbind(train_store35$Temperature))

result_31=sarima.for(train_store31$Weekly_Sales_Store, n.ahead = 33,5,0,5,0,0,0,S=7,
                     newxreg=cbind(train_store31$Temperature))
result_30=sarima.for(train_store30$Weekly_Sales_Store, n.ahead = 33,5,0,5,0,0,0,S=7,
                     newxreg=cbind(train_store30$Temperature, train_store30$Date, train_store30$Unemployment))


