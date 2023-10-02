# Loading all libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(forecast)
library(tseries)
library(astsa)
library(Metrics)

setwd("C:/Users/shalm/OneDrive - Georgia State University/Documents/R/Predictive_Analytics/Walmart_Forecasting/")
# Reading the data
train <- read.csv("train.csv")

# Separating the date format
train$Date <- as.Date(train$Date, format = "%Y-%m-%d")


# Checking for missing values if any
colSums(is.na(train))
# There are no missing values 

# Data Visualization 
# Plotting weekly sales
train %>% group_by(Date) %>% summarise(sales = sum(Weekly_Sales)) %>% ggplot(aes(x = Date, y = sales)) + geom_line() + scale_y_log10() + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 weeks") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#train %>% group_by(Store, Dept) %>% summarize(count_wk = n_distinct(Date)) %>% ggplot(aes(x = count_wk)) + geom_histogram()

# Observation:
#a) Sales peak the most in the winter months (Nov and Dec) every year. This is during the holiday and festive weeks of Thanks Giving, Christmas and New Year. 
#b) There are lesser spike in sales during April-May weeks, that may be due to Easter and Labor day.



sales_agg<-data.frame(train$Date,train$Weekly_Sales)
sales_a<-ts(sales_agg[,-1],frequency =12,start=c(2010,2),end=c(2012,10))
head(sales_a)
plot(sales_a)

# Plotting the monthly sales
Plot_1 <- plot(sales_a,xlab="Year",ylab="Sales (USD)",main="Walmart Sales")
# The above plot definitely shows that the time series is not stationary. Need to decompose the time series to deep-dive. 

#Visualizing the data using time-series decomposition to decompose our time series into three distinct components: trend, seasonality, and noise.
sales_a_decomp <- decompose(sales_a)
plot(sales_a_decomp, yax.flip = TRUE)
#a) There is definitely seasonality observed in the data - Peaks in the end of the year and small peaks in the summer months. 
#b) The trend in Walmart sales decreased rapidly from mid-year 2010 and seems to be constant from mid-year 2011 onwards.

# Checking if the time series is stationary
adf.test(sales_a)
# The p value is very high, i.e., 0.1171. Hence, we fail to reject the null hypothesis or can say that the 'series is non-stationary'. 

# Differencing the time series
sales_d <- diff(sales_a)
plot(sales_d)
adf.test(sales_d)
# As we can see from the plot, we have successfully stationrized the time series. Let's re-confirm with Dickey-Fuller Test. 
# P value is 0.02 indicating we can reject the null hypothesis 'series is non-stationary'. Thus, the time series is stationary after the transformation. 

# Plotting the ACF and PACF using the differenced series find the AR and MA terms.
acf2(sales_d)
#a) This seems to be a seasonal arima model. The ACF spikes at 2 places, while, the PACF has a spike at lag 1.

# Plotting the ACF and PACF using the differenced series find the AR and MA terms.
acf(sales_d,lag.max = 10)
pacf(sales_d,lag.max = 10)

#a) We know there is seasonality in the time series. As per the ACF plot, it seems to be an AR(1) model. PACF spikes at 2 positions, indicating an MA(2) model
# Further the seasonal pattern is 12 months. Hence, the order of the model as as below; 
 
fit <- arima(sales_d, c(1, 1, 2),seasonal = list(order = c(0, 0, 1), period = 12))
fit


fit1 <- sarima(sales_d,1,1,2)
fit1

#a. The Standardized residuals show some pattern until end of 2010 where the sales are highest. They again decrease at start of 2011 and then remain constant. Since there is no continuous pattern, it resembles white noise.
#b. Similarly, ACF also resembles the Standardized Residuals in terms of white noise.
#c. QQ plot has few outliers in the beginning but otherwise has a good fit
#d. The P-values are above or equal to the significance level.

tsdiag(fit,gof.lag = 52)
#a) There is no clear pattern for standardized residuals (resemble white noise).
#b) ACF of the residuals (are within the limits) and resemble ACF of white noise.
#c) The p-values are above the significance levels. 

# Predicting the sales on training data
pred_y <- forecast(fit,12)
pred_y
plot(pred_y)
print('Non-stationary series: Yearly')

pred_m <- forecast(fit,52)
pred_m
plot(pred_m)


print('Non-stationary series')
      
#Sales forecasting on training data
sarima.for(sales_d, 12, 2, 1, 2)

# A new dataframe with only monthly sales and plotting the time series
sales_df<-data.frame(train$Date,train$Weekly_Sales)

#Predicting for test data

train <- sales_df %>% slice(1:337256)
str(train)
test <- sales_df %>% slice(337257:421570)
str(test)

# Predicting the sales of the test data
#a) convert test data into time series
sales_t<-ts(test[,-1],frequency =52,start=c(2010,2,5),end=c(2012,10,26))
#b) fit the time series
fit_test <- arima(sales_t, c(1, 1, 2),seasonal = list(order = c(0, 1, 1), period = 52))

pred_t <- forecast(fit_test,52)
pred_t
plot(pred_t)


# MAPE

actual <- c(1036.39229115592, -374.583985770334, -381.963923095491, -165.278632105435, -659.094758297724, -383.36505079437, 437.111062253952, -257.757430214433, -717.066588797427, -939.095150969181, -234.514491633503, 132.85466093747, -796.368011283589, 515.02873387761, -710.837380495689, 183.063945276943, -272.001721862213, -369.089729025445, -192.640641849388, -349.435794216533, 296.290531282502, -339.049968725688, 2159.96378756013, -599.102497633355, -919.520119031669, 287.575074100969, -171.405156974363, 171.043200806504, 949.532893297235, 175.527305455968, -404.201153320643, -961.158399142016, -762.926337530717, 458.627513775852, 1300.76673158411, -320.131402194039, -97.7766532473042, -1266.50772188452, -1149.75274193439, -910.256173712419, -1307.76957208314, -977.075906018981, -759.200242060264, -381.12456024463, -431.593713423315, -175.849474589311, -852.322406859167, -657.386309129518, -1238.3969290411, -1652.05556162522, -1757.52392805184, -837.882587000741)
predicted <- c(3893.79402088054, 3692.19766201801, 21079.2277023924, -8471.3860306178, -20341.6053474951, -2558.21629989294, -2001.5956292711, 788.225305608264, 917.597094632459, -389.198784819034, -711.176550653875, 857.603455793381, -131.219923258762, 982.935070266537, -320.059825540574, 214.288575895838, 326.290260599359, -345.38346426379, -626.859188572814, 327.183728882292, 31.6801088586489, 904.3248132344, 2625.20767322668, 978.694969990913, -521.884364849108, 711.422187394858, 2667.60001544922, 2505.98048789234, 384.190783808, 8590.9182127013, 7993.36643503455, -20770.7742971104, 523.020400979603, 1936.56993495126, 4531.56058710011, 8225.26955933638, 12679.197677981, 1575.82039055967, -23776.0538824447, -6467.52351608706, 542.640717481228, 1589.23329068493, 230.74205719539, 5236.09382080726, 13766.7462643241, 16915.407268865, -33503.7255579245, 204.406658257692, 1197.519070417, 1322.09231352921, -47.0005893711007, 6785.61232082724)
MAPE <- mean(abs((actual - predicted)/actual))
MAPE
# The MSPE value suggests that on average, the forecast is off by ~13%"



