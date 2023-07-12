#install packages
install.packages("TTR")

#load libraries
library(tidyverse)
library(TTR)

#set working directory
setwd("C:/Users/kspen/OneDrive/Documents")

#read dataset into R
airdf <- read.csv("airfare.csv")
view(airdf)

#Create a time series plot of the average airfare data from 2004 to 2018
ggplot(data = airdf, mapping=aes(x=Year, y=Airfare))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = seq(2004, 2018, by=1))
labs(title="Average Airfare", x= "Year", y="Airfare")

#Create a vector for the actual yearly airfare
airfare_actuals <- airdf$Airfare

#Use a SIMPLE MOVING AVERAGE with n=3 (3 years) to calculate a forecast for the
#average airfare in 2019
airsma<- SMA(airfare_actuals, n=3)
airsma

#Adjust the vector of predicted values to align with the actual values vector
airsma_pred <- c(NA, airsma[-length(airsma)])

#Create functions for the accuracy measures
mae<-function(actual, pred){
  mae<-mean(abs(actual-pred), na.rm=TRUE)
  return (mae)}

mse<-function(actual, pred){
  mse<-mean((actual-pred)^2, na.rm=TRUE)
  return(mse)}

rmse<- function(actual, pred){
  rmse<-sqrt(mean((actual-pred)^2, na.rm=TRUE))
  return(rmse)}

mape<-function(actual, pred){
  mape<-mean(abs((actual-pred)/actual), na.rm=TRUE)*100
  return(mape)}

#accuracy measures for the simple moving average method.
mae(airfare_actuals, airsma_pred)
mse(airfare_actuals, airsma_pred)
rmse(airfare_actuals, airsma_pred)
mape(airfare_actuals, airsma_pred)

################################################################################

#Use a SIMPLE EXPONENTIAL SMOOTHING with a smoothing constant of 0.2 to calculate
#a forecast for the average airfare in 2019.
airexp <-EMA(airfare_actuals, n=1,ratio=.2)
airexp

#Adjust the vector of predicted values to align with the actual values vector
airexp_pred <- c(NA, airexp[-length(airexp)])

#accuracy measures for the simple exponential smoothing method  with constant 0.2
mae(airfare_actuals, airexp_pred)
mse(airfare_actuals, airexp_pred)
rmse(airfare_actuals, airexp_pred)
mape(airfare_actuals, airexp_pred)

#Use a SIMPLE EXPONENTIAL SMOOTHING with a smoothing constant of 0.8 to calculate a
#forecast for the average airfare in 2019.
airexp8 <-EMA(airfare_actuals, n=1,ratio=.8)
airexp8

#Adjust the vector of predicted values to align with the actual values vector
airexp8_pred <- c(NA, airexp8[-length(airexp8)])

#accuracy measures for the exponential smoothing method with constant 0.8.
mae(airfare_actuals, airexp8_pred)
mse(airfare_actuals, airexp8_pred)
rmse(airfare_actuals, airexp8_pred)
mape(airfare_actuals, airexp8_pred)

##################################################################

#read dataset into R
warnerdf <- read.csv("warner_music.csv")
view(warnerdf)

#Create a time series plot of the Warner Music Group data.
ggplot(data = warnerdf, mapping=aes(x=Quarter, y=Revenue))+
  geom_line(group=1)+
  geom_point()+
  theme(axis.text.x=element_text(angle=90))
labs(title="Warner Music Quarterly Revenue 2015 to 2021", x= "Quarter", y="Revenue")

#add a column of consecutive numbers corresponding to each quarter
warnerdf$Time <- 1:nrow(warnerdf)

#Use LINEAR REGRESSION to model the trend in the time series.
warnerreg1<-lm(Revenue ~ Time, data=warnerdf)
summary(warnerreg1)

#Create a vector for the actual quarterly revenue
warner_actuals <- warnerdf$Revenue

#Create an object with the time periods to use for the prediction
new <- data.frame(Time=c(26, 27, 28, 29))
predict(warnerreg1, newdata=new)

#Create a vector of predicted values
warner_pred1 = predict(warnerreg1)

#accuracy measures for predicted values on the regression.
mae(warner_actuals, warner_pred1)
mse(warner_actuals, warner_pred1)
rmse(warner_actuals, warner_pred1)
mape(warner_actuals, warner_pred1)

#Create dummy variables corresponding to each quarter
warnerdf$Q1 <- ifelse(grepl("Q1", warnerdf$Quarter), 1,0)
warnerdf$Q2 <- ifelse(grepl("Q2", warnerdf$Quarter), 1,0)
warnerdf$Q3 <- ifelse(grepl("Q3", warnerdf$Quarter), 1,0)
warnerdf$Q4 <- ifelse(grepl("Q4", warnerdf$Quarter), 1,0)

#Use LINEAR REGRESSION to model both the trend and the seasonality in the time series.
warnerreg2 <- lm(Revenue ~ Time+Q2+Q3+Q4, data=warnerdf)
summary(warnerreg2)

#Create a vector of predicted values
warner_lmpred2 = predict(warnerreg2)

#accuracy measures for predicted values based on the regression.
mae(warner_actuals, warner_lmpred2)
mse(warner_actuals, warner_lmpred2)
rmse(warner_actuals, warner_lmpred2)
mape(warner_actuals, warner_lmpred2)

#Forecast Q1, Q2, Q3, and Q4 revenues for 2022 using the results of the regression
#analysis modeling both trend and seasonality.

#create an object with time periods to use for the prediction
new <- data.frame(Time=c(26,27,28,29),Q2=c(0,1,0,0), Q3=c(0,0,1,0), Q4=c(0,0,0,1))
predict(warnerreg2, newdata=new)

###############################################################################

#read dataset into R
amazondf <- read.csv("amazon_web_services.csv")
view(amazondf)

#Create a time series plot of the Amazon Web Services revenue data.
ggplot(data = amazondf, mapping=aes(x=Quarter, y=Revenue))+
  geom_line(group=1)+
  geom_point()+
  theme(axis.text.x=element_text(angle=90))
labs(title="Amazon Web Services Quarterly Revenue", x= "Quarter", y="Revenue")

#add a column of consecutive numbers corresponding to each quarter
amazondf$Time <- 1:nrow(amazondf)

#Use LINEAR REGRESSION first to model the trend in the time series.
amazonreg1<-lm(Revenue ~ Time, data=amazondf)
summary(amazonreg1)
 
#Create a vector for the actual quarterly revenue
amazon_actuals <- amazondf$Revenue

#Create a vector of predicted values
amazon_pred1 = predict(amazonreg1)

#accuracy measures for predicted values based on the regression
mae(amazon_actuals, amazon_pred1)
mse(amazon_actuals, amazon_pred1)
rmse(amazon_actuals, amazon_pred1)
mape(amazon_actuals, amazon_pred1)

#Create a new variable that squares the time variable
amazondf$Time2 <- amazondf$Time^2

#Use QUADRATIC REGRESSION MODEL to create a regression equation for forecasting
liregquad <- lm(Revenue ~ Time + Time2, data=amazondf)
summary(liregquad)

#Create a vector of predicted values generated from the quadratic regression
amazon_pred2 = predict(liregquad)

#accuracy measures for predicted values based on the regression.
mae(amazon_actuals, amazon_pred2)
mse(amazon_actuals, amazon_pred2)
rmse(amazon_actuals, amazon_pred2)
mape(amazon_actuals, amazon_pred2)

#Forecast Q1, Q2, Q3, and Q4 revenues for 2022 using the results of the quadratic
#regression analysis.
new <- data.frame(Time=c(33,34,35,36), Time2=c(1089, 1156, 1225, 1296))
predict(liregquad, newdata=new)


