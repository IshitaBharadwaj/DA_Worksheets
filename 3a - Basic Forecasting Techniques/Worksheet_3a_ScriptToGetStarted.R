# Worksheet 3a
#SRN: 
#Name: 
#I have collaborated with... (SRN, Name) 

#Preamble 
df <- read.csv('sales.csv')
head(df)

#Time series prediction on Sales 
sales <- df$Sales
head(sales)

#Weekly data => frequency =52
sales_timeseries <- ts(sales, frequency = 52, start=c(2010, 2, 5))
sales_timeseries
plot.ts(sales_timeseries)

#Problem 1: use x<-decompose(sales_timeseries) followed by plot 

#Problem 2: use sales_ses <- HoltWinters(sales_ts, gamma=FALSE, beta=FALSE)
#For DES and TES, set appropriate values of the parameters for the same fn

#Problem 3: 
#plot(sales_timeseries, ylab="Weekly Sales")
lines(sales_ses$fitted[,1], lty="dashed", col="red")
lines(sales_des$fitted[,1], lty="dashed", col="blue")
lines(sales_tes$fitted[,1], lty="dashed", col="green")

legend(x="topleft", legend=c('SES', 'DES', 'TES'), col=c('red', 'blue', 'green'), lty="dashed")

#Linear regression: Convert date to numeric OR drop the date column and use lm
#sales_mlr=lm(Sales ~ . - Date, data = df)
#summary(sales_mlr)

plot.ts(sales_timeseries)
sales_mlr_pred <- ts(predict(reg), frequency = 52, start=c(2010,2,5))
lines(reg_pred, lty="dashed", col="blue")

#Problem 4: use the command 'croston'
library(forecast) 
laptop_demand <- df$Laptop_Demand
laptop_demand_ts <- ts(laptop_demand, frequency = 52, start = c(2010, 2, 5))
plot.ts(laptop_demand_ts)

#use croston with laptop_demand_ts, h=143
#plot the result using lines as shown on line 83

#Problem 5: sample metrics below; compute rmse and mape for every model 
#Comment on which model is better 
library(Metrics)
sales_ses_mape <- mape(sales_timeseries, sales_ses$fitted[,1])

