data<- read.csv("infosys stock.csv")
View(data)
summary(data)
data$Close<-as.numeric(data$Close)

data$Date<- as.Date(data$Date, format ="%m/%d/%Y")

#data$Date <- as.Date(data$Date)

class(data$Date)
class(data$Close)
View(data)
summary(data)
library(ggplot2)
plot(as.Date(data$Date,"%d-%b-%y"),data$Close,xlab="Dates",ylab="adjusted closing price",type="l",col="red",main="Adjested closing price of Infosys")
library(tseries,quietly=T)
adf.test(data$Close)


infy_ret <- 100*diff(log(data$Close))
summary(arma(infy_ret,order=c(2,2)))


library(forecast, quietly = T)
infy_ret_train <- infy_ret[1:(0.9 * length(infy_ret))]  # Train dataset
infy_ret_test <- infy_ret[(0.9 * length(infy_ret) + 1):length(infy_ret)]  # Test dataset

fit <- arima(infy_ret_train, order = c(2, 0, 2))
arma.preds <- predict(fit, n.ahead = (length(infy_ret) - (0.9 * length(infy_ret))))$pred
arma.forecast <- forecast(fit, h = 25)

plot(arma.forecast, main = "ARMA forecasts for INFY returns")
accuracy(arma.preds, infy_ret_test)[2]



#data$day <- as.factor(weekdays(as.Date(data$Date, "%d-%b-%y")))
#days <- data$day[2:nrow(data)]
#days
#xreg1 <- model.matrix(~as.factor(days))[, 2:5]
#colnames(xreg1) <- c("Monday", "Thursday", "Tuesday", "Wednesday")

#xreg1
#data$day
#View(data)
#library(fpp2)
#fit2 <- arima(infy_ret_train, order = c(2, 0, 2), xreg=xreg1[c(1:( 0.9* length(infy_ret))), ])
#fit1.preds <- forecast(fit2, h = 20, xreg=xreg1[c(1019:1132), ])
#fit1.preds <- predict(fit2, n.ahead = 25, xreg=xreg1[c(223:247), ])
#plot(forecast(fit2, h = 20, xreg1[c(223:247), ]), main = "ARIMAX forecasts of INFY returns")
