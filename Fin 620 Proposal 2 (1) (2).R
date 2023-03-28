# Load the required package
library(quantmod)
library(fBasics)
library(forecast)
library(fBasics)
library(moments)
library(tseries)
library(stats)
library(TSA)

# Define the ticker symbol and the start and end dates
# Downloading stock data for Thermo Fisher Scientific 
ticker <- "TMO"
start_date <- as.Date("1980-01-01")
end_date <- as.Date("2023-03-13")


# Use the getSymbols function to retrieve the data
getSymbols(ticker, src = "yahoo", from = start_date, to = end_date)

# View the data
head(TMO)
tail(TMO)


# Using TMO Adjusted Price for our calculation
tmo.price <- TMO$TMO.Adjusted

# Taking log of price
tmo.log.price <- log(tmo.price)

# Calculating Daily Simple Return
tmo.daily.simple.return <- (diff(tmo.price))
head(tmo.daily.simple.return)

# Calculating Log of Daily simple return
tmo.daily.log.return <- diff(tmo.log.price )
#Defining time series
tmo.daily.log.return <- ts(tmo.daily.log.return, start = c(1980, 1), frequency = 252)


#Converting into dataframe
tmo.daily.log.ret1 <- as.data.frame(tmo.daily.log.return) 
tmo.daily.adjusted.price <- as.data.frame(tmo.price)

#Combining 3 coulmns 
tmo.1 <- cbind(tmo.daily.adjusted.price, tmo.daily.simple.return, tmo.daily.log.ret1)
              tmo.2 <- na.omit(tmo.1)
              rownames(tmo.2) <- NULL #Removing dates from Index column
              colnames(tmo.2) <- c('Adj.Price','Sim.ret', 'Log.ret') #Renaming column name
tmo.3 <- tmo.2
combined=ts(tmo.3)
plot(combined)

# Statistics
basicStats(tmo.3)

# Perform test for mean Price return being zero.
t.test(tmo.price)
t.test(tmo.daily.log.return)

# Perform normality test using the Jaque-Bera method.
jarque.bera.test(tmo.price)
tmo.daily.log.return <- na.omit(tmo.daily.log.return)
jarque.bera.test(tmo.daily.log.return)



# Plotting the graph for TMO daily price 
plot(tmo.price)
# Plotting the graph for TMO daily simple return
plot(tmo.daily.simple.return)
# Plotting the graph for TMO daily log return
plot(tmo.daily.log.return, col = 'red')
 
# plotting acf and pacf for price
acf(tmo.price)
acf(tmo.daily.log.return)
pacf(tmo.price)
pacf(tmo.daily.log.return)
eacf(tmo.price, ar.max = 7, ma.max = 13)
eacf(tmo.daily.log.return, ar.max = 7, ma.max = 13)
adf.test(tmo.price)
adf.test(tmo.daily.log.return)

# ljung box test
log(T) # check what should be m --> m ~ log(T)
Box.test(tmo.price$TMO.Adjusted, lag = 20, type = "Ljung")
# ljung box test
log(T) # check what should be m --> m ~ log(T)
Box.test(tmo.daily.log.ret1$TMO.Adjusted, lag = 20, type = "Ljung")

ts.plot(tmo.price)
ts.plot(diff(tmo.price))


acf(tmo.price)
acf(na.omit(diff(tmo.price)))
acf(na.omit(diff(tmo.price)), lag = 252)


pacf(tmo.price)
pacf(na.omit(diff(tmo.price)))
pacf(na.omit(diff(tmo.price)), lag = 252)


#AR model for Price series
m1=ar(tmo.3$Adj.Price, method="mle")
# Note that the AIC value of the ar command in R
m1$aic
m1$order

# AR model log return series
m4 = ar(tmo.3$Log.ret, method = "mle")
m4
m4$aic
m4$order
m5 = arima(tmo.3$Log.ret, order = c(1,0,1))
m5$aic
sqrt(m5$sigma2)

m6 = arima(tmo.3$Log.ret, order = c(2,0,3))
m6$aic

# Residual Checking
checkresiduals(m6 , lag = 10)
checkresiduals(m5 , lag = 10)

m_auto = auto.arima(tmo.price$TMO.Adjusted)

# Forecasting
tmo_forward = forecast(m_auto,h=20,level=c(90,95))
plot(tmo_forward)



