# ------------------------------------------------------------------------------
# -------------------------LOADING REQUIRED PACKAGES----------------------------
# ------------------------------------------------------------------------------
#install.packages("vars")
#install.packages("tseries")
#install.packages("quantmod")
#install.packages("xts")
#install.packages("ggplot2")
#install.packages("tseries")
#install.packages("forecast")
#install.packages("vars")
#install.packages("PerformanceAnalytics")
#install.packages("lmtest")
#install.packages("leaps")
#install.packages("fBasics")
#install.packages("tidyverse")
#install.packages("zoo")

library(quantmod)
library(xts)
library(ggplot2)
library(tseries)
library(forecast)
library(vars)
library(PerformanceAnalytics)
library(lmtest)
library(leaps)
library(fBasics)
library(tidyverse)
library(zoo)



# ------------------------------------------------------------------------------
# ---------------------------------DATA-PREP------------------------------------
# ------------------------------------------------------------------------------
## Loading the LHA Data via Yahoo Finance API
data <- NULL
tickers_index <- c("LHA.DE") # Ticker from Yahoo Finance for the SMI

for (Ticker in tickers_index){
  data <- cbind(data,
                getSymbols.yahoo(Ticker, from="2010-01-01", to="2022-07-02", periodicity = "daily", auto.assign=FALSE)[,6])
}

LHA <- data
colnames(LHA) <- c("LHA")
LHA<- na.omit(LHA)

## Loading the OIL/LIT Data via Yahoo Finance API
data2 <- NULL
tickers_index2 <- c("CL=F")

for (Ticker in tickers_index2){
  data2 <- cbind(data2,
                 getSymbols.yahoo(Ticker, from="2010-01-01", to="2022-07-02", periodicity = "daily",
                                  auto.assign=FALSE)[,6])
}

OIL <- data2
colnames(OIL)<-c("OIL")
OIL<- na.omit(OIL)

## Loading the MSCI Europe Data via Yahoo Finance API
data3 <- NULL
tickers_index3 <- c("IEUR")

for (Ticker in tickers_index3){
  data3 <- cbind(data3,
                 getSymbols.yahoo(Ticker, from="2010-01-01", to="2022-07-02", periodicity = "daily",
                                  auto.assign=FALSE)[,6])
}

MSCI <- data3
colnames(MSCI)<-c("MSCI")
MSCI<- na.omit(MSCI)

## Loading the EXR Data via Yahoo Finance API
data4 <- NULL
tickers_index4 <- c("EURUSD=X", "EURJPY=X", "EURCNY=X" )

for (Ticker in tickers_index4){
  data4 <- cbind(data4,
                 getSymbols.yahoo(Ticker, from="2010-01-01", to="2022-07-02", periodicity = "daily",
                                  auto.assign=FALSE)[,6])
}

EXR <- data4
colnames(EXR)<-c("EURUSD", "EURJPY", "EURCNY")
EXR<- na.omit(EXR)

## Loading the GDP Data of Germany via FRED API
GDP <- getSymbols("CPMNACNSAB1GQDE", periodicity = "monthly", auto.assign=FALSE, src = "FRED")
colnames(GDP) <- "GDP_D"
GDP <- GDP[index(GDP) >= as.Date("2010-01-01") & index(GDP) <= as.Date("2022-07-02")]


#Creating Time Series
lha <- ts(LHA[-1], start = 2010, end = c(2022,9), frequency = 12)
oil <- ts(OIL[-1], start = 2010, end = c(2022,9), frequency = 12)
gdp <- ts(GDP[-1], start = 2010, end = c(2022,9), frequency = 12)
msci <- ts(MSCI[-1], start = 2010, end = c(2022,9), frequency = 12)
eurusd <- ts(EXR$EURUSD[-1], start = 2010, end = c(2022,9), frequency = 12)
eurjpy <- ts(EXR$EURJPY[-1], start = 2010, end = c(2022,9), frequency = 12)
eurcny <- ts(EXR$EURCNY[-1], start = 2010, end = c(2022,9), frequency = 12)



# ------------------------------------------------------------------------------
# -------------------------------STATIONARITY-----------------------------------
# ------------------------------------------------------------------------------
# Testing for stationarity and creating stationary series
adf.test(LHA)
adf.test(OIL)
adf.test(GDP)
adf.test(MSCI)
adf.test(EXR$EURUSD)
adf.test(EXR$EURJPY)
adf.test(EXR$EURCNY)

# Calculating log returns
LHA_r <- na.omit(diff(log(LHA)))
OIL_r <- na.omit(diff(log(OIL)))
GDP_r <- na.omit(diff(log(GDP)))
MSCI_r <- na.omit(diff(log(MSCI)))
EXR_r <- na.omit(diff(log(EXR)))
EXREU_r <- na.omit(diff(log(EXR$EURUSD)))
EXREJ_r <- na.omit(diff(log(EXR$EURJPY)))
EXREC_r <- na.omit(diff(log(EXR$EURCNY)))

adf.test(LHA_r)
adf.test(OIL_r)
adf.test(MSCI_r)
adf.test(EXREU_r)
adf.test(EXREJ_r)
adf.test(EXREC_r)

# ------------------------------------------------------------------------------
# ----------------------------DESCRIPTIVE-STATISTICS----------------------------
# ------------------------------------------------------------------------------

## Time Series Plot LHA
plot(zoo(LHA), type = "l", main = "Stock Price Development Lufthansa",ylab="Share Price", xlab="Time")
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

## Time Series Plot OIL
plot(zoo(OIL), type = "l", main = "Prices",ylab="OIL", xlab="")
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

## Time Series Plot MSCI
plot(zoo(MSCI), type = "l", main = "MSCI",ylab="Price", xlab="")
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

## Time Series Plot EXR
plot(zoo(EXR), type = "l", main = "Exchange Rates",ylab=colnames(EXR), xlab="")
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

## Time Series Plot GDP
plot(zoo(GDP), type = "l", main = "GDP",ylab="in EUR bn", xlab="")
grid(col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)

#Decomposition (Seasonality)
lha_dec <- decompose(lha)
lha <- lha_dec$x - lha_dec$seasonal
plot(lha_dec, xlab="Year LHA")

oil_dec <- decompose(oil)
oil <- oil_dec$x - oil_dec$seasonal
plot(oil_dec, xlab="Year OIL")

eurusd_dec <- decompose(eurusd)
eurusd <- eurusd_dec$x - eurusd_dec$seasonal
plot(eurusd_dec, xlab="Year EURUSD")

eurjpy_dec <- decompose(eurjpy)
eurusd <- eurusd_dec$x - eurjpy_dec$seasonal
plot(eurjpy_dec, xlab="Year EURJPY")

eurcny_dec <- decompose(eurcny)
eurcny <- eurcny_dec$x - eurcny_dec$seasonal
plot(eurcny_dec, xlab="Year EURCNY")

gdp_dec <- decompose(gdp)
gdp <- gdp_dec$x - gdp_dec$seasonal
plot(gdp_dec, xlab="Year GDP")

## Histogram
hist(LHA_r, breaks = 25, main = 'Histogram Continuous Returns', xlab = 'LHA Returns')
basicStats(LHA_r)

## ACF & PACF
par(mfrow=c(1,2))
acf(LHA_r, main = 'ACF LHA Returns')
pacf(LHA_r, main='PACF LHA Returns')

# ------------------------------------------------------------------------------
# ----------------------------MODELING------------------------------------------
# ------------------------------------------------------------------------------
# Regression model
dataframe <- data.frame(as.numeric(lha), as.numeric(oil), as.numeric(gdp), as.numeric(msci), as.numeric(eurusd), as.numeric(eurjpy), as.numeric(eurcny))
mreg <- lm(dataframe$as.numeric.lha.~ dataframe$as.numeric.oil. + dataframe$as.numeric.gdp. + dataframe$as.numeric.msci. + dataframe$as.numeric.eurusd. + dataframe$as.numeric.eurjpy. + dataframe$as.numeric.eurcny.)
summary(mreg)

# Backward selection
reg <- regsubsets(as.numeric.lha. ~ ., data = dataframe, method = "backward", nvmax = 4)
reg.sum <- summary(reg)
reg.sum$which

dataframe2 <- data.frame(as.numeric(lha), as.numeric(oil), as.numeric(msci), as.numeric(eurcny))
mreg2 <- lm(dataframe2$as.numeric.lha.~ dataframe2$as.numeric.oil. + dataframe2$as.numeric.msci.+ dataframe2$as.numeric.eurcny.)
summary(mreg2)

# ------------------------------------------------------------------------------
# ----------------------------ARIMA---------------------------------------------
# ------------------------------------------------------------------------------
# Downloading levels of LHA via Yahoo Finance API
data <- NULL
tickers_index <- c("LHA.DE") # Ticker from Yahoo Finance for the SMI

for (Ticker in tickers_index){
  data <- cbind(data,
                getSymbols.yahoo(Ticker, from="2010-01-01", periodicity = "daily", auto.assign=FALSE)[,6])
}

LHA <- data
colnames(LHA) <- c("LHA")


# Plotting LHA in levels and testing for stationarity
plot(LHA, main="LHA")
adf.test(LHA)

LHA_r <- na.omit(diff(log(LHA)))

plot(LHA_r, main="LHA Returns", ylab=NULL)
adf.test(LHA_r) # LHA returns are stationary

# ARIMA AIC
# Identifying the orders p and q of the ARIMA(p,1,q)-model by testing different model specifications
max.order <- 2
d <- 1 

arima_aic <- matrix(NA, ncol=max.order+1, nrow=max.order+1)
row.names(arima_aic) <- c(0:max.order)
colnames(arima_aic) <- c(0:max.order)

# Calculating and storing the AICs for different model specifications
for(i in 0:max.order){
  for(j in 0:max.order){
    arima_aic[i+1,j+1]<-Arima(y=log(LHA), order=c(i,d,j), include.constant =  TRUE)$aic
  }
}
arima_aic

# ARIMA
index <- which(arima_aic == min(arima_aic), arr.ind = TRUE)
ar <- as.numeric(rownames(arima_aic)[index[1]])
ma <- as.numeric(colnames(arima_aic)[index[2]])
ar
ma

# Estimating the optimal ARIMA-model and testing for significance of the coefficients
arima <- Arima(y=LHA, order=c(ar,1,ma), include.constant = TRUE)
coeftest(arima)

# Shortcut! Estimating the orders of the ARIMA(p,d,q) automatically
arima <- auto.arima(LHA, ic="aic")

# Forecasting
pred <- forecast(arima,level=0.95,h=100)
plot(pred, ylab="LHA") + grid()

