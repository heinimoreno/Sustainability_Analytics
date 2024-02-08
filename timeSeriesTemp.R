# Only Time Series
#### load libraries ####
library(tidyverse) # for data wrangling
library(lubridate) # date manipulation
library(TSstudio) # time series interactive viz
library(forecast) # time series library
library(tseries) # for adf.test
# Clean Files

# Engelberg
engelberg <- read.csv("Daten/Engelberg_data.csv",sep=';', na.strings=c('-',''))

engelberg$time <- as.Date(as.character(engelberg$time), format = "%Y%m%d")
engelberg_clean <- engelberg %>% select(
                                        'time',
                                        'tre200nn') %>% 
  rename('temp' = 'tre200nn') %>%
  na_replace(fill=0)

# NA anschauen
str(engelberg_clean)
summary(engelberg_clean)
glimpse(engelberg_clean)
anyNA(engelberg_clean)
# hat 0 NA's

#Zeitreihe
range(engelberg_clean$time)
interval(start = head(engelberg_clean$date)[1], end = tail(engelberg_clean$date)[1])

#### Time Series ####

# create TS-object
ts_engelberg <- ts(data = engelberg_clean$temp,
               start = c(1990,01),
               frequency = 365)
autoplot(ts_engelberg)

# Decompose
ts_engelberg_dc <- decompose(ts_engelberg)
plot(ts_engelberg_dc)

# Stationary?
adf.test(ts_engelberg)
## it is stationary

acf(ts_engelberg)
acf(ts_engelberg_dc$random,na.action=na.pass)
pacf(ts_engelberg_dc$random,na.action=na.pass)


#### > Arima Model ####
engelberg_auto <- auto.arima(ts_engelberg, seasonal = T)
engelberg_auto




