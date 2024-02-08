# Only Time Series
#### load libraries ####
library(tidyverse) # for data wrangling
library(lubridate) # date manipulation
library(TSstudio) # time series interactive viz
library(forecast) # time series library
library(tseries) # for adf.test
library(tseries)
library(astsa)
library(imputeTS)
library(forecast)
library(magrittr)
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
# engelberg_auto <- auto.arima(ts_engelberg, D=1, d=1, seasonal = T)
# engelberg_auto

#### Data only winter ####

engelberg_filtered <- engelberg_clean %>%
  # Assuming 'time' is already a Date object. If not, convert it first with as.Date()
  filter(year(time) > 1989) %>%
  filter(month(time) %in% c(12, 1, 2, 3)) 

# below 0 per month
monthly_below_zero <- engelberg_filtered %>%
  filter(temp < 0) %>%
  mutate(year = year(time),
         month = month(time)) %>%
  group_by(year, month) %>%
  summarise(days_below_zero = n(), .groups = "drop") %>% # Drop the grouping
  mutate(year_month = paste(year, month, sep="-")) %>% # Create year-month column
  select(year_month, days_below_zero) # Select only the columns you want

monthly_below_zero <- monthly_below_zero %>%
  mutate(year_month = as.Date(paste(year_month, "01", sep="-")))

# If you want to see the result
glimpse(monthly_below_zero)

ts_month <- ts(data = monthly_below_zero$days_below_zero,
                   start = c(1990,01),
                   frequency = 12)
autoplot(ts_month)

# Decompose
ts_month_dc <- decompose(ts_month)
plot(ts_month_dc)

# Stationary?
adf.test(ts_month)
## it is stationary

acf(ts_month)
acf(ts_month_dc$random,na.action=na.pass)
pacf(ts_month_dc$random,na.action=na.pass)

#### > Arima Model ####
month_auto <- auto.arima(ts_month, D=1, d=1)
month_auto


