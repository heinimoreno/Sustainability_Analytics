#### load libraries ####
library(tidyverse) # for data wrangling
library(lubridate) # date manipulation
library(TSstudio) # time series interactive viz
library(tseries) # for adf.test
library(astsa)
library(imputeTS)
library(forecast)
library(magrittr)

#### Clean Files ####

# Engelberg einlesen
kuessnacht <- read.csv("Daten/Seebodenalp_data.csv",sep=';', na.strings=c('-',''))

kuessnacht$time <- as.Date(as.character(kuessnacht$time), format = "%Y%m%d")
kuessnacht_clean <- kuessnacht %>% select('time',
                                        'tre200nn') %>% 
  rename('temp' = 'tre200nn') %>%
  filter(year(time) > 1989)

# NA anschauen
str(kuessnacht_clean)
summary(kuessnacht_clean)
glimpse(kuessnacht_clean)
anyNA(kuessnacht_clean)

kuessnacht_clean <- na.omit(kuessnacht_clean[!is.na(kuessnacht_clean$temp), ])

#### > Winter ####

kuessnacht_filtered <- kuessnacht_clean %>%
  # Assuming 'time' is already a Date object. If not, convert it first with as.Date()
  filter(year(time) > 1989) %>%
  filter(month(time) %in% c(12, 1, 2, 3)) 

#### >>Monthly below 0 ####

# below 0 per month
monthly_below_zero <- kuessnacht_filtered %>%
  filter(temp < 0) %>%
  mutate(year = year(time),
         month = month(time)) %>%
  group_by(year, month) %>%
  summarise(days_below_zero = n(), .groups = "drop") %>% # Drop the grouping
  mutate(year_month = paste(year, month, sep="-")) %>% # Create year-month column
  select(year_month, days_below_zero) # Select only the columns you want

monthly_below_zero_k <- monthly_below_zero %>%
  mutate(year_month = as.Date(paste(year_month, "01", sep="-")))

# If you want to see the result
glimpse(monthly_below_zero_k)
range(monthly_below_zero_k$year_month)

ts_month_k <- ts(data = monthly_below_zero_k$days_below_zero,
               start = c(1990,01),
               frequency = 4)
autoplot(ts_month_k)

# Decompose
ts_month_k_dc <- decompose(ts_month_k)
plot(ts_month_k_dc)

# Stationary?
adf.test(ts_month_k)
## it is stationary

acf(ts_month_k)
acf(ts_month_k_dc$random,na.action=na.pass)
pacf(ts_month_k_dc$random,na.action=na.pass)

# Arima Model 
month_auto_k <- auto.arima(ts_month_k, D=1, d=1)
month_auto_k.1 <- auto.arima(ts_month_k)
month_auto_k
month_auto_k.1

# Forecasting
f <- forecast(month_auto_k, level=c(95), h=5*4)

#last_year <- as.numeric(format(time(ts_month_k)[length(ts_month_k)], "%Y"))


# Plotting the forecast
plot(f, main = "Forecast for the Next 5 Winter Months", 
     xlab = "Time", ylab = "Forecast")


