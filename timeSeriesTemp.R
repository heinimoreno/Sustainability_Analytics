# Only Time Series

#### load libraries ####
library(tidyverse) # for data wrangling
library(lubridate) # date manipulation
library(TSstudio) # time series interactive viz
library(tseries) # for adf.test
library(astsa)
library(imputeTS)
library(forecast)
library(magrittr)
# Clean Files

# Engelberg
engelberg <- read.csv("Daten/Engelberg_data.csv",sep=';', na.strings=c('-',''))

engelberg$time <- as.Date(as.character(engelberg$time), format = "%Y%m%d")
engelberg_clean <- engelberg %>% select('time',
                                        'tre200nn') %>% 
  rename('temp' = 'tre200nn') %>%
  filter(year(time) > 1989)
  ###na_replace(fill=0)

# NA anschauen
str(engelberg_clean)
summary(engelberg_clean)
glimpse(engelberg_clean)
anyNA(engelberg_clean)

# Where are they missing
#Get dates for which temperatures are missing
missingCases <- which(is.na(engelberg_clean$temp)==TRUE)
u <- engelberg_clean$time[missingCases]
max(u)
min(u)
# alle finden in the 1982
# hat 0 NA's

#Zeitreihe
range(engelberg_clean$time)
interval(start = head(engelberg_clean$date)[1], end = tail(engelberg_clean$date)[1])

#### Time Series ####
#### > Year ####
engelberg_clean <- engelberg_clean %>%
  # Assuming 'time' is already a Date object. If not, convert it first with as.Date()
  filter(year(time) > 1989) %>%
  na_replace(fill=0)

ts_engelberg <- ts(data = engelberg_clean$temp,
               start = c(1990,01),
               frequency = 365)

engelberg_clean_trend <- lm(engelberg_clean$temp~engelberg_clean$time)
plot(ts_engelberg)
abline(engelberg_clean_trend, col = 'red')
abline(engelberg_homog_trend, col = 'blue')

#autoplot(ts_engelberg)

# Decompose
ts_engelberg_dc <- decompose(ts_engelberg)
plot(ts_engelberg_dc)

# Stationary?
adf.test(ts_engelberg)
## it is stationary

acf(ts_engelberg)
acf(ts_engelberg_dc$random,na.action=na.pass)
# Plot the PACF
pacf(ts_engelberg_dc$random, na.action = na.pass)

# Plotting
PAutoCorrelation <- pacf(ts_engelberg_dc$random, na.action = na.pass, plot=FALSE)
plot(PAutoCorrelation, main = "Whole Year PACF")

#Arima
# # Fitting to PACF 
window <- list(start=1990,end=2023)

temp_comp_random_y <- data.frame(Date=time(ts_engelberg_dc$random), Random=ts_engelberg_dc$random)
temp_comp_random_y %<>% filter(Date>=window$start&Date<window$end)
temp_comp_random_y_ts <- ts(temp_comp_random_y$Random)
arima(temp_comp_random_y_ts, c(1,0,0))


#### > Winter ####

engelberg_filtered <- engelberg_clean %>%
  # Assuming 'time' is already a Date object. If not, convert it first with as.Date()
  filter(year(time) > 1989) %>%
  filter(month(time) %in% c(12, 1, 2, 3)) 

#### >>Monthly below 0 ####

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

# Arima Model 
month_auto <- auto.arima(ts_month, D=1, d=1)
month_auto.1 <- auto.arima(ts_month)
month_auto
month_auto.1

#### >> daily temp in the winter months ####
# If you want to see the result
glimpse(engelberg_filtered)

ts_daily <- ts(data = engelberg_filtered$temp,
               start = c(1990,01),
               frequency = 365)
autoplot(ts_daily)

# Decompose
ts_daily_dc <- decompose(ts_daily)
plot(ts_daily_dc)

# Stationary?
adf.test(ts_daily)
## it is stationary

acf(ts_daily)
acf(ts_daily_dc$random,na.action=na.pass)
# Plotting
PAutoCorrelation_m <- pacf(ts_daily_dc$random,na.action=na.pass, plot=FALSE)
plot(PAutoCorrelation_m, main = "Winter month PACF")

# Arima Model
# # Fitting to PACF 
window <- list(start=1990,end=2023)

temp_comp_random <- data.frame(Date=time(ts_daily_dc$random), Random=ts_daily_dc$random)
temp_comp_random %<>% filter(Date>=window$start&Date<window$end)
temp_comp_random_ts <- ts(temp_comp_random$Random)

arima(temp_comp_random_ts, c(1,0,0))


# To do
#### Soll PACF vs. PACF Winter
#### > 1 Winter ####
one_engelberg <- engelberg_filtered %>% 
  filter(year(time) == 2000)
         # > 1999 &
         #   year(time) < 2003)

glimpse(one_engelberg)

ts_one <- ts(data = one_engelberg$temp, frequency = 30)
autoplot(ts_one)

# Decompose
ts_one_dc <- decompose(ts_one)
plot(ts_one_dc)

# Stationary?
adf.test(ts_one)

ts_one_diff <- na.omit(diff(ts_one))
adf.test(ts_one_diff)
## it is stationary
ts_one_diff_dc <- decompose(ts_one_diff)


acf(ts_one_diff)
acf(ts_one_diff_dc$random,na.action=na.pass)
# Plotting
PAutoCorrelation_m <- pacf(ts_one_diff_dc$random,na.action=na.pass, plot=FALSE)
plot(PAutoCorrelation_m, main = "1 winter PACF 2000")

# Arima Model
# # Fitting to PACF 
window <- list(start=2000,end=2002)

temp_comp_random_one <- data.frame(Date=time(ts_one_dc$random), Random=ts_one_dc$random)
#temp_comp_random_one %<>% filter(Date>=window$start&Date<window$end)
temp_comp_random_one_ts <- ts(temp_comp_random_one$Random)

arima(temp_comp_random_ts, c(2,0,0))


  #### Nachfolgende Tage unter 0

#### der langfristige Trend vergleichen --> abline
#### Homogenisierte Daten
#### ca. lm von observed und trend 
