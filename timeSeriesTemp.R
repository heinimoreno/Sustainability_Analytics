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

#### Clean Files ####

#homogenisierte Daten
Engelberg_homogenisiert <- read.csv(paste0(data_folder, "Messreihe_Engelberg.csv"))
temp_homo_ts <- ts(summary_data$Avg_Temperature, start = c(1864, 1), frequency = 12)

# Engelberg einlesen
engelberg <- read.csv("Daten/Engelberg_data.csv",sep=';', na.strings=c('-',''))

engelberg$time <- as.Date(as.character(engelberg$time), format = "%Y%m%d")
engelberg_clean <- engelberg %>% select('time',
                                        'tre200nn') %>% 
  rename('temp' = 'tre200nn') %>%
  filter(year(time) > 1989)

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

# Wie lange
range(engelberg_clean$time)
interval(start = head(engelberg_clean$date)[1], end = tail(engelberg_clean$date)[1])

#### Time Series ####
#### > Year ####
engelberg_clean <- engelberg_clean %>%
  filter(year(time) > 1989) %>%
  na_replace(fill=0)

ts_engelberg <- ts(data = engelberg_clean$temp,
               start = c(1990,01,01),
               frequency = 365)

# Trend Visualisierung
# > Tägliche Daten von 1990-2023
engelberg_clean_trend <- lm(engelberg_clean$temp~engelberg_clean$time)

plot(ts_engelberg)
abline(engelberg_clean_trend, col = 'red')

# >> Monatliche Daten von 1990-2023 mit Trendlinie von Homogen.
temp_yr <- engelberg_clean %>%
  mutate(temp_raw=replace_na(temp,0)) %>%
  group_by(Year=year(time)) %>%
  filter(Year>=1990 & Year<=2023) %>%
  summarize(temp_yr=mean(temp_raw)) %>%
  ungroup()
temp_mn <- engelberg_clean %>%
  mutate(temp_raw=replace_na(temp,0)) %>%
  group_by(Year=year(time), Month=month(time)) %>%
  filter(Year>=1990 & Year<=2023) %>%
  summarize(temp_mn=mean(temp_raw), .groups='keep') %>%
  ungroup()

temp_mn_ts <- ts(temp_mn$temp_mn, start=c(temp_mn$Year[1],temp_mn$Month[1]), frequency=12)
plot(temp_mn_ts)

# Visualisierung des Trendes
fresh_snow_trend <- lm(temp_mn$temp_mn~temp_mn$Year)
engelberg_homog_trend <- lm(Engelberg_homogenisiert$Temperature~Engelberg_homogenisiert$Year)
plot(temp_mn_ts, xlab='Year', ylab='Average Temp.')
abline(fresh_snow_trend, col = 'red')
abline(engelberg_homog_trend, col = 'blue')

# Yearly Data decomposing
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
range(monthly_below_zero$year_month)

ts_month <- ts(data = monthly_below_zero$days_below_zero,
                   start = c(1990,01),
                   frequency = 4)
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

# Forecasting
f <- forecast(month_auto, level=c(95), h=5*4)


# Plotting the forecast
plot(f, main = "Forecast for the Next 5 Winter Months", 
     xlab = "Time", ylab = "Forecast")

# Assuming 'f' is a forecast object that has a 'mean' component
y_lower <- min(-10, min(f$mean))
y_upper <- 60

plot(f, main = "Forecast for the Next 5 Winter Months", 
     xlab = "Time", ylab = "Forecast",
     ylim = c(y_lower, y_upper))

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

two_engelberg <- engelberg_filtered %>% 
  filter(year(time) == 2001)

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

winter_model_dc <- arima(temp_comp_random_ts, c(1,0,0))
summary(winter_model_dc)

winter_model <- arima(ts_one, c(2,0,0))

# Forecasting
f <- forecast(winter_model, level=c(95), h=length(two_engelberg$temp))

# Plotting the forecast
plot(f)

# Adding actual data from 'two_engelberg' to the plot
# Ensure 'two_engelberg$value' is the correct reference for your actual data
lines(seq(length(ts_one) + 1, by = 1, 
          length.out = length(two_engelberg$temp)), 
      two_engelberg$temp, col = "blue")

#### Nachfolgende Tage unter 0

#### ca. lm von observed und trend














