library(tidyverse)
library(lubridate)
library(tseries)
library(astsa)
library(imputeTS)
library(forecast)
library(magrittr)


#### Daten einlesen ####

# Engelberg
engelberg <- read.csv("Daten/Engelberg_data.csv",sep=';', na.strings=c('-',''))

engelberg$time <- as.Date(as.character(engelberg$time), format = "%Y%m%d")
engelberg_clean <- engelberg %>% select('stn',
                                        'time',
                                        'tre200nn')

# NA anschauen
str(engelberg_clean)
summary(engelberg_clean)
# hat 654 NA's

#seeboden
seeboden <- read.csv("Daten/Seebodenalp_data.csv",sep=';', na.strings=c('-',''))
# Convert integer dates to Date objects
seeboden$time <- as.Date(as.character(seeboden$time), format = "%Y%m%d")
seeboden_clean <- seeboden %>% select('stn',
                                            'time',
                                            'tre200nn')

# NA anschauen
str(seeboden_clean)
summary(seeboden_clean)
# hat 119 NA's

#### EDA ####
# Histogram
ggplot(engelberg_clean, aes(x = tre200nn)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Engelberg tre200nn", x = "tre200nn", y = "Count")

ggplot(seeboden_clean, aes(x = tre200nn)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  labs(title = "Distribution of Seebodenalp tre200nn", x = "tre200nn", y = "Count")

# Boxplot
ggplot(engelberg_clean, aes(y = tre200nn)) +
  geom_boxplot(fill = "blue", color = "darkred") +
  labs(title = "Boxplot of Engelberg tre200nn", y = "tre200nn")

ggplot(seeboden_clean, aes(y = tre200nn)) +
  geom_boxplot(fill = "orange", color = "darkred") +
  labs(title = "Boxplot of Seebodenalp tre200nn", y = "tre200nn")

# Timeseries
ggplot(engelberg_clean, aes(x = time, y = tre200nn)) +
  geom_line() +
  labs(title = "Time Series of Engelberg tre200nn", x = "Time", y = "tre200nn")

ggplot(seeboden_clean, aes(x = time, y = tre200nn)) +
  geom_line() +
  labs(title = "Time Series of Seebodenalp tre200nn", x = "Time", y = "tre200nn")


#### Time Series Lecture ####

# Use the time series class of the library stats
freq_daily <- 365.2422


# Similar procedure as above

stdt <- as.Date('01.01.1990', '%d.%m.%Y')
temp_ts <- ts(engelberg_clean$tre200nn,start=c(year(min(engelberg_clean$time)),
                                               yday(min(engelberg_clean$time))),
              frequency=freq_daily) %>%
  na_replace(fill=0)
plot(temp_ts)

# Manual trend detection
engelberg_clean %<>% mutate(year=year(engelberg_clean$time)+yday(engelberg_clean$time)/freq_daily)
engelberg_clean_trend <- lm(engelberg_clean$tre200nn~engelberg_clean$year)
plot(temp_ts)
abline(engelberg_clean_trend, col = 'red')

temp_comp=decompose(temp_ts)

plot(temp_comp)

adf.test(temp_ts)
#Stationary
acf(temp_comp$random,na.action=na.pass)
pacf(temp_comp$random,na.action=na.pass)
pacf(temp_ts)

# Compare graphically
plot(temp_comp$random)

# Compare mathematically
window <- list(start=1990,end=2023)
# Make the time series comparable
temp_comp_random <- data.frame(time=time(temp_comp$random), Random=temp_comp$random)
# Make the time series comparable
temp_comp_random <- data.frame(time=time(temp_comp$random), Random=temp_comp$random)
#
temp_comp_random %<>% filter(time>=window$start&time<window$end)
#
temp_comp_random_ts <- ts(temp_comp_random$Random, start=c(window$start,1), frequency=freq_daily)
plot(temp_comp_random_ts)
# Cross correlation function
#ccf(temp_comp_random_ts)

# Ex: What do you find with the total snow height?

# Autocorrelation and ARIMA model
arima(temp_comp_random_ts, c(2,0,0))
sd(temp_comp_random_ts)
plot(ts(arima.sim(n=round((window$end-window$start)*freq_daily),
                  model=list(ar=c(0.89,-0.18), ma=c(), sd=3.4)), 
        start=c(window$start,1), 
        frequency=freq_daily))
plot(temp_comp_random_ts)

# Demo 2: Stochastic process "bootstrap"

# Stochastic process 1: AR(2)
ts1 <- ts(arima.sim(n=100,model=list(ar=c(0.49,-0.5), ma=c(), sd=1)), start=c(2015,1), 
          frequency=4)

plot(ts1)
