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
temp <-
  ts(engelberg_clean$tre200nn, start=c(year(min(engelberg_clean$time)),
                                       yday(min(engelberg_clean$time))),
     frequency=freq_daily) %>%
  na_replace(fill=0)

plot(temp)

adf.test(temp,k=0)

temp_comp=decompose(temp)
plot(temp_comp)


# Similar procedure as above

stdt <- as.Date('01.01.1980', '%d.%m.%Y')
temp_ts <- ts(engelberg_clean$tre200nn,start=c(year(min(engelberg_clean$time)),
                                               yday(min(engelberg_clean$time))),
              frequency=freq_daily) %>%
  na_replace(fill=0)
plot(temp_ts)
temp_comp=decompose(temp_ts)

plot(temp_comp)

adf.test(temp_ts,k=3)
acf(temp_comp$random,na.action=na.pass)
pacf(temp_comp$random,na.action=na.pass)
pacf(temp_ts)

# Compare graphically
plot(temp_comp$random)

# Compare mathematically
window <- list(start=1980,end=2023)
# Make the time series comparable
temp_comp_random <- data.frame(time=time(temp_comp$random), Random=temp_comp$random)
# Make the time series comparable
temp_comp_random <- data.frame(time=time(temp_comp$random), Random=temp_comp$random)
#
temp_comp_random %<>% filter(time>=window$start&time<window$end)
#
temp_comp_random_ts <- ts(temp_comp_random$Random, start=c(window$start,1), frequency=freq_daily)

# Cross correlation function
#ccf(temp_comp_random_ts,fresh_snow_comp_random_ts)

# Ex: What do you find with the total snow height?

# Autocorrelation and ARIMA model
arima(temp_comp_random_ts, c(2,0,0))
sd(temp_comp_random_ts)
plot(ts(arima.sim(n=round((window$end-window$start)*freq_daily),model=list(ar=c(0.89,-0.18), ma=c(), sd=3.4)), start=c(window$start,1), frequency=freq_daily))
plot(temp_comp_random_ts)

# Ex: Which conclusions do you take from this?

# Ex: Explore temperature and precipitation time series, homogenized, Luzern
#

#
# Demo 2: Stochastic process "bootstrap"
#
# Stochastic process 1: AR(2)
ts1 <- ts(arima.sim(n=80,model=list(ar=c(0.49,-0.5), ma=c(), sd=1)), start=c(2020,1), frequency=4)

plot(ts1)

# Stochastic process 2: AR(2)
# White noise process
w <- rnorm(80, mean=0, sd=1)
# Pulse response
w <- c(1, rep(0,79))
# AR parameters
ar <- c(0,1)
# Simulation
t <- c(w[1],w[2]+ar[1]*w[1])
for (i in 3:80) {
  t <- c(t,w[i]+t[i-1]*ar[1]+t[i-2]*ar[2])
}
ts2 <- ts(t, start=c(2020,1), frequency=4)

# Test the simulated time series and determine the stochastic process
adf.test(ts1,k=2)
acf(ts1)
pacf(ts1)
a=arima(ts1, c(2,0,0))


# Stochastic process 3: Poisson
Poisson <- function(k,l){return(l^k*exp(-l)/factorial(k))}
Poisson(1,2)
dpois(1,2)
rpois(20,0.05)

# Homogeneous / non-homogeneous
h <- rpois(1000,10)
mean(h)
var(h)
m <- 1+0.5*sin(2*pi*1:1000/1000)
nh <- rpois(1000,10*m)
mean(nh)
var(nh)

# Binomial and negative binomial
b <- rbinom(1000,100,0.1)
mean(b)
var(b)
nb <- rnbinom(1000,12,mu=10)
mean(nb)
var(nb)





