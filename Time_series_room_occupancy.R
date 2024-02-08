library(tseries)
library(astsa)
library(imputeTS)
library(forecast)
library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)

# Import Datasets
EB <- read.csv("~/HSLU/3. Semester/Sustainability/Room_Occupancy_Egelberg.csv")
View(EB)
KS <- read.csv("~/HSLU/3. Semester/Sustainability/Room_Occupancy_Kuessnacht SZ.csv")
View(KS)

# Create time series
freq_monthly <- 12
Occupancy_Engelberg<-
  ts(EB$Room.occupancy, start=c(year(min(EB$Date)),yday(min(EB$Date))), frequency=freq_monthly) %>%
  na_replace(fill=0)
Occupancy_Kuessnacht<-
  ts(KS$Room.occupancy, start=c(year(min(KS$Date)),yday(min(KS$Date))), frequency=freq_monthly) %>%
  na_replace(fill=0)
plot(Occupancy_Engelberg)
plot(Occupancy_Kuessnacht)


# Sationarity test and decomposition
adf.test(Occupancy_Engelberg,k=0)
adf.test(Occupancy_Engelberg,k=20)

Occupency_Engelberg_comp=decompose(Occupancy_Engelberg)
Occupency_Kuessnacht_comp=decompose(Occupancy_Kuessnacht)
plot(Occupency_Engelberg_comp)
plot(Occupency_Kuessnacht_comp)

# ACF/PACF
adf.test(Occupancy_Engelberg,k=3)
acf(Occupency_Engelberg_comp$random,na.action=na.pass)
pacf(Occupency_Engelberg_comp$random,na.action=na.pass)
pacf(Occupancy_Engelberg)

adf.test(Occupancy_Kuessnacht,k=3)
acf(Occupency_Kuessnacht_comp$random,na.action=na.pass)
pacf(Occupency_Kuessnacht_comp$random,na.action=na.pass)
pacf(Occupancy_Kuessnacht)

# Compare graphically
plot(Occupency_Engelberg_comp$random)
plot(Occupency_Kuessnacht_comp$random)

# Cross correlation function
ccf(na.omit(Occupency_Engelberg_comp$random), na.omit(Occupency_Kuessnacht_comp$random))

# Delete all months except winter
df_filtered <- df[format(df$Date, "%m") %in% c("12", "01", "02", "03"), ]
View(df_filtered)
df2_filtered <- df2[format(df2$Date, "%m") %in% c("12", "01", "02", "03"), ]
View(df2_filtered)

# Taking average of ski season
x = c(1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11)
length(x)
Year <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

df_filtered$num <- x
df2_filtered$num <- x

years <- df_filtered %>%
  group_by(num) %>%
  summarise(average_room_occupancy = mean(as.numeric(Room.occupancy)))
df_EB_year <- data.frame(Year, years)
df_EB_year <- df_EB_year %>%
  select(-num)
View(df_EB_year)

years2 <- df2_filtered %>%
  group_by(num) %>%
  summarise(average_room_occupancy = mean(as.numeric(Room.occupancy)))
df_KS_year <- data.frame(Year, years2)
df_KS_year <- df_KS_year %>%
  select(-num)
View(df_EB_year)
View(df_KS_year)

# Creating time series of monthly seasonal data
freq_monthly <- 4
Occupancy_Engelberg_season<-
  ts(as.numeric(df_filtered$Room.occupancy), frequency=freq_monthly) %>%
  na_replace(fill=0)
Occupancy_Kuessnacht_season<-
  ts(as.numeric(df2_filtered$Room.occupancy), frequency=freq_monthly) %>%
  na_replace(fill=0)
plot(Occupancy_Engelberg_season)
plot(Occupancy_Kuessnacht_season)

# decomposition
Occupency_Engelberg_season_comp=decompose(Occupancy_Engelberg_season)
Occupency_Kuessnacht_season_comp=decompose(Occupancy_Kuessnacht_season)
plot(Occupency_Engelberg_season_comp)
plot(Occupency_Kuessnacht_season_comp)

# Creating time series for yearly data
freq_monthly <- 1
Occupancy_Engelberg_season_year<-
  ts(as.numeric(df_EB_year$average_room_occupancy)) %>%
  na_replace(fill=0)
Occupancy_Kuessnacht_season_year<-
  ts(as.numeric(df_KS_year$average_room_occupancy)) %>%
  na_replace(fill=0)
plot(Occupancy_Engelberg_season_year)
plot(Occupancy_Kuessnacht_season_year)

# decomposition
Occupency_Engelberg_season_year_comp=decompose(Occupancy_Engelberg_season_year)
Occupency_Kuessnacht_season_year_comp=decompose(Occupancy_Kuessnacht_season_year)
plot(Occupency_Engelberg_comp)
plot(Occupency_Kuessnacht_comp)

#########################################################################################3
# Time series temperature
df_temp_EN <- read.csv("~/HSLU/3. Semester/Sustainability/Engelberg_monthly_below_zero.csv")

df_temp_EN <- slice(df_temp_EN, -(1:92))

df_filtered <- slice(df_filtered, -42)
freq_monthly <- 4
Occupancy_Engelberg_season<-
  ts(as.numeric(df_filtered$Room.occupancy), frequency=freq_monthly) %>%
  na_replace(fill=0)
temp_Engelberg <- 
  ts(as.numeric(df_temp_EN$days_below_zero), frequency=freq_monthly) %>%
  na_replace(fill=0)

plot(Occupancy_Engelberg_season)
plot(temp_Engelberg)

Occupancy_Engelberg_season_comp <- decompose(Occupancy_Engelberg_season)
temp_Engelberg_comp <- decompose(temp_Engelberg)
plot(Occupancy_Engelberg_season_comp)
plot(temp_Engelberg_comp)

ccf(temp_Engelberg_comp$random, Occupancy_Engelberg_season_comp$random, na.action = na.pass)

ccf(Occupancy_Engelberg_season, temp_Engelberg)
