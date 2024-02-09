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
kuessnacht <- read.csv("Seebodenalp_data.csv",sep=';', na.strings=c('-',''))

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