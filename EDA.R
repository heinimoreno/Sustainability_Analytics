library(tidyverse)

# Engelberg
engelberg <- read.csv("Daten/Engelberg_data.csv",sep=';', na.strings=c('-',''))

engelberg$time <- as.Date(as.character(engelberg$time), format = "%Y%m%d")
engelberg_clean <- engelberg %>% select('stn',
                                        'time',
                                        'tre200nn')

# Schwyz
schwyz <- read.csv("Daten/Schwyz_data.csv",sep=';', na.strings=c('-',''))
schwyz$time <- as.Date(as.character(schwyz$time), format = "%Y%m%d")
schwyz_clean <- schwyz %>% select('stn',
                                 'time',
                                 'tre200nn')

