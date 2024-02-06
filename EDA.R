library(tidyverse)

# Engelberg
engelberg <- read.csv("Daten/Engelberg_data.csv",sep=';', na.strings=c('-',''))

engelberg$time <- as.Date(as.character(engelberg$time), format = "%Y%m%d")
engelberg_clean <- engelberg %>% select('stn',
                                        'time',
                                        'tre200nn')

# Schwyz
# schwyz <- read.csv("Daten/Schwyz_data.csv",sep=';', na.strings=c('-',''))
# schwyz_clean <- schwyz %>% select('stn',
#                                  'time',
#                                  'tre200nn')
schwyz.1 <- read.csv("Daten/Schwyz_data_1.csv",sep=';', na.strings=c('-',''))
schwyz.1$time <- as.Date(as.character(schwyz.1$time), format = "%Y%m%d")
schwyz.1_clean <- schwyz.1 %>% select('stn',
                                  'time',
                                  'tre200nn')


schwyz.alle <- read.csv("Daten/schwyz_alle_jahre.csv",sep=';', na.strings=c('-',''))
# Convert integer dates to Date objects
schwyz.alle$time <- as.Date(as.character(schwyz.alle$time), format = "%Y%m%d")
schwyz.alle_clean <- schwyz.alle %>% select('stn',
                                      'time',
                                      'tre200nn')

str(engelberg_clean)




