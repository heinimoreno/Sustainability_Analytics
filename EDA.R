library(tidyverse)

#### Engelberg ####
engelberg <- read.csv("Daten/Engelberg_data.csv",sep=';', na.strings=c('-',''))

engelberg$time <- as.Date(as.character(engelberg$time), format = "%Y%m%d")
engelberg_clean <- engelberg %>% select('stn',
                                        'time',
                                        'tre200nn')

# NA anschauen
str(engelberg_clean)
summary(engelberg_clean)


#### Rigi ####
seeboden <- read.csv("Daten/Seebodenalp_data.csv",sep=';', na.strings=c('-',''))
# Convert integer dates to Date objects
seeboden$time <- as.Date(as.character(seeboden$time), format = "%Y%m%d")
seeboden_clean <- seeboden %>% select('stn',
                                            'time',
                                            'tre200nn')

# NA anschauen
str(seeboden_clean)
summary(seeboden_clean)


# EDA
ggplot(engelberg_clean) + 
  geom_histogram(mapping = aes(x = tre200nn), binwidth = 0.5)










