library(tidyverse)

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













