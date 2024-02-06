library(tidyverse)

# Read a CSV file into a data frame
engelberg <- read.csv("Daten/Engelberg_data.csv",sep=';', na.strings=c('-',''))

engelberg$time <- as.Date(as.character(engelberg$time), format = "%Y%m%d")


# View the first few rows
schwyz <- read.csv("Daten/Schwyz_data.csv",sep=';', na.strings=c('-',''))
