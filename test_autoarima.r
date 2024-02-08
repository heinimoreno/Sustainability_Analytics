#### Laden der Bibliotheken ####
library(tidyverse) # für Datenmanipulation
library(lubridate) # Datummanipulation
library(TSstudio) # interaktive Visualisierung von Zeitreihen
library(forecast) # Zeitreihenbibliothek
library(tseries) # für adf.test

#### Daten bereinigen ####

# Engelberg-Daten laden
engelberg <- read.csv("Daten/Engelberg_data.csv", sep=';', na.strings=c('-',''))

# Datumsformat anpassen
engelberg$time <- as.Date(as.character(engelberg$time), format = "%Y%m%d")
engelberg_clean <- engelberg %>% 
  select(time, tre200nn) %>% 
  rename(temp = tre200nn) %>%
  na_replace(fill=0) # NAs ersetzen

#### Zeitreihe erstellen ####

# TS-Objekt erstellen
ts_engelberg <- ts(data = engelberg_clean$temp,
                   start = c(1990, 01), # Startjahr und -monat anpassen
                   frequency = 365) # oder entsprechende Frequenz für Daten

# Zeitreihe visualisieren
autoplot(ts_engelberg)

# Überprüfung auf Stationarität
adf.test(ts_engelberg)
# Falls nicht stationär, Differenzierung durchführen

#### ARIMA-Modellierung ####

# Bestimmung der ARIMA-Parameter automatisch
arima_model <- auto.arima(ts_engelberg, trace = TRUE, ic = "aic")
summary(arima_model)

# Modell-Diagnostik
checkresiduals(arima_model)

# Vorhersage
forecast_length <- 30 # Anzahl der Vorhersagetage
forecast_arima <- forecast(arima_model, h = forecast_length)
autoplot(forecast_arima) + labs(title = "ARIMA-Modell Vorhersage für Engelberg Temperatur", y = "Temperatur")

