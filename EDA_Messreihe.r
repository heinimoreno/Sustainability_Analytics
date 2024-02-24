# Laden der installierten Pakete
library(ggplot2)
library(dplyr)
library(stats)

# Pfad zum Unterordner
data_folder <- "Daten/"

# Funktion zum Extrahieren des Standortnamens aus dem Dateinamen
get_location <- function(file_name) {
  if (grepl("Engelberg", file_name)) {
    return("Engelberg")
  } else if (grepl("Luzern", file_name)) {
    return("Luzern")
  } else {
    return("Unknown")
  }
}

# Laden der Datensätze für Engelberg und Luzern
Engelberg <- read.csv(paste0(data_folder, "Messreihe_Engelberg.csv"))
Luzern <- read.csv(paste0(data_folder, "Messreihe_Luzern.csv"))

# Extrahieren des Standortnamens und Hinzufügen zur Datensätze
Engelberg$Location <- get_location("Messreihe_Engelberg.csv")
Luzern$Location <- get_location("Messreihe_Luzern.csv")

# Zusammenführen der Daten
combined_data <- rbind(Engelberg, Luzern)

# Umwandlung der Spalten 'Year' und 'Month' in ein Datumsformat
combined_data$Date <- as.Date(paste(combined_data$Year, combined_data$Month, "01", sep = "-"))

# Daten ab 1990 und bis zum Ende von 2023 filtern
combined_data <- combined_data %>%
  filter(Year >= 1990 & Year <= 2023)

# Gruppieren nach Jahr und Standort und Berechnen des Durchschnitts pro Jahr
summary_data <- combined_data %>%
  group_by(Year, Location) %>%
  summarise(Avg_Temperature = mean(Temperature),
            Avg_Precipitation = mean(Precipitation))

# Berechnung der linearen Regression für Engelberg und Luzern
lm_Engelberg <- lm(Avg_Temperature ~ Year, data = summary_data, subset = Location == "Engelberg")
lm_Luzern <- lm(Avg_Temperature ~ Year, data = summary_data, subset = Location == "Luzern")

# Plotten der Temperaturzeitreihe für Engelberg und Luzern mit Trendlinie
ggplot(summary_data, aes(x = Year, y = Avg_Temperature, color = Location)) +
  geom_line() +
  geom_abline(intercept = coef(lm_Engelberg)[1], slope = coef(lm_Engelberg)[2], linetype = "dashed", color = "blue") +
  geom_abline(intercept = coef(lm_Luzern)[1], slope = coef(lm_Luzern)[2], linetype = "dashed", color = "orange") +
  labs(x = "Year", y = "Average Temperature (°C)", color = "Location") +
  ggtitle("Average Temperature Time Series for Engelberg and Luzern with Trend Lines") +
  theme_minimal()

# Berechnung der linearen Regression für Engelberg und Luzern
lm_precip_Engelberg <- lm(Avg_Precipitation ~ Year, data = summary_data, subset = Location == "Engelberg")
lm_precip_Luzern <- lm(Avg_Precipitation ~ Year, data = summary_data, subset = Location == "Luzern")

# Plotten der Niederschlagszeitreihe für Engelberg und Luzern mit Trendlinie
ggplot(summary_data, aes(x = Year, y = Avg_Precipitation, color = Location)) +
  geom_line() +
  geom_abline(intercept = coef(lm_precip_Engelberg)[1], slope = coef(lm_precip_Engelberg)[2], linetype = "dashed", color = "blue") +
  geom_abline(intercept = coef(lm_precip_Luzern)[1], slope = coef(lm_precip_Luzern)[2], linetype = "dashed", color = "orange") +
  labs(x = "Year", y = "Average Precipitation (mm)", color = "Location") +
  ggtitle("Average Precipitation Time Series for Engelberg and Luzern with Trend Lines") +
  theme_minimal()

# Gruppieren nach 5-Jahres-Schritten und Berechnen des Durchschnitts pro Zeitraum
summary_data <- combined_data %>%
  mutate(Year_Group = as.integer((Year - 1) %/% 5) * 5 + 1) %>%
  group_by(Year_Group, Location) %>%
  summarise(Avg_Temperature = mean(Temperature),
            Avg_Precipitation = mean(Precipitation))

# Plotten der Temperaturzeitreihe für Engelberg und Luzern
ggplot(summary_data, aes(x = Year_Group, y = Avg_Temperature, color = Location)) +
  geom_line() +
  labs(x = "5-Year Period", y = "Average Temperature (°C)", color = "Location") +
  ggtitle("Average Temperature Time Series for Engelberg and Luzern") +
  theme_minimal()

# Plotten der Niederschlagszeitreihe für Engelberg und Luzern
ggplot(summary_data, aes(x = Year_Group, y = Avg_Precipitation, color = Location)) +
  geom_line() +
  labs(x = "5-Year Period", y = "Average Precipitation (mm)", color = "Location") +
  ggtitle("Average Precipitation Time Series for Engelberg and Luzern") +
  theme_minimal()

# Gruppieren nach Jahr und Standort und Berechnen des Durchschnitts pro Jahr
summary_data <- combined_data %>%
  group_by(Year, Location) %>%
  summarise(Avg_Temperature = mean(Temperature),
            Avg_Precipitation = mean(Precipitation))

# Konvertieren der aggregierten Daten in eine Zeitreihe
temp_ts <- ts(summary_data$Avg_Temperature, start = c(1990, 1), frequency = 12)
precip_ts <- ts(summary_data$Avg_Precipitation, start = c(1990, 1), frequency = 12)

# Anwenden der decompose-Funktion
temp_decomposed <- decompose(temp_ts)
precip_decomposed <- decompose(precip_ts)

# Plotten der saisonalen, trendigen und saisonal bereinigten Komponenten der Temperaturzeitreihe
plot(temp_decomposed)

# Plotten der saisonalen, trendigen und saisonal bereinigten Komponenten der Niederschlagszeitreihe
plot(precip_decomposed)

#--------------------------------------------------------------------------------


#### load libraries ####
library(tidyverse) # for data wrangling
library(lubridate) # date manipulation
library(TSstudio) # time series interactive viz
library(tseries) # for adf.test
library(astsa)
library(imputeTS)
library(forecast)
library(magrittr)

# Clean Files

# Engelberg
engelberg <- read.csv("Daten/Engelberg_data.csv", sep=';', na.strings=c('-', ''))

engelberg$time <- as.Date(as.character(engelberg$time), format = "%Y%m%d")
engelberg_clean <- engelberg %>% 
  select(time, tre200nn) %>% 
  rename(temp = tre200nn) %>%
  filter(year(time) > 1989)

# NA behandeln, z.B. durch Ersetzen mit dem Durchschnitt oder einem anderen Verfahren
engelberg_clean <- engelberg_clean %>%
  na.replace(fill = "auto")

# Umwandlung des Datums in numerische Werte für die Regression
engelberg_clean$year_numeric <- as.numeric(format(engelberg_clean$time, "%Y")) + 
                               (as.numeric(format(engelberg_clean$time, "%j")) - 1) / 365.25

# Lineare Regression für die Temperaturtrendlinie
engelberg_clean_trend <- lm(temp ~ year_numeric, data = engelberg_clean)

# Erstellen eines Vektors mit vorhergesagten Werten basierend auf dem Modell
predicted_values <- predict(engelberg_clean_trend, newdata = data.frame(year_numeric = engelberg_clean$year_numeric))

# Erstellen eines Dataframes zur Verwendung mit lines()
trend_data <- data.frame(time = engelberg_clean$time, fitted_values = predicted_values)

# Sortieren des Dataframes nach Datum für die Plot-Funktion
trend_data <- trend_data %>% arrange(time)

# Plotten der Zeitreihe
plot(engelberg_clean$time, engelberg_clean$temp, type = 'l', xlab = "Year", ylab = "Temperature", main = "Temperature Time Series for Engelberg")

# Hinzufügen der Trendlinie
lines(trend_data$time, trend_data$fitted_values, col = 'red')












#---------------------------------------------------------------------------------
# Trendlinien Engelberg
#---------------------------------------------------------------------------------
# Laden der Bibliotheken
library(tidyverse)
library(lubridate)
library(imputeTS)

data_folder <- "Daten/"

# Laden und Vorbereiten der Daten aus "Engelberg_data.csv"
engelberg_data <- read.csv(paste0(data_folder, "Engelberg_data.csv"), sep=';', na.strings=c('-', ''))

engelberg_data$time <- as.Date(as.character(engelberg_data$time), format = "%Y%m%d")
engelberg_clean <- engelberg_data %>% 
  select(time, tre200nn) %>% 
  rename(temp = tre200nn) %>%
  filter(year(time) > 1989)

# Behandeln von NA-Werten
engelberg_clean <- na.replace(engelberg_clean, fill = "auto")

# Umwandeln des Datums in numerische Werte für die Regression
engelberg_clean$year_numeric <- as.numeric(format(engelberg_clean$time, "%Y")) +
                               (as.numeric(format(engelberg_clean$time, "%j")) - 1) / 365.25

# Lineare Regression für die Temperaturtrendlinie
engelberg_clean_trend <- lm(temp ~ year_numeric, data = engelberg_clean)

# Vorhergesagte Werte basierend auf dem Modell
predicted_values <- predict(engelberg_clean_trend, newdata = data.frame(year_numeric = engelberg_clean$year_numeric))

# Erstellen eines Dataframes zur Verwendung mit lines()
trend_data <- data.frame(time = engelberg_clean$time, fitted_values = predicted_values)

# Laden und Vorbereiten der Daten aus "messreihe_engelberg.csv"
engelberg_messreihe <- read_csv(paste0(data_folder, "messreihe_engelberg.csv"), 
                                show_col_types = FALSE) %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
  filter(Year >= 1990)

# Lineare Regression für die monatlichen Daten
lm_engelberg_messreihe <- lm(Temperature ~ Year + Month, data = engelberg_messreihe)

# Vorhersagen für die Trendlinie
engelberg_messreihe$fitted_values <- predict(lm_engelberg_messreihe, newdata = engelberg_messreihe)

# Lineare Regression für die zickzackförmigen Messdaten der Engelberg-Messreihe
lm_engelberg_messreihe_zickzack <- lm(Temperature ~ Date, data = engelberg_messreihe)

# Vorhersagen für die Trendlinie der zickzackförmigen Daten
engelberg_messreihe$fitted_values_zickzack <- predict(lm_engelberg_messreihe_zickzack, newdata = engelberg_messreihe)

# Plotten der Temperaturzeitreihen und Trendlinien
ggplot() +
  geom_line(data = engelberg_clean, aes(x = time, y = temp), lwd = 1) +
  geom_line(data = trend_data, aes(x = time, y = fitted_values), color = "blue", lwd = 2) +
  geom_line(data = engelberg_messreihe, aes(x = Date, y = fitted_values_zickzack), color = "red", lwd = 2) +
  labs(x = "Date", y = "Temperature (°C)", title = "Temperature Time Series and Trend Lines for Engelberg") +
  theme_minimal()


# Berechnen der Werte der linearen Regression für die Temperaturtrendlinie
summary_engelberg_clean_trend <- summary(engelberg_clean_trend)
print(summary_engelberg_clean_trend)

# Berechnen der Werte der linearen Regression für die zickzackförmigen Messdaten der Engelberg-Messreihe
summary_lm_engelberg_messreihe_zickzack <- summary(lm_engelberg_messreihe_zickzack)
print(summary_lm_engelberg_messreihe_zickzack)


# Koeffizienten für die Temperaturtrendlinie
coefficients_trend <- coef(engelberg_clean_trend)
slope_trend <- coefficients_trend[2]  # Steigung
intercept_trend <- coefficients_trend[1]  # Y-Achsenabschnitt

# Koeffizienten für die monatlichen Daten
coefficients_monthly <- coef(lm_engelberg_messreihe)
slope_monthly <- coefficients_monthly[2]  # Steigung
intercept_monthly <- coefficients_monthly[1]  # Y-Achsenabschnitt

# Koeffizienten für die zickzackförmigen Messdaten der Engelberg-Messreihe
coefficients_zickzack <- coef(lm_engelberg_messreihe_zickzack)
slope_zickzack <- coefficients_zickzack[2]  # Steigung
intercept_zickzack <- coefficients_zickzack[1]  # Y-Achsenabschnitt

# Ausgabe der Steigungen
print(paste("Steigung der Temperaturtrendlinie:", slope_trend))
print(paste("Steigung der monatlichen Daten:", slope_monthly))
print(paste("Steigung der zickzackförmigen Messdaten:", slope_zickzack))




#---------------------------------------------------------------------------------
# Trendlinien Küssnacht
#---------------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Laden der Bibliotheken
library(tidyverse)
library(lubridate)
library(imputeTS)

data_folder <- "Daten/"

# Laden und Vorbereiten der Daten aus "Seebodenalp_data.csv"
seebodenalp_data <- read.csv(paste0(data_folder, "Seebodenalp_data.csv"), sep=';', na.strings=c('-', ''))

seebodenalp_data$time <- as.Date(as.character(seebodenalp_data$time), format = "%Y%m%d")
seebodenalp_clean <- seebodenalp_data %>% 
  select(time, tre200nn) %>% 
  rename(temp = tre200nn) %>%
  filter(year(time) > 1989)

# Behandeln von NA-Werten und ungültigen Werten
seebodenalp_clean$temp[is.infinite(seebodenalp_clean$temp) | is.nan(seebodenalp_clean$temp)] <- NA
seebodenalp_clean <- na.omit(seebodenalp_clean)

# Umwandeln des Datums in numerische Werte für die Regression
seebodenalp_clean$year_numeric <- as.numeric(format(seebodenalp_clean$time, "%Y")) +
                               (as.numeric(format(seebodenalp_clean$time, "%j")) - 1) / 365.25

# Lineare Regression für die Temperaturtrendlinie
seebodenalp_clean_trend <- lm(temp ~ year_numeric, data = seebodenalp_clean)

# Vorhergesagte Werte basierend auf dem Modell
predicted_values_seebodenalp <- predict(seebodenalp_clean_trend, newdata = data.frame(year_numeric = seebodenalp_clean$year_numeric))

# Erstellen eines Dataframes zur Verwendung mit lines()
trend_data_seebodenalp <- data.frame(time = seebodenalp_clean$time, fitted_values = predicted_values_seebodenalp)

# Laden und Vorbereiten der Daten aus "Messreihe_Luzern.csv"
luzern_messreihe <- read_csv(paste0(data_folder, "Messreihe_Luzern.csv"), 
                                show_col_types = FALSE) %>%
  mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
  filter(Year >= 1990)

# Lineare Regression für die monatlichen Daten
lm_luzern_messreihe <- lm(Temperature ~ Year + Month, data = luzern_messreihe)

# Vorhersagen für die Trendlinie
luzern_messreihe$fitted_values <- predict(lm_luzern_messreihe, newdata = luzern_messreihe)

# Lineare Regression für die zickzackförmigen Messdaten der Luzern-Messreihe
lm_luzern_messreihe_zickzack <- lm(Temperature ~ Date, data = luzern_messreihe)

# Vorhersagen für die Trendlinie der zickzackförmigen Daten
luzern_messreihe$fitted_values_zickzack <- predict(lm_luzern_messreihe_zickzack, newdata = luzern_messreihe)

# Plotten der Temperaturzeitreihen und Trendlinien
ggplot() +
  geom_line(data = seebodenalp_clean, aes(x = time, y = temp), lwd = 1) +
  geom_line(data = trend_data_seebodenalp, aes(x = time, y = fitted_values), color = "blue", lwd = 2) +
  geom_line(data = luzern_messreihe, aes(x = Date, y = fitted_values_zickzack), color = "red", lwd = 2) +
  labs(x = "Date", y = "Temperature (°C)", title = "Temperature Time Series and Trend Lines for Seebodenalp & Luzern") +
  theme_minimal()


# Berechnen der Werte der linearen Regression für die Temperaturtrendlinie
summary_seebodenalp_clean_trend <- summary(seebodenalp_clean_trend)
print(summary_seebodenalp_clean_trend)

# Berechnen der Werte der linearen Regression für die zickzackförmigen Messdaten der Seebodenalp-Messreihe
summary_lm_seebodenalp_messreihe_zickzack <- summary(lm_seebodenalp_messreihe_zickzack)
print(summary_lm_seebodenalp_messreihe_zickzack)


# Koeffizienten für die Temperaturtrendlinie
coefficients_trend_seebodenalp <- coef(seebodenalp_clean_trend)
slope_trend_seebodenalp <- coefficients_trend_seebodenalp[2]  # Steigung
intercept_trend_seebodenalp <- coefficients_trend_seebodenalp[1]  # Y-Achsenabschnitt

# Koeffizienten für die monatlichen Daten
coefficients_monthly_luzern <- coef(lm_luzern_messreihe)
slope_monthly_luzern <- coefficients_monthly_luzern[2]  # Steigung
intercept_monthly_luzern <- coefficients_monthly_luzern[1]  # Y-Achsenabschnitt

# Koeffizienten für die zickzackförmigen Messdaten der Luzern-Messreihe
coefficients_zickzack_luzern <- coef(lm_luzern_messreihe_zickzack)
slope_zickzack_luzern <- coefficients_zickzack_luzern[2]  # Steigung
intercept_zickzack_luzern <- coefficients_zickzack_luzern[1]  # Y-Achsenabschnitt

# Ausgabe der Steigungen
print(paste("Steigung der Temperaturtrendlinie (Seebodenalp):", slope_trend_seebodenalp))
print(paste("Steigung der monatlichen Daten (Luzern):", slope_monthly_luzern))
print(paste("Steigung der zickzackförmigen Messdaten (Luzern):", slope_zickzack_luzern))
