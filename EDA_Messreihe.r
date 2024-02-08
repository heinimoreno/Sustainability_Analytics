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

# Gruppieren nach Jahr und Standort und Berechnen des Durchschnitts pro Jahr
summary_data <- combined_data %>%
  group_by(Year, Location) %>%
  summarise(Avg_Temperature = mean(Temperature),
            Avg_Precipitation = mean(Precipitation))

# Plotten der Temperaturzeitreihe für Engelberg und Luzern
ggplot(summary_data, aes(x = Year, y = Avg_Temperature, color = Location)) +
  geom_line() +
  labs(x = "Year", y = "Average Temperature (°C)", color = "Location") +
  ggtitle("Average Temperature Time Series for Engelberg and Luzern") +
  theme_minimal()

# Plotten der Niederschlagszeitreihe für Engelberg und Luzern
ggplot(summary_data, aes(x = Year, y = Avg_Precipitation, color = Location)) +
  geom_line() +
  labs(x = "Year", y = "Average Precipitation (mm)", color = "Location") +
  ggtitle("Average Precipitation Time Series for Engelberg and Luzern") +
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
temp_ts <- ts(summary_data$Avg_Temperature, start = c(1864, 1), frequency = 12)
precip_ts <- ts(summary_data$Avg_Precipitation, start = c(1864, 1), frequency = 12)

# Fehlende Werte in der Zeitreihe ersetzen (hier mit 0)
temp_ts_filled <- na.fill(temp_ts, 0)
precip_ts_filled <- na.fill(precip_ts, 0)

# Anwenden der decompose-Funktion
temp_decomposed <- decompose(temp_ts_filled)
precip_decomposed <- decompose(precip_ts_filled)

# Plotten der saisonalen, trendigen und saisonal bereinigten Komponenten der Temperaturzeitreihe
plot(temp_decomposed)

# Plotten der saisonalen, trendigen und saisonal bereinigten Komponenten der Niederschlagszeitreihe
plot(precip_decomposed)
