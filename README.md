# weather-warning-app.de

Eine Shiny-Anwendung zur Beobachtung von Wetterbedingungen und zur visuellen Warnung bei kritischen Grenzwerten.

## Funktionen

- Aktuelle Wetterdaten für frei wählbare Koordinaten (Breite/Länge)
- Warnstatus für:
  - Temperatur
  - Windgeschwindigkeit
  - Niederschlag
  - Sichtweite
- 7-Tage-Vorhersage mit Tageskarten
- Interaktive Karte mit Standortmarker und Wetter-Popup
- Eigene Alerts-Ansicht mit klaren/auffälligen Zuständen
- Automatische Aktualisierung alle 10 Minuten

## Datenquellen

- [Open-Meteo](https://open-meteo.com/) für Wetterdaten
- [Nominatim (OpenStreetMap)](https://nominatim.openstreetmap.org/) für Reverse Geocoding

## Voraussetzungen

- R (empfohlen: aktuelle Version)
- Installierte Pakete:
  - `shiny`
  - `httr`
  - `jsonlite`
  - `leaflet`

## Lokaler Start

1. Abhängigkeiten installieren:

   ```r
   install.packages(c("shiny", "httr", "jsonlite", "leaflet"))
   ```

2. Anwendung starten:

   ```r
   shiny::runApp("app.R")
   ```

## Projektdateien

- `/home/runner/work/weather-warning-app.de/weather-warning-app.de/app.R` – komplette Shiny-App (UI + Server + Wetter-Szene)
- `/home/runner/work/weather-warning-app.de/weather-warning-app.de/notes.md` – ergänzende Projekt-Notizen