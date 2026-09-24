# notes.md

## Überblick

Die App zeigt Wetterdaten standortbezogen an und bewertet diese anhand fester Schwellenwerte.

## Aktuelle Schwellenwerte

- Hitze: Warnung ab **35°C**
- Wind: Warnung ab **60 km/h**
- Niederschlag: Warnung ab **10 mm/h**
- Sichtweite: Warnung bei **unter 1 km**

## Technische Hinweise

- Datenabruf erfolgt über Open-Meteo (`get_weather`).
- Standortname wird über Nominatim (`reverse_geocode`) ermittelt.
- Auto-Refresh ist auf 600.000 ms (10 Minuten) gesetzt.
- Die Wetter-Szene wird als SVG dynamisch aus Wettercode, Wind und Regen berechnet.

## Mögliche nächste Schritte

- Konfigurierbare Schwellenwerte über UI ergänzen
- Fehler-/Fallback-Anzeige bei API-Ausfall verbessern
- Optionales Caching zur Reduktion externer Requests einbauen
