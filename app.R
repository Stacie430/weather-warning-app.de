library(shiny)
library(httr)
library(jsonlite)
library(leaflet)

# ── API ───────────────────────────────────────────────────────────────────────
get_weather <- function(lat, lon) {
  url <- paste0(
    "https://api.open-meteo.com/v1/forecast",
    "?latitude=", lat, "&longitude=", lon,
    "&current=temperature_2m,windspeed_10m,precipitation,visibility,weathercode",
    "&hourly=temperature_2m,windspeed_10m,precipitation,weathercode",
    "&daily=temperature_2m_max,temperature_2m_min,windspeed_10m_max,precipitation_sum,weathercode",
    "&timezone=auto&forecast_days=7"
  )
  tryCatch({
    res <- GET(url, timeout(10))
    if (status_code(res) != 200) return(NULL)
    fromJSON(content(res, "text", encoding = "UTF-8"))
  }, error = function(e) NULL)
}

reverse_geocode <- function(lat, lon) {
  url <- paste0("https://nominatim.openstreetmap.org/reverse?lat=", lat,
                "&lon=", lon, "&format=json")
  tryCatch({
    res <- GET(url, add_headers("User-Agent" = "StormWatch/1.0"), timeout(5))
    d <- fromJSON(content(res, "text", encoding = "UTF-8"))
    city <- d$address$city %||% d$address$town %||% d$address$village %||% "Unknown"
    country <- d$address$country %||% ""
    paste0(city, ", ", country)
  }, error = function(e) paste0("Lat ", round(lat,2), " Lon ", round(lon,2)))
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

weather_icon <- function(code) {
  if (is.null(code) || is.na(code)) return("🌡")
  if (code == 0) return("☀️")
  if (code %in% 1:3) return("⛅")
  if (code %in% 45:48) return("🌫")
  if (code %in% 51:67) return("🌧")
  if (code %in% 71:77) return("❄️")
  if (code %in% 80:82) return("🌦")
  if (code %in% 95:99) return("⛈")
  return("🌡")
}

weather_label <- function(code) {
  if (is.null(code) || is.na(code)) return("Unknown")
  if (code == 0) return("Clear sky")
  if (code %in% 1:3) return("Partly cloudy")
  if (code %in% 45:48) return("Foggy")
  if (code %in% 51:67) return("Rain")
  if (code %in% 71:77) return("Snow")
  if (code %in% 80:82) return("Showers")
  if (code %in% 95:99) return("Thunderstorm")
  return("Mixed")
}

check_warnings <- function(temp, wind, rain, vis_km) {
  list(
    heat = list(val=temp,   unit="°C",   limit=35,  label="Warning at 35°C",    pct=min(100,round(temp/35*100))),
    wind = list(val=wind,   unit=" km/h",limit=60,  label="Warning at 60 km/h", pct=min(100,round(wind/60*100))),
    rain = list(val=rain,   unit=" mm/h",limit=10,  label="Warning at 10 mm/h", pct=min(100,round(rain/10*100))),
    fog  = list(val=vis_km, unit=" km",  limit=1,   label="Warning below 1 km", pct=min(100,round((1-min(vis_km,10)/10)*100)))
  )
}

overall_status <- function(w) {
  vals <- c(w$heat$pct, w$wind$pct, w$rain$pct, w$fog$pct)
  if (any(vals >= 100)) "danger" else if (any(vals >= 80)) "warning" else "clear"
}

day_label <- function(date_str) {
  d <- as.Date(date_str)
  today <- Sys.Date()
  if (d == today) "Today"
  else if (d == today+1) "Tomorrow"
  else format(d, "%a %d")
}

# ── CSS ───────────────────────────────────────────────────────────────────────
app_css <- "
@import url('https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;500;600&display=swap');
*{box-sizing:border-box;margin:0;padding:0}
body{font-family:'Space Grotesk',sans-serif;background:#0f1623;color:#e8edf5;min-height:100vh}

.sw-header{background:#131d2e;border-bottom:1px solid #1e2d45;padding:14px 24px;display:flex;align-items:center;justify-content:space-between}
.sw-logo{display:flex;align-items:center;gap:10px;font-size:16px;font-weight:600}
.sw-logo-dot{width:10px;height:10px;border-radius:50%;background:#1e90ff;animation:pulse-dot 2s infinite}
@keyframes pulse-dot{0%,100%{opacity:1;transform:scale(1)}50%{opacity:.5;transform:scale(1.4)}}
.sw-nav{display:flex;gap:4px}
.sw-nav-btn{background:transparent;border:1px solid transparent;color:#4a6280;font-family:'Space Grotesk',sans-serif;font-size:13px;padding:6px 14px;border-radius:6px;cursor:pointer;transition:.2s}
.sw-nav-btn:hover{color:#e8edf5;background:#1a2740}
.sw-nav-btn.active{color:#1e90ff;background:rgba(30,144,255,.12);border-color:rgba(30,144,255,.3)}
.sw-updated{font-size:12px;color:#4a6280}

.sw-page{display:none;padding:20px}
.sw-page.active{display:block}
.sw-two-col{display:grid;grid-template-columns:1fr 300px;gap:16px}

/* Hero */
.sw-hero{border-radius:14px;border:1px solid #1e2d45;padding:20px 24px;margin-bottom:16px;display:flex;align-items:center;gap:20px;background:#131d2e;transition:border-color .4s}
.sw-hero.warning{border-color:#c47a00}.sw-hero.danger{border-color:#c0392b}
.sw-pulse{position:relative;width:64px;height:64px;flex-shrink:0;display:flex;align-items:center;justify-content:center}
.sw-pulse-ring{position:absolute;border-radius:50%;border:1.5px solid #1e90ff;animation:ring-out 2.4s ease-out infinite;width:64px;height:64px;opacity:0}
.sw-pulse-ring.r2{animation-delay:1.2s}
.sw-hero.warning .sw-pulse-ring{border-color:#ffb700}.sw-hero.danger .sw-pulse-ring{border-color:#ff3b3b;animation-duration:1s}
@keyframes ring-out{0%{width:24px;height:24px;opacity:.9}100%{width:64px;height:64px;opacity:0}}
.sw-pulse-core{width:26px;height:26px;border-radius:50%;border:2px solid #1e90ff;background:rgba(30,144,255,.15);display:flex;align-items:center;justify-content:center;font-size:13px;z-index:1}
.sw-hero.warning .sw-pulse-core{border-color:#ffb700;background:rgba(255,183,0,.15)}
.sw-hero.danger .sw-pulse-core{border-color:#ff3b3b;background:rgba(255,59,59,.15)}
.sw-hero-text{flex:1}
.sw-hero-label{font-size:10px;text-transform:uppercase;letter-spacing:.1em;color:#4a6280;margin-bottom:4px}
.sw-hero-title{font-size:20px;font-weight:600;margin-bottom:2px}
.sw-hero-sub{font-size:13px;color:#7a9abf}
.sw-badge{font-size:12px;font-weight:500;padding:5px 12px;border-radius:6px;white-space:nowrap}
.sw-badge.clear{background:rgba(39,174,96,.15);color:#27ae60;border:1px solid rgba(39,174,96,.3)}
.sw-badge.warning{background:rgba(255,183,0,.15);color:#ffb700;border:1px solid rgba(255,183,0,.3)}
.sw-badge.danger{background:rgba(255,59,59,.15);color:#ff3b3b;border:1px solid rgba(255,59,59,.3)}

/* Tiles */
.sw-tiles{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin-bottom:16px}
.sw-tile{background:#131d2e;border:1px solid #1e2d45;border-radius:12px;padding:14px 16px;transition:border-color .3s}
.sw-tile.near{border-color:#c47a00}.sw-tile.over{border-color:#c0392b}
.sw-tile-label{font-size:10px;text-transform:uppercase;letter-spacing:.08em;color:#4a6280;margin-bottom:8px}
.sw-tile-value{font-size:24px;font-weight:600;line-height:1}
.sw-tile-unit{font-size:11px;color:#4a6280;margin-top:4px}
.sw-tile.near .sw-tile-value{color:#ffb700}.sw-tile.over .sw-tile-value{color:#ff3b3b}

/* Chart */
.sw-chart-box{background:#131d2e;border:1px solid #1e2d45;border-radius:12px;padding:16px 20px;margin-bottom:16px}
.sw-chart-top{display:flex;justify-content:space-between;align-items:center;margin-bottom:14px}
.sw-chart-title{font-size:13px;font-weight:500}
.sw-chart-tabs{display:flex;gap:3px}
.sw-chart-tab{background:transparent;border:1px solid transparent;color:#4a6280;font-family:'Space Grotesk',sans-serif;font-size:11px;padding:4px 10px;border-radius:5px;cursor:pointer}
.sw-chart-tab.active{background:#1a2740;color:#e8edf5;border-color:#1e2d45}
.sw-bars{display:flex;align-items:flex-end;gap:5px;height:100px}
.sw-bar-wrap{flex:1;display:flex;flex-direction:column;align-items:center;gap:4px}
.sw-bar{width:100%;border-radius:3px 3px 0 0;min-height:2px;transition:height .5s}
.sw-bar-lbl{font-size:9px;color:#4a6280}

/* Forecast */
.sw-forecast-grid{display:grid;grid-template-columns:repeat(7,1fr);gap:8px;margin-bottom:16px}
.sw-fc-card{background:#131d2e;border:1px solid #1e2d45;border-radius:10px;padding:12px 8px;text-align:center}
.sw-fc-day{font-size:10px;color:#4a6280;text-transform:uppercase;letter-spacing:.06em;margin-bottom:6px}
.sw-fc-icon{font-size:22px;margin-bottom:6px}
.sw-fc-hi{font-size:15px;font-weight:600}
.sw-fc-lo{font-size:12px;color:#4a6280;margin-top:2px}
.sw-fc-wind{font-size:10px;color:#4a6280;margin-top:6px;padding-top:6px;border-top:1px solid #1e2d45}

/* Sidebar */
.sw-sidebar{}
.sw-side-label{font-size:10px;text-transform:uppercase;letter-spacing:.1em;color:#4a6280;margin-bottom:10px}
.sw-loc-row{margin-bottom:16px;padding-bottom:16px;border-bottom:1px solid #1e2d45}
.sw-loc-name{font-size:15px;font-weight:500;margin-bottom:2px}
.sw-loc-coords{font-size:11px;color:#4a6280}
.sw-thresh{background:#131d2e;border:1px solid #1e2d45;border-radius:10px;padding:12px 14px;margin-bottom:8px;transition:border-color .3s}
.sw-thresh.near{border-color:#c47a00;background:rgba(255,183,0,.04)}
.sw-thresh.over{border-color:#c0392b;background:rgba(255,59,59,.04)}
.sw-thresh.safe{border-color:rgba(39,174,96,.25)}
.sw-thresh-top{display:flex;align-items:center;gap:8px;margin-bottom:8px}
.sw-thresh-icon{width:28px;height:28px;border-radius:6px;background:#1a2740;display:flex;align-items:center;justify-content:center;font-size:14px;flex-shrink:0}
.sw-thresh-name{font-size:13px;font-weight:500;flex:1}
.sw-thresh.near .sw-thresh-name{color:#ffb700}.sw-thresh.over .sw-thresh-name{color:#ff3b3b}.sw-thresh.safe .sw-thresh-name{color:#27ae60}
.sw-thresh-val{font-size:11px;color:#4a6280}
.sw-prog{height:3px;border-radius:2px;background:#1e2d45;overflow:hidden;margin-bottom:4px}
.sw-prog-fill{height:100%;border-radius:2px;background:#1e90ff;transition:width .6s}
.sw-thresh.near .sw-prog-fill{background:#ffb700}.sw-thresh.over .sw-prog-fill{background:#ff3b3b}.sw-thresh.safe .sw-prog-fill{background:#27ae60}
.sw-thresh-hint{font-size:10px;color:#4a6280}

/* Alerts */
.sw-alert-list{display:flex;flex-direction:column;gap:10px}
.sw-alert-item{background:#131d2e;border:1px solid #1e2d45;border-radius:10px;padding:14px 16px}
.sw-alert-item.danger{border-color:#c0392b;background:rgba(255,59,59,.05)}
.sw-alert-item.warning{border-color:#c47a00;background:rgba(255,183,0,.04)}
.sw-alert-item.clear{border-color:rgba(39,174,96,.3);background:rgba(39,174,96,.04)}
.sw-alert-top{display:flex;align-items:center;gap:10px;margin-bottom:6px}
.sw-alert-dot{width:8px;height:8px;border-radius:50%;flex-shrink:0}
.sw-alert-item.danger .sw-alert-dot{background:#ff3b3b}.sw-alert-item.warning .sw-alert-dot{background:#ffb700}.sw-alert-item.clear .sw-alert-dot{background:#27ae60}
.sw-alert-name{font-size:14px;font-weight:500}
.sw-alert-desc{font-size:12px;color:#7a9abf;line-height:1.5}
.sw-alert-time{font-size:11px;color:#4a6280;margin-top:6px}

/* Inputs */
.sw-input-row{display:flex;gap:8px;margin-bottom:10px}
.sw-input-row .form-control,.sw-input-row input{flex:1;background:#1a2740;border:1px solid #1e2d45;border-radius:6px;color:#e8edf5;font-family:'Space Grotesk',sans-serif;font-size:13px;padding:7px 10px;outline:none}
.sw-input-row .form-control:focus,.sw-input-row input:focus{border-color:#1e90ff}
.form-group{margin-bottom:0}
.sw-btn{background:rgba(30,144,255,.15);border:1px solid rgba(30,144,255,.4);color:#1e90ff;font-family:'Space Grotesk',sans-serif;font-size:13px;font-weight:500;padding:7px 14px;border-radius:6px;cursor:pointer;white-space:nowrap;width:100%;margin-bottom:12px}
.sw-btn:hover{background:rgba(30,144,255,.25)}
.sw-footer{font-size:11px;color:#4a6280;text-align:center;margin-top:16px}
.leaflet-container{border-radius:10px}
#map{border-radius:10px;border:1px solid #1e2d45;margin-bottom:16px}
"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(HTML(app_css)),
    tags$title("StormWatch"),
    tags$script(HTML("
      function switchTab(name) {
        document.querySelectorAll('.sw-page').forEach(p => p.classList.remove('active'));
        document.querySelectorAll('.sw-nav-btn').forEach(b => b.classList.remove('active'));
        document.getElementById('page-' + name).classList.add('active');
        document.getElementById('btn-' + name).classList.add('active');
      }
      function switchChart(type) {
        document.querySelectorAll('.sw-chart-tab').forEach(t => t.classList.remove('active'));
        event.target.classList.add('active');
        Shiny.setInputValue('chart_type', type, {priority: 'event'});
      }
    "))
  ),

  div(class="sw-header",
    div(class="sw-logo", div(class="sw-logo-dot"), "StormWatch"),
    div(class="sw-nav",
      tags$button(id="btn-dashboard", class="sw-nav-btn active", onclick="switchTab('dashboard')", "Dashboard"),
      tags$button(id="btn-forecast",  class="sw-nav-btn",        onclick="switchTab('forecast')",  "Forecast"),
      tags$button(id="btn-map",       class="sw-nav-btn",        onclick="switchTab('map')",        "Map"),
      tags$button(id="btn-alerts",    class="sw-nav-btn",        onclick="switchTab('alerts')",     "Alerts")
    ),
    div(class="sw-updated", textOutput("last_updated", inline=TRUE))
  ),

  # ── DASHBOARD ──────────────────────────────────────────────────────────────
  div(id="page-dashboard", class="sw-page active",
    div(class="sw-two-col",
      div(
        uiOutput("hero"),
        uiOutput("weather_scene_ui"),
        uiOutput("tiles"),
        div(class="sw-chart-box",
          div(class="sw-chart-top",
            div(class="sw-chart-title", "Hourly data · next 12 hours"),
            div(class="sw-chart-tabs",
              tags$button(class="sw-chart-tab active", onclick="switchChart('wind')",  "Wind"),
              tags$button(class="sw-chart-tab",        onclick="switchChart('temp')",  "Temp"),
              tags$button(class="sw-chart-tab",        onclick="switchChart('rain')",  "Rain")
            )
          ),
          uiOutput("hourly_chart")
        )
      ),
      div(class="sw-sidebar",
        div(class="sw-loc-row",
          div(class="sw-loc-name", textOutput("loc_name", inline=TRUE)),
          div(class="sw-loc-coords", textOutput("loc_coords", inline=TRUE))
        ),
        div(class="sw-side-label", "Change location"),
        div(class="sw-input-row",
          numericInput("lat", NULL, value=51.96, min=-90,  max=90,  step=0.01),
          numericInput("lon", NULL, value=7.63,  min=-180, max=180, step=0.01)
        ),
        tags$button(class="sw-btn",
          onclick="Shiny.setInputValue('refresh', Math.random())", "Check weather"),
        div(class="sw-side-label", "Warning thresholds"),
        uiOutput("thresholds"),
        div(class="sw-footer", "Data: Open-Meteo · auto-refresh 10 min")
      )
    )
  ),

  # ── FORECAST ───────────────────────────────────────────────────────────────
  div(id="page-forecast", class="sw-page",
    uiOutput("forecast_cards"),
    div(class="sw-chart-box",
      div(class="sw-chart-top",
        div(class="sw-chart-title", "7-day temperature range"),
        div()
      ),
      uiOutput("forecast_chart")
    )
  ),

  # ── MAP ────────────────────────────────────────────────────────────────────
  div(id="page-map", class="sw-page",
    leafletOutput("map", height="500px"),
    uiOutput("map_info")
  ),

  # ── ALERTS ─────────────────────────────────────────────────────────────────
  div(id="page-alerts", class="sw-page",
    div(class="sw-side-label", style="margin-bottom:16px", "Active conditions"),
    div(class="sw-alert-list", uiOutput("alert_list"))
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  autoInvalidate <- reactiveTimer(600000)
  chart_type <- reactiveVal("wind")

  observeEvent(input$chart_type, { chart_type(input$chart_type) })

  weather <- reactive({
    autoInvalidate(); input$refresh
    isolate(get_weather(input$lat, input$lon))
  })

  location_name <- reactive({
    input$refresh
    isolate(reverse_geocode(input$lat, input$lon))
  })

  output$last_updated <- renderText({
    req(weather()); paste("Updated", format(Sys.time(), "%H:%M"))
  })
  output$loc_name   <- renderText({ location_name() })
  output$loc_coords <- renderText({ paste0("Lat ", round(input$lat,2), "  ·  Lon ", round(input$lon,2)) })

  cur <- reactive({
    w <- weather(); req(w)
    list(
      temp = round(w$current$temperature_2m, 1),
      wind = round(w$current$windspeed_10m, 1),
      rain = round(w$current$precipitation, 1),
      vis  = round(w$current$visibility / 1000, 1),
      code = w$current$weathercode
    )
  })

  warns <- reactive({ c <- cur(); check_warnings(c$temp, c$wind, c$rain, c$vis) })
  status <- reactive({ overall_status(warns()) })

  tile_cls <- function(pct) if (pct>=100) "over" else if (pct>=80) "near" else ""

  # Hero
  output$hero <- renderUI({
    s <- status(); w <- warns()
    info <- list(
      clear   = list(title="All clear",      icon="✓", sub="No active warnings — conditions within safe thresholds", badge="Normal"),
      warning = list(title="Advisory",       icon="⚠", sub="One or more conditions approaching the warning threshold", badge="Caution"),
      danger  = list(title="Active warning", icon="!", sub="One or more conditions have exceeded the warning threshold", badge="Warning")
    )[[s]]
    div(class=paste("sw-hero", s),
      div(class="sw-pulse",
        div(class="sw-pulse-ring"), div(class="sw-pulse-ring r2"),
        div(class="sw-pulse-core", info$icon)
      ),
      div(class="sw-hero-text",
        div(class="sw-hero-label", "Current status"),
        div(class="sw-hero-title", info$title),
        div(class="sw-hero-sub",   info$sub)
      ),
      div(class=paste("sw-badge", s), info$badge)
    )
  })

  # Weather scene
  output$weather_scene_ui <- renderUI({
    c <- cur()
    weather_scene(c$code, wind_kmh = c$wind, rain_mmh = c$rain)
  })

  # Tiles
  output$tiles <- renderUI({
    c <- cur(); w <- warns()
    div(class="sw-tiles",
      div(class=paste("sw-tile", tile_cls(w$heat$pct)),
        div(class="sw-tile-label","Temperature"),
        div(class="sw-tile-value", paste0(c$temp,"°")),
        div(class="sw-tile-unit","Celsius")),
      div(class=paste("sw-tile", tile_cls(w$wind$pct)),
        div(class="sw-tile-label","Wind speed"),
        div(class="sw-tile-value", c$wind),
        div(class="sw-tile-unit","km/h")),
      div(class=paste("sw-tile", tile_cls(w$rain$pct)),
        div(class="sw-tile-label","Precipitation"),
        div(class="sw-tile-value", c$rain),
        div(class="sw-tile-unit","mm/hr")),
      div(class=paste("sw-tile", tile_cls(100 - w$fog$pct)),
        div(class="sw-tile-label","Visibility"),
        div(class="sw-tile-value", c$vis),
        div(class="sw-tile-unit","km"))
    )
  })

  # Hourly chart
  output$hourly_chart <- renderUI({
    w <- weather(); req(w); ct <- chart_type()
    vals  <- switch(ct,
      wind = head(w$hourly$windspeed_10m, 12),
      temp = head(w$hourly$temperature_2m, 12),
      rain = head(w$hourly$precipitation, 12)
    )
    times  <- head(w$hourly$time, 12)
    hours  <- format(as.POSIXct(times), "%Hh")
    limit  <- switch(ct, wind=60, temp=35, rain=10)
    max_v  <- max(abs(vals), 1)
    bars <- lapply(seq_along(vals), function(i) {
      pct <- round(abs(vals[i]) / max_v * 100)
      col <- if (vals[i] >= limit) "#ff3b3b" else if (vals[i] >= limit*.8) "#ffb700" else "#1e90ff"
      div(class="sw-bar-wrap",
        div(class="sw-bar", style=paste0("height:",pct,"%;background:",col)),
        div(class="sw-bar-lbl", hours[i]))
    })
    div(class="sw-bars", bars)
  })

  # Thresholds
  thresh_card <- function(icon, name, val, unit, limit_label, pct) {
    cls <- if (pct>=100) "over" else if (pct>=80) "near" else "safe"
    div(class=paste("sw-thresh", cls),
      div(class="sw-thresh-top",
        div(class="sw-thresh-icon", icon),
        div(class="sw-thresh-name", name),
        div(class="sw-thresh-val", paste0(val, unit))
      ),
      div(class="sw-prog", div(class="sw-prog-fill", style=paste0("width:",pct,"%"))),
      div(class="sw-thresh-hint", limit_label)
    )
  }

  output$thresholds <- renderUI({
    c <- cur(); w <- warns()
    tagList(
      thresh_card("🌡","Heat",           c$temp, "°C",   "Warning at 35°C",    w$heat$pct),
      thresh_card("💨","High wind",      c$wind, " km/h","Warning at 60 km/h", w$wind$pct),
      thresh_card("🌧","Heavy rain",     c$rain, " mm/h","Warning at 10 mm/h", w$rain$pct),
      thresh_card("🌫","Low visibility", c$vis,  " km",  "Warning below 1 km", w$fog$pct)
    )
  })

  # Forecast cards
  output$forecast_cards <- renderUI({
    w <- weather(); req(w)
    days   <- w$daily$time
    hi     <- w$daily$temperature_2m_max
    lo     <- w$daily$temperature_2m_min
    winds  <- w$daily$windspeed_10m_max
    codes  <- w$daily$weathercode
    cards <- lapply(seq_along(days), function(i) {
      div(class="sw-fc-card",
        div(class="sw-fc-day",  day_label(days[i])),
        div(class="sw-fc-icon", weather_icon(codes[i])),
        div(class="sw-fc-hi",   paste0(round(hi[i]),"°")),
        div(class="sw-fc-lo",   paste0(round(lo[i]),"°")),
        div(class="sw-fc-wind", paste0(round(winds[i])," km/h"))
      )
    })
    div(class="sw-forecast-grid", cards)
  })

  # Forecast temp chart
  output$forecast_chart <- renderUI({
    w <- weather(); req(w)
    hi  <- w$daily$temperature_2m_max
    lo  <- w$daily$temperature_2m_min
    days <- sapply(w$daily$time, day_label)
    max_t <- max(hi, 1); min_t <- min(lo)
    range_t <- max_t - min_t + 1
    bars <- lapply(seq_along(hi), function(i) {
      hi_pct <- round((hi[i] - min_t) / range_t * 100)
      lo_pct <- round((lo[i] - min_t) / range_t * 100)
      div(class="sw-bar-wrap",
        div(style=paste0("display:flex;flex-direction:column;justify-content:flex-end;height:100%;width:100%;gap:2px"),
          div(class="sw-bar", style=paste0("height:",hi_pct,"%;background:#1e90ff;border-radius:3px 3px 0 0")),
          div(class="sw-bar", style=paste0("height:",lo_pct,"%;background:#1e2d45;border-radius:3px 3px 0 0"))
        ),
        div(class="sw-bar-lbl", days[i])
      )
    })
    div(class="sw-bars", style="height:120px", bars)
  })

  # Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng=input$lon, lat=input$lat, zoom=9)
  })

  observe({
    w <- weather(); c <- cur()
    req(w, c)
    s <- status()
    col <- switch(s, clear="#27ae60", warning="#ffb700", danger="#ff3b3b")
    popup <- paste0(
      "<div style='font-family:Space Grotesk,sans-serif;color:#e8edf5;background:#131d2e;padding:10px;border-radius:8px'>",
      "<b style='font-size:14px'>", location_name(), "</b><br>",
      "<span style='color:#7a9abf;font-size:12px'>", weather_label(c$code), "</span><br><br>",
      "🌡 ", c$temp, "°C &nbsp; 💨 ", c$wind, " km/h<br>",
      "🌧 ", c$rain, " mm/h &nbsp; 👁 ", c$vis, " km",
      "</div>"
    )
    leafletProxy("map") %>%
      clearMarkers() %>%
      setView(lng=input$lon, lat=input$lat, zoom=9) %>%
      addCircleMarkers(
        lng=input$lon, lat=input$lat,
        radius=12, color=col, fillColor=col, fillOpacity=0.3, weight=2,
        popup=popup, popupOptions=popupOptions(closeButton=FALSE)
      )
  })

  output$map_info <- renderUI({
    c <- cur()
    div(style="display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin-top:12px",
      div(class="sw-fc-card", div(class="sw-fc-day","Temperature"), div(class="sw-fc-hi",paste0(c$temp,"°C"))),
      div(class="sw-fc-card", div(class="sw-fc-day","Wind"),        div(class="sw-fc-hi",paste0(c$wind," km/h"))),
      div(class="sw-fc-card", div(class="sw-fc-day","Rain"),        div(class="sw-fc-hi",paste0(c$rain," mm/h"))),
      div(class="sw-fc-card", div(class="sw-fc-day","Visibility"),  div(class="sw-fc-hi",paste0(c$vis," km")))
    )
  })

  # Alerts
  output$alert_list <- renderUI({
    c <- cur(); w <- warns()
    make_alert <- function(icon, name, val, unit, pct, limit, hint) {
      cls  <- if (pct>=100) "danger" else if (pct>=80) "warning" else "clear"
      desc <- if (pct>=100) paste0("⚠️ ", name, " has exceeded the warning threshold (", val, unit, " / limit ", limit, unit, ")")
              else if (pct>=80) paste0("Approaching warning threshold — currently at ", val, unit, " (", pct, "% of limit)")
              else paste0("Within safe range — currently at ", val, unit)
      div(class=paste("sw-alert-item", cls),
        div(class="sw-alert-top",
          div(class="sw-alert-dot"),
          div(class="sw-alert-name", paste(icon, name))
        ),
        div(class="sw-alert-desc", desc),
        div(class="sw-alert-time", paste("Last checked:", format(Sys.time(), "%H:%M:%S")))
      )
    }
    tagList(
      make_alert("🌡","Heat",           c$temp,"°C",   w$heat$pct, 35,  "Extreme heat can cause heatstroke"),
      make_alert("💨","High wind",      c$wind," km/h",w$wind$pct, 60,  "Strong winds may affect travel"),
      make_alert("🌧","Heavy rain",     c$rain," mm/h",w$rain$pct, 10,  "Heavy rain may cause flooding"),
      make_alert("🌫","Low visibility", c$vis, " km",  w$fog$pct,  1,   "Poor visibility reduces driving safety")
    )
  })
}

shinyApp(ui, server)

# ════════════════════════════════════════════════════════════════════════════
# WEATHER SCENE — appended patch
# Adds an animated SVG scene driven by the live weather code.
# Call weather_scene(code, wind_kmh, rain_mmh) from server renderUI.
# ════════════════════════════════════════════════════════════════════════════

weather_scene <- function(code, wind_kmh = 0, rain_mmh = 0) {

  code <- if (is.null(code) || is.na(code)) 0L else as.integer(code)

  # ── what's showing ────────────────────────────────────────────────────────
  show_sun    <- code %in% c(0L, 1L)
  show_clouds <- code %in% c(1L:3L, 45L:48L, 51L:67L, 80L:82L, 95L:99L)
  show_rain   <- code %in% c(51L:67L, 80L:82L, 95L:99L) || rain_mmh >= 1
  show_snow   <- code %in% c(71L:77L)
  show_storm  <- code %in% c(95L:99L)
  show_fog    <- code %in% c(45L:48L)

  # wind speed → animation duration (faster = windier)
  wind_dur  <- max(0.6, round(3 - wind_kmh / 40, 2))
  rain_cnt  <- if (rain_mmh >= 5) 18L else if (rain_mmh >= 1) 10L else 7L
  cloud_opacity <- if (show_fog) 0.55 else if (show_storm) 0.85 else 0.75

  # ── helpers ───────────────────────────────────────────────────────────────
  sun_svg <- function() {
    # rotating rays + core disc
    paste0(
      '<g id="sun-group" style="animation:sun-spin 12s linear infinite;transform-origin:80px 72px">',
      # rays
      paste(sapply(seq(0, 315, by=45), function(a) {
        r <- a * pi / 180
        x1 <- 80 + 30 * cos(r); y1 <- 72 + 30 * sin(r)
        x2 <- 80 + 42 * cos(r); y2 <- 72 + 42 * sin(r)
        sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#FFD060" stroke-width="2.5" stroke-linecap="round" opacity="0.8"/>', x1, y1, x2, y2)
      }), collapse=""),
      '</g>',
      '<circle cx="80" cy="72" r="22" fill="#FFD060" opacity="0.95"/>',
      '<circle cx="80" cy="72" r="18" fill="#FFE590" opacity="0.7"/>'
    )
  }

  cloud_svg <- function(cx, cy, scale=1, op=cloud_opacity, dur=wind_dur, dx=30) {
    s <- scale
    anim_id <- paste0("cloud-", cx)
    paste0(
      sprintf('<g style="animation:%s %ss ease-in-out infinite alternate;transform-box:fill-box;transform-origin:center">', anim_id, dur),
      sprintf('@keyframes %s{from{transform:translateX(0)}to{transform:translateX(%spx)}}', anim_id, dx),
      # cloud body — overlapping ellipses
      sprintf('<ellipse cx="%s" cy="%s" rx="%s" ry="%s" fill="#c8d8f0" opacity="%s"/>',
              cx, cy, 32*s, 18*s, op),
      sprintf('<ellipse cx="%s" cy="%s" rx="%s" ry="%s" fill="#dce8f8" opacity="%s"/>',
              cx-18*s, cy+4*s, 20*s, 14*s, op),
      sprintf('<ellipse cx="%s" cy="%s" rx="%s" ry="%s" fill="#dce8f8" opacity="%s"/>',
              cx+16*s, cy+5*s, 18*s, 13*s, op),
      sprintf('<ellipse cx="%s" cy="%s" rx="%s" ry="%s" fill="#c8d8f0" opacity="%s"/>',
              cx, cy+10*s, 28*s, 12*s, op),
      '</g>'
    )
  }

  rain_drops <- function(n = rain_cnt) {
    set.seed(42)
    xs  <- sample(seq(20, 220, by=8), n, replace=FALSE)
    dys <- sample(seq(0, 1800, by=100), n, replace=TRUE)
    paste(sapply(seq_len(n), function(i) {
      dur2 <- round(runif(1, 0.6, 1.0), 2)
      sprintf(
        '<line x1="%d" y1="-10" x2="%d" y2="6" stroke="#7ab8f5" stroke-width="1.4" stroke-linecap="round" opacity="0.7" style="animation:rain-fall %ss linear %sms infinite"/>',
        xs[i], xs[i]+2, dur2, dys[i]
      )
    }), collapse="")
  }

  snow_flakes <- function(n = 10L) {
    set.seed(7)
    xs  <- sample(seq(15, 225, by=10), n, replace=FALSE)
    dys <- sample(seq(0, 2000, by=200), n, replace=TRUE)
    paste(sapply(seq_len(n), function(i) {
      dur2 <- round(runif(1, 2.0, 3.5), 2)
      sprintf(
        '<circle cx="%d" cy="-8" r="2.5" fill="white" opacity="0.8" style="animation:snow-fall %ss linear %sms infinite"/>',
        xs[i], dur2, dys[i]
      )
    }), collapse="")
  }

  lightning_svg <- function() {
    paste0(
      '<polyline points="120,50 108,80 118,80 104,115" fill="none" stroke="#FFE050" stroke-width="2.5" stroke-linejoin="round" style="animation:lightning-flash 3s ease-in-out infinite" opacity="0"/>',
      '<polyline points="150,45 140,72 149,72 136,108" fill="none" stroke="#FFE050" stroke-width="2" stroke-linejoin="round" style="animation:lightning-flash 3s ease-in-out 1.5s infinite" opacity="0"/>'
    )
  }

  fog_svg <- function() {
    paste(sapply(c(20, 40, 60, 80, 100), function(y) {
      sprintf(
        '<rect x="0" y="%d" width="240" height="6" rx="3" fill="#8fb0d0" opacity="0.18" style="animation:fog-drift 4s ease-in-out infinite alternate"/>',
        y
      )
    }), collapse="")
  }

  # ── keyframe block ────────────────────────────────────────────────────────
  keyframes <- paste0(
    "<style>",
    "@keyframes sun-spin{from{transform:rotate(0deg)}to{transform:rotate(360deg)}}",
    "@keyframes rain-fall{0%{transform:translateY(0)}100%{transform:translateY(160px);opacity:0}}",
    "@keyframes snow-fall{0%{transform:translateY(0) rotate(0deg)}100%{transform:translateY(160px) rotate(180deg);opacity:0}}",
    "@keyframes lightning-flash{0%,85%,100%{opacity:0}88%{opacity:1}92%{opacity:0}96%{opacity:.7}}",
    "@keyframes fog-drift{from{transform:translateX(-10px)}to{transform:translateX(10px)}}",
    "</style>"
  )

  # ── compose scene ─────────────────────────────────────────────────────────
  scene_parts <- keyframes

  if (show_fog)    scene_parts <- paste0(scene_parts, fog_svg())
  if (show_sun)    scene_parts <- paste0(scene_parts, sun_svg())
  # back cloud (larger, slower)
  if (show_clouds) scene_parts <- paste0(scene_parts,
                     cloud_svg(160, 58, scale=1.15, op=cloud_opacity*0.8, dur=wind_dur*1.4, dx=20))
  # front cloud
  if (show_clouds) scene_parts <- paste0(scene_parts,
                     cloud_svg(78, 78, scale=1.0, op=cloud_opacity, dur=wind_dur, dx=28))
  if (show_storm)  scene_parts <- paste0(scene_parts, lightning_svg())
  if (show_rain)   scene_parts <- paste0(scene_parts, rain_drops())
  if (show_snow)   scene_parts <- paste0(scene_parts, snow_flakes())

  # sky gradient via rect
  sky_col <- if (show_storm) "#0a1020"
             else if (show_fog) "#1a2a40"
             else if (show_clouds && !show_sun) "#0f1e34"
             else "#0d1e3a"

  HTML(paste0(
    '<div style="background:', sky_col, ';border-radius:12px;overflow:hidden;height:140px;',
    'border:1px solid #1e2d45;margin-bottom:16px;position:relative">',
    '<svg width="100%" height="140" viewBox="0 0 240 140" xmlns="http://www.w3.org/2000/svg" ',
    'style="position:absolute;inset:0;width:100%;height:100%">',
    scene_parts,
    '</svg>',
    '<div style="position:absolute;bottom:10px;left:14px;font-family:Space Grotesk,sans-serif;',
    'font-size:11px;color:#4a6280;letter-spacing:.06em;text-transform:uppercase">',
    weather_label(code), '</div>',
    '</div>'
  ))
}
