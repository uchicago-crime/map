library(dplyr)
library(stringr)
library(sf)
library(leaflet)
library(htmltools)
library(lubridate)


# Importing and Cleaning Data ---------------------------------------------

crime_sf <- read.csv("data/23-24_crime.csv") %>%
  st_as_sf(coords = c("long", "lat")) %>% 
  mutate(
    Incident = if_else(str_detect(Incident, "Theft"), "Theft", Incident),
    Incident = if_else(str_detect(Incident, "Robbery"),
                       "Armed Robbery", Incident),
    Incident = if_else(str_detect(Incident, "Battery|Assault"),
                       "Battery/Assault", Incident)
  )


# Leaflet Creation --------------------------------------------------------

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(0%,0%);
    position: fixed !important;
    bottom: 3%;
    text-align: center;
    padding-left: 0.3em; 
    padding-right: 0.3em; 
    background: rgba(255,255,255,0.5);
    font-weight: bold;
    font-size: 3em;
    font-color: 'white';
  }
"))

title <- tags$div(
  tag.map.title, HTML("UChicago Crime – SY23-24")
)  

tag.map.subtitle <- tags$style(HTML("
  .leaflet-control.map-subtitle { 
    transform: translate(0%, 0%);
    position: fixed !important;
    bottom: 0%;
    text-align: center;
    padding-left: 0.3em; 
    padding-right: 0.3em; 
    background: rgba(255,255,255,0.5);
    font-weight: bold;
    font-size: 1.5em;
    font-color: 'white';
  }
"))

subtitle <- tags$div(
  tag.map.subtitle, HTML(str_glue("Updated {format.Date(today(), '%m/%d/%Y')}"))
)  

leaflet_map <- leaflet(
  data = crime_sf,
  options = leafletOptions(minZoom = 15)
) %>% 
  addTiles() %>%
  addControl(
    title, position = "bottomleft", className="map-title"
  ) %>% 
  addControl(
    subtitle,position = "bottomleft", className="map-subtitle"
  ) %>% 
  setView(
    lng = -87.594633, 
    lat = 41.793983, 
    zoom = 15
  ) %>% 
  setMaxBounds(
    lng1 = -87.610,
    lat1 = 41.783,
    lng2 = -87.583,
    lat2 = 41.805
  ) %>% 
  addProviderTiles(
    providers$Stadia.AlidadeSmoothDark
  ) %>% 
  addCircleMarkers(
    label = ~ str_glue("{Incident} on {Occurred}"), # line break only with popup
    color = ~ if_else(Incident == "Armed Robbery", "red", 
                      if_else(Incident == "Battery/Assault", "orange", "pink")),
    fillOpacity = 0.8,
    radius = ~ if_else(Incident == "Armed Robbery", 7, 
                       if_else(Incident == "Battery/Assault", 5, 4)),
    stroke = FALSE
  ) %>% 
  addLegend(
    title = "Crime Type",
    position = "bottomright",
    colors = c("red", "orange", "pink"),
    labels = c("Armed Robbery", "Battery/Assault", "Theft")
  )

htmlwidgets::saveWidget(leaflet_map, 
                        file = "docs/index.html",
                        title = "SY23-24 UChicago Crime")
