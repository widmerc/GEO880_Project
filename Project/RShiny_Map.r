# Load required libraries
#library(shiny)
library(dplyr)
library(sf)
library(tidyr)
library(leaflet)
library(rlang)

# Beispiel für die Bird-Daten (vermutlich von einer CSV-Datei)
bird_data <- read.csv("data/NuCra_Davos_all_data_2025-02-07_V2.csv")

# Check and handle missing values
bird_data <- bird_data %>% drop_na(longitude, latitude, datetime)

# Convert datetime to proper format
bird_data$datetime <- as.POSIXct(bird_data$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Convert to sf object
bird_data <- bird_data %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  arrange(id, datetime)

# Create line geometries per bird
bird_tracks <- bird_data %>%
  group_by(id) %>%
  summarise(
    geometry = st_cast(st_combine(geometry), "LINESTRING"),
    hdop = mean(hdop, na.rm = TRUE),
    altitude = mean(altitude, na.rm = TRUE),
    satellites = mean(satellites, na.rm = TRUE),
    tag_type = first(tag.type),
    year = first(year),
    month = first(month),
    season = first(season),
    brutzeit = first(brutzeit),
    ring_no = first(ring_no),
    bag = first(bag),
    bird_and_bag = first(bird_and_bag),
    weight = first(weight),
    wing_length = first(wing_length),
    bill_depth = first(bill_depth),
    bill_length = first(bill_length),
    tarsus_length = first(tarsus_length),
    feathers = first(feathers),
    stage_at_capture = first(stage.at.capture),
    photo = first(photo),
    datetime_at_capture = first(datetime.at.capture),
    timediff = mean(timediff, na.rm = TRUE),
    steplength = sum(steplength, na.rm = TRUE),
    stepsize_from_last_hour = mean(stepsize.from.last.hour, na.rm = TRUE),
    stage_current = first(stage.current),
    id_stage = first(id.stage),
    date = first(date),
    ndays_new = first(ndays.new),
    n_datapoints = first(n.datapoints),
    dates_spanned_per_year = first(dates.spanned.per.year),
    .groups = 'drop'
  )

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Liniendaten mit Leaflet in Shiny"),
  
  # Sidebar Layout with collapsible filter panel on top
  sidebarLayout(
    sidebarPanel(
      # Collapsible filter panel
      wellPanel(
        collapsible = TRUE,  # Make the panel collapsible
        collapsed = FALSE,   # Start the filter panel expanded
        sliderInput("year_filter", "Select Year Range:",
                    min = min(bird_tracks$year), 
                    max = max(bird_tracks$year), 
                    value = c(min(bird_tracks$year), max(bird_tracks$year)), 
                    step = 1, animate = TRUE)
      )
    ),
    
    # Main panel to display the map at the bottom
    mainPanel(
      leafletOutput("map", height = "500px")  # Set a fixed height for the map
    )
  )
)


# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  # Reactive expression to filter the data based on selected year range
  filtered_data <- reactive({
    # Filter bird_tracks based on the selected year range from the slider
    bird_tracks %>%
      filter(year >= input$year_filter[1] & year <= input$year_filter[2])
  })
  
  # Create color palette for different bird ids
  pal <- colorFactor(palette = "Set1", domain = bird_tracks$id)
  
  # Render the leaflet map based on filtered data
  output$map <- renderLeaflet({
    # Get filtered data
    data <- filtered_data()
    
    # Create a leaflet map with filtered data
    leaflet(data) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolylines(
        color = ~pal(id),  # Color by bird id
        weight = 2,  # Line width
        opacity = 0.4
      ) %>%
      setView(lng = median(st_coordinates(data)[,1]), lat = median(st_coordinates(data)[,2]), zoom = 10)  # Set center and zoom level
  })
}

# Run the application
shinyApp(ui = ui, server = server)