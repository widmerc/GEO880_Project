# Load required libraries
library(shiny)
library(dplyr)
library(sf)
library(tmap)
library(tidyr)
library(rlang)

#Git Test <- das habe ich neu gemacht

# Load the new bird GPS data
bird_data <- read.csv("data/NuCra_Davos_all_data_2025-02-07_V2.csv")

# Check and handle missing values
bird_data <- bird_data %>% drop_na(longitude, latitude, datetime)

# Convert datetime to proper format
bird_data$datetime <- as.POSIXct(bird_data$datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Convert to sf object
bird_data <- bird_data %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  arrange(id, datetime)

# Create line geometries per bird, and KEEP ALL ATTRIBUTES needed for filtering
bird_tracks <- bird_data %>%
  group_by(id) %>%
  summarise(
    geometry = st_cast(st_combine(geometry), "LINESTRING"),
    # Add all columns for filtering and information
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

# Define UI for the Shiny app with hardcoded categories
ui <- fluidPage(
  titlePanel("Bird GPS Tracks Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      # Bird count indicator - displays the number of birds or directs to info
      textOutput("birdCountInfo"),
      hr(),
      
      # Hardcoded selectInput for filtering based on predefined categories
      selectInput("category", "Choose Category to Filter:", 
                  choices = c("id", "altitude", "year", "month", "season", "brutzeit", "wing_length", "weight", "feathers"),
                  selected = "id"),
      uiOutput("categoryUI"),  # Dynamic UI for the chosen category
      
      # Add a divider
      hr(),
      
      # Conditional UI for bird information
      uiOutput("birdInfoUI")
    ),
    
    mainPanel(
      # Map to display the bird GPS data with click functionality
      tmapOutput("birdMap", height = 600),
      textOutput("noDataMessage")  # Text output for "no data" message
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  # Selected bird reactive value for click interactions
  selected_bird_id <- reactiveVal(NULL)
  
  # Dynamically update the UI based on selected category
  output$categoryUI <- renderUI({
    # Get the unique values of the chosen category
    category_values <- unique(bird_tracks[[input$category]])
    
    # If the category is numeric, display a sliderInput
    if (is.numeric(bird_tracks[[input$category]])) {
      min_value <- min(category_values, na.rm = TRUE)
      max_value <- max(category_values, na.rm = TRUE)
      
      sliderInput("selectedCategory", 
                  label = paste("Select", input$category), 
                  min = min_value, 
                  max = max_value, 
                  value = c(min_value, max_value), 
                  step = (max_value - min_value) / 100)
    } else {
      # If the category is non-numeric, display a selectInput
      selectInput("selectedCategory", 
                  paste("Select", input$category), 
                  choices = category_values, 
                  selected = category_values[1])
    }
  })
  
  # Create reactive filtered data
  filtered_data <- reactive({
    req(input$category, input$selectedCategory)
    
    # Handle numeric columns differently for filtering
    if (is.numeric(bird_tracks[[input$category]])) {
      bird_tracks %>%
        filter(.data[[input$category]] >= input$selectedCategory[1] & 
                 .data[[input$category]] <= input$selectedCategory[2])
    } else {
      bird_tracks %>%
        filter(.data[[input$category]] == input$selectedCategory)
    }
  })
  
  # Display text about bird count
  output$birdCountInfo <- renderText({
    req(filtered_data())
    
    bird_count <- nrow(filtered_data())
    
    if (bird_count == 0) {
      return("No birds selected")
    } else if (bird_count == 1) {
      return("Single bird selected (see details below)")
    } else {
      return(paste(bird_count, "birds selected"))
    }
  })
  
  # Conditional UI for bird information
  output$birdInfoUI <- renderUI({
    req(filtered_data())
    
    bird_count <- nrow(filtered_data())
    
    if (bird_count == 0) {
      return(NULL)
    } else if (bird_count == 1) {
      # Show bird info directly if only one bird is selected
      return(tagList(
        h4("Bird Information"),
        verbatimTextOutput("singleBirdInfo")
      ))
    } else if (!is.null(selected_bird_id())) {
      # Show info for the clicked bird
      return(tagList(
        h4(paste("Bird", selected_bird_id(), "Information")),
        verbatimTextOutput("clickedBirdInfo")
      ))
    } else {
      # Just show a hint to click on a bird
      return(tagList(
        h5("Click on a bird track for details")
      ))
    }
  })
  
  # Bird info for a single bird (when only one is selected)
  output$singleBirdInfo <- renderPrint({
    req(filtered_data())
    if (nrow(filtered_data()) != 1) return(NULL)
    
    displayBirdInfo(filtered_data()[1,])
  })
  
  # Bird info for a clicked bird
  output$clickedBirdInfo <- renderPrint({
    req(selected_bird_id(), filtered_data())
    
    bird <- filtered_data() %>%
      filter(id == selected_bird_id())

    if (nrow(bird) == 0) {
      return("Bird data not found.")
    }
    
    displayBirdInfo(bird[1,])
  })
  
  # Helper function to display bird info
  displayBirdInfo <- function(bird) {
    cat("ID:", bird$id, "\n")
    if (!is.na(bird$ring_no)) cat("Ring number:", bird$ring_no, "\n")
    if (!is.na(bird$weight)) cat("Weight:", bird$weight, "g\n")
    if (!is.na(bird$wing_length)) cat("Wing length:", bird$wing_length, "mm\n")
    if (!is.na(bird$bill_length)) cat("Bill length:", bird$bill_length, "mm\n")
    if (!is.na(bird$bill_depth)) cat("Bill depth:", bird$bill_depth, "mm\n")
    if (!is.na(bird$tarsus_length)) cat("Tarsus length:", bird$tarsus_length, "mm\n")
    if (!is.na(bird$feathers)) cat("Feathers:", bird$feathers, "\n")
    
    cat("\nTracking Info:\n")
    if (!is.na(bird$steplength)) cat("Total distance tracked:", round(bird$steplength), "meters\n")
    if (!is.na(bird$n_datapoints)) cat("Number of data points:", bird$n_datapoints, "\n")
    
    cat("\nCapture Info:\n")
    if (!is.na(bird$stage_at_capture)) cat("Stage at capture:", bird$stage_at_capture, "\n")
    if (!is.na(bird$datetime_at_capture)) cat("Date/time at capture:", bird$datetime_at_capture, "\n")
    
    cat("\nAdditional Info:\n")
    if (!is.na(bird$season)) cat("Season:", bird$season, "\n")
    if (!is.na(bird$brutzeit)) cat("Breeding time:", bird$brutzeit, "\n")
    if (!is.na(bird$year)) cat("Year:", bird$year, "\n")
    if (!is.na(bird$month)) cat("Month:", bird$month, "\n")
  }
  
  # Display a message if no data is found
  output$noDataMessage <- renderText({
    if (nrow(filtered_data()) == 0) {
      "No data found for the selected criteria."
    } else {
      ""
    }
  })
  
  # Filter the data based on user input and render the map
  output$birdMap <- renderTmap({
    # Only render map if there is data
    if (nrow(filtered_data()) == 0) {
      # Return an empty map if no data
      tmap_mode("view")
      tm_shape() + tm_view()
    } else {
      # Visualization using tmap
      tmap_mode("view")
      
      # Create map with clickable features
      tm <- tm_shape(filtered_data()) +
        tm_lines(col = "id", lwd = 2, col_alpha = 0.7, 
                 col.scale = tm_scale(values = "dark2_cyc"),
                 id = "id") +  # This enables clicking on lines
        tm_legend(show = FALSE)
      
      tm
    }
  })
  
  # Handle map clicks to select a bird
  observeEvent(input$birdMap_shape_click, {
    click_data <- input$birdMap_shape_click
    if (!is.null(click_data)) {
      # The id will be in the click data
      selected_bird_id(click_data$id)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)