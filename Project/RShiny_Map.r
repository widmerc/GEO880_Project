library(shiny)
library(sf)
library(tmap)
library(geojsonsf)

ui <- fluidPage(
  titlePanel("Bird Tracks Viewer"),
  tabsetPanel(
    tabPanel("Single Bird View",
             sidebarLayout(
               sidebarPanel(
                 selectInput("bird_id", "Select Bird ID:", choices = NULL),
                 HTML("<h3>Selected Bird Information:</h3>"),
                 htmlOutput("bird_info")
               ),
               mainPanel(
                 tmapOutput("map")
               )
             )
    ),
    tabPanel("Compare Birds",
             sidebarLayout(
               sidebarPanel(
                 selectInput("bird_id_1", "Select Bird 1 ID:", choices = NULL),
                 selectInput("bird_id_2", "Select Bird 2 ID:", choices = NULL)
               ),
               mainPanel(
                 tmapOutput("compare_map")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  showNotification("Loading data, please wait...", type = "message", duration = 10)
  
  # Lade die GeoJSON-Daten
  bird_tracks <- st_read("bird_tracks.geojson")
  bird_data <- st_read("bird_data.geojson")
  
  # Aktualisiere die Auswahlmöglichkeiten basierend auf den einzigartigen IDs
  updateSelectInput(session, "bird_id", choices = unique(bird_tracks$id))
  updateSelectInput(session, "bird_id_1", choices = unique(bird_tracks$id))
  updateSelectInput(session, "bird_id_2", choices = unique(bird_tracks$id))
  
  # Single Bird View - Map
  output$map <- renderTmap({
    req(input$bird_id)
    selected_data <- bird_tracks[bird_tracks$id == input$bird_id, ]
    selected_points <- bird_data[bird_data$id == input$bird_id, ]
    
    # Bounding Polygon berechnen
    mbp <- if (nrow(selected_points) > 2) st_convex_hull(st_union(selected_points)) else NULL
    
    tmap_mode("view")
    tm_basemap("OpenStreetMap") +
      tm_shape(selected_data) +
      tm_lines(lwd = 1.5, col = "black") +
      tm_shape(selected_points) +
      tm_symbols(shape = 3, col = "red", size = 0.2) +
      if (!is.null(mbp)) {
        tm_shape(mbp) +
          tm_borders(col = "red", lwd = 2)
      } +
      tm_scale_bar(position = c("left", "bottom"), text.size = 1)
  })
  
  # Single Bird View - Bird Info
  output$bird_info <- renderUI({
    req(input$bird_id)
    selected_points <- bird_data[bird_data$id == input$bird_id, ]
    
    if (nrow(selected_points) > 0) {
      bird_info <- list(
        paste("<strong>Bird ID:</strong> ", input$bird_id),
        paste("<strong>Altitude Range (m):</strong> ", min(selected_points$altitude, na.rm = TRUE), " - ", max(selected_points$altitude, na.rm = TRUE)),
        paste("<strong>Date Range:</strong> ", format(min(selected_points$datetime, na.rm = TRUE), "%Y-%m-%d"), " - ", format(max(selected_points$datetime, na.rm = TRUE), "%Y-%m-%d")),
        paste("<strong>Weight (g):</strong> ", unique(selected_points$weight)),
        paste("<strong>Wing Length (cm):</strong> ", unique(selected_points$wing_length)),
        paste("<strong>Bill Depth (cm):</strong> ", unique(selected_points$bill_depth)),
        paste("<strong>Bill Length (cm):</strong> ", unique(selected_points$bill_length)),
        paste("<strong>Tarsus Length (cm):</strong> ", unique(selected_points$tarsus_length)),
        paste("<strong>Stage at Capture:</strong> ", unique(selected_points$stage.at.capture))
      )
      HTML(paste(bird_info, collapse = "<br>"))
    } else {
      HTML("<em>No data available for the selected bird.</em>")
    }
  })
  
  # Compare Birds - Map
  output$compare_map <- renderTmap({
    req(input$bird_id_1, input$bird_id_2)
    
    bird1_data <- bird_tracks[bird_tracks$id == input$bird_id_1, ]
    bird2_data <- bird_tracks[bird_tracks$id == input$bird_id_2, ]
    
    bird1_points <- bird_data[bird_data$id == input$bird_id_1, ]
    bird2_points <- bird_data[bird_data$id == input$bird_id_2, ]
    
    # Bounding Polygons berechnen
    mbp1 <- if (nrow(bird1_points) > 2) st_convex_hull(st_union(bird1_points)) else NULL
    mbp2 <- if (nrow(bird2_points) > 2) st_convex_hull(st_union(bird2_points)) else NULL
    
    # Überschneidung berechnen
    intersection <- if (!is.null(mbp1) && !is.null(mbp2)) {
      st_intersection(mbp1, mbp2)
    } else {
      NULL
    }
    
    has_intersection <- !is.null(intersection) && length(intersection) > 0 && !st_is_empty(intersection)
    
    tmap_mode("view")
    tm_basemap("OpenStreetMap") +
      tm_shape(bird1_data) +
      tm_lines(lwd = 1.5, col = "red") +
      tm_shape(bird2_data) +
      tm_lines(lwd = 1.5, col = "blue") +
      if (!is.null(mbp1)) {
        tm_shape(mbp1) +
          tm_borders(col = "red", lwd = 2, lty = "dashed")
      } +
      if (!is.null(mbp2)) {
        tm_shape(mbp2) +
          tm_borders(col = "blue", lwd = 2, lty = "dashed")
      } +
      if (has_intersection) {
        tm_shape(intersection) +
          tm_fill(col = "green", alpha = 0.4) +
          tm_borders(col = "green", lwd = 2)
      } +
      tm_scale_bar(position = c("left", "bottom"), text.size = 1)
  })
}

shinyApp(ui = ui, server = server)
