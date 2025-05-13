library(shiny)
library(sf)
library(leaflet)
library(geojsonsf)
library(leaflet.extras)

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
                 leafletOutput("map")
               )
             )
    ),
    tabPanel("Compare Birds",
             sidebarLayout(
               sidebarPanel(
                 selectInput("bird_id_1", "Select Bird 1 ID:", choices = NULL, selected = ".458"),
                 selectInput("bird_id_2", "Select Bird 2 ID:", choices = NULL, selected = "7314"),
                 checkboxInput("toggle_heatmap", "Show Heatmap", value = FALSE)
               ),
               mainPanel(
                 leafletOutput("compare_map")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  showNotification("Loading data, please wait...", type = "message", duration = 10)
  
  bird_tracks <- st_read("preprocessing_export/bird_tracks.geojson")
  bird_data <- st_read("preprocessing_export/bird_data.geojson")
  DB_Scan_data <- st_read("preprocessing_export/DB_Scan_polygons.geojson")
  
  bird_ids <- unique(bird_tracks$id)
  
  updateSelectInput(session, "bird_id", choices = bird_ids)
  updateSelectInput(session, "bird_id_1", choices = bird_ids)
  updateSelectInput(session, "bird_id_2", choices = bird_ids)

  observeEvent(input$bird_id_2, {
    # Nur bird_id_1 aktualisieren, wenn sich bird_id_2 ändert
    if (!is.null(input$bird_id_2)) {
      updateSelectInput(session, "bird_id_1", choices = setdiff(bird_ids, input$bird_id_2))
    }
  }, ignoreInit = TRUE)
  
  output$bird_info <- renderUI({
    req(input$bird_id)
    selected_points <- bird_data[bird_data$id == input$bird_id, ]

    if (nrow(selected_points) > 0) {
      bird_info <- list(
        paste("<strong>Bird ID:</strong> ", input$bird_id),
        paste("<strong>Number of Points:</strong> ", nrow(selected_points)),
        paste("<strong>Date Range:</strong> ", format(min(selected_points$datetime, na.rm = TRUE), "%Y-%m-%d"), " - ", format(max(selected_points$datetime, na.rm = TRUE), "%Y-%m-%d")),
        paste("<strong>Median Time Diff.</strong> ", -1 * round(median(selected_points$timediff, na.rm = TRUE),2)),
        paste("<strong>Altitude Range (m):</strong> ", min(selected_points$altitude, na.rm = TRUE), " - ", max(selected_points$altitude, na.rm = TRUE)),
        paste("<strong>Median Step Length (m):</strong> ", round(median(selected_points$steplength, na.rm = TRUE)),2),
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
  output$map <- renderLeaflet({
    req(input$bird_id)
    selected_data <- bird_tracks[bird_tracks$id == input$bird_id, ]
    selected_points <- bird_data[bird_data$id == input$bird_id, ]
    selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id, ]
    # TODO: hier geht es nicht wenn ich es in selected_dbscan integrieren möchte
    
    mbp <- if (nrow(selected_points) > 2) st_convex_hull(st_union(selected_points)) else NULL
    
    leaflet_map <- leaflet() %>%
      addTiles(group = "Base Map") %>%
      addPolygons(data = mbp, color = "#FF5733", weight = 2, fillOpacity = 0.1, group = "Bounding Polygon") %>%
      addPolylines(data = selected_data, color = "#1E90FF", weight = 2, group = "Track") %>%
      addCircleMarkers(data = selected_points, color = "#FF5733", radius = 1, group = "Points") %>%
      
      # !!!  TODO: hier geht es nicht wenn ich es in selected_dbscan integrieren möchte  !!!
      
      addPolygons(data = selected_dbscan, color = "green", weight = 2, group = "DBScan Clusters")
      
    leaflet_map %>%
      addLayersControl(
        overlayGroups = c("Points", "Track", "Polygon"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addLegend(position = "bottomright", colors = c("#FF5733", "#1E90FF","#FF5733", "green"), labels = c("Points", "Track","Bounding Polygon", "DBScan Clusters"), title = "Legend") %>%
      addScaleBar(position = "bottomleft")
  
  })

  output$compare_map <- renderLeaflet({
    req(input$bird_id_1, input$bird_id_2)
    
    # Daten filtern
    bird1_data <- bird_tracks[bird_tracks$id == input$bird_id_1, ]
    bird2_data <- bird_tracks[bird_tracks$id == input$bird_id_2, ]
    bird1_points <- bird_data[bird_data$id == input$bird_id_1, ]
    bird2_points <- bird_data[bird_data$id == input$bird_id_2, ]
    
    # Bounding Polygons berechnen
    mbp1 <- if (nrow(bird1_points) > 2) st_convex_hull(st_union(bird1_points)) else NULL
    mbp2 <- if (nrow(bird2_points) > 2) st_convex_hull(st_union(bird2_points)) else NULL
    
    # Überschneidung berechnen
    intersection <- NULL
    if (!is.null(mbp1) && !is.null(mbp2)) {
      intersection <- st_intersection(mbp1, mbp2)
    }
    has_intersection <- !is.null(intersection) && length(intersection) > 0 && !st_is_empty(intersection)
    
    # Heatmap-Daten vorbereiten
    heatmap_data <- rbind(bird1_points, bird2_points)
    heatmap_coords <- data.frame(
      lng = st_coordinates(heatmap_data)[,1],
      lat = st_coordinates(heatmap_data)[,2]
    )
    
    # Basiskarte und Vektor-Layer hinzufügen
    leaflet_map <- leaflet() %>%
      addTiles(group = "Base Map") %>%
      addPolygons(data = mbp1, color = "#FF5733", weight = 2, fillOpacity = 0.1, group = "Bird 1 Polygon") %>%
      addPolygons(data = mbp2, color = "#1E90FF", weight = 2, fillOpacity = 0.1, group = "Bird 2 Polygon") %>%
      addPolylines(data = bird1_data, color = "#FF5733", weight = 1, group = "Bird 1 Track") %>%
      addPolylines(data = bird2_data, color = "#1E90FF", weight = 1, group = "Bird 2 Track")
    
    # Überschneidung anzeigen
    if (has_intersection) {
      leaflet_map <- leaflet_map %>%
        addPolygons(data = intersection, color = "#32CD32", weight = 2, fillOpacity = 0.5, group = "Intersection")
    } else {
      leaflet_map <- leaflet_map %>%
        addControl(html = paste0(
          "<strong style='color:red;'>No intersection between ",
          input$bird_id_1, " and ", input$bird_id_2, "</strong>"), position = "topleft")
    }
    
    # Heatmap zuletzt hinzufügen, wenn aktiviert
    if (isTRUE(input$toggle_heatmap)) {
      leaflet_map <- leaflet_map %>%
        addHeatmap(data = heatmap_coords, lng = ~lng, lat = ~lat,
                   blur = 20, max = 0.5, radius = 15, group = "Heatmap")
    }
    
    # Steuerung und Legende
    leaflet_map %>%
      addLayersControl(baseGroups = c("Base Map"),
                       overlayGroups = c("Heatmap", "Bird 1 Track", "Bird 2 Track", "Bird 1 Polygon", "Bird 2 Polygon", "Intersection"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addLegend(position = "bottomright", colors = c("#FF5733", "#1E90FF", "#32CD32"),
                labels = c("Bird 1", "Bird 2", "Intersection"), title = "Legend") %>%
      addScaleBar(position = "bottomleft")
  })
}

shinyApp(ui = ui, server = server)