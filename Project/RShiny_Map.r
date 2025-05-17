library(shiny)
library(sf)
library(leaflet)
library(geojsonsf)
library(leaflet.extras)
library(ggplot2)
library(shinythemes)
library(dplyr)


ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Bird Tracking Dashboard"),
  tabsetPanel(
    
    # Single Bird View Tab with Nested Tabs in Sidebar
    tabPanel("Single Bird View",
             sidebarLayout(
               sidebarPanel(
                 width = 5,
                 hr(),
                 
                 # Bird ID Selector with Buttons
                 selectInput("bird_id", "Select Bird ID:", choices = NULL),
                 fluidRow(
                   column(6, actionButton("prev_bird", "Previous", style = "width: 100%;")),
                   column(6, actionButton("next_bird", "Next", style = "width: 100%;"))
                 ),
                 checkboxInput("filter_seasons", "Only Birds with have Data from Multiple Seasons", value = FALSE),
                 
                 hr(),
                 
                 # Season Selection
                 uiOutput("season_toggle_ui"),
                 
                 hr(),
                 
                 # Nested Tabs in Sidebar
                 tabsetPanel(
                   tabPanel("Bird Information",
                            fluidPage(
                              h3("Selected Bird Information"),
                              htmlOutput("bird_info")
                            )
                   ),
                   tabPanel("Data Season Distribution",
                            fluidPage(
                              h3("Seasonal Distribution of Data Points"),
                              plotOutput("season_pie_chart", height = "300px")
                            )
                   ),
                   tabPanel("Home Range",
                            fluidPage(
                              h3("Home Range Size Comparison by Season"),
                              plotOutput("home_range_plot", height = "300px")
                            )
                   ),
                   tabPanel("DBScan Clusters",
                            fluidPage(
                              h3("DBScan Clusters - Number and Size Overview"),
                              plotOutput("dbscan_clusters_plot", height = "300px")
                            )
                   )
                 )
               ),
               
               # Map View - Always Visible
               mainPanel(
                 width = 7,
                 leafletOutput("map", height = "80vh")
               )
             )
    )
  )
)




server <- function(input, output, session) {
  # --- Funktionen für später: ---
  
  # Calculate 95% MCP
  calculate_95_mcp <- function(points_sf) {
    if (nrow(points_sf) < 3) return(NULL)
    
    centroid <- st_centroid(st_union(points_sf))
    points_sf <- points_sf %>% mutate(distance = st_distance(geometry, centroid))
    cutoff <- quantile(points_sf$distance, 0.95)
    points_95 <- points_sf %>% filter(distance <= cutoff)
    
    if (nrow(points_95) > 2) {
      hr95 <- st_convex_hull(st_union(points_95))
      return(hr95)
    } else {
      return(NULL)
    }
  }
  
  # Calculate Home Range Area
  calculate_home_range <- function(points_sf) {
    if (nrow(points_sf) < 3) return(NA)
    hr <- st_convex_hull(st_union(points_sf))
    return(as.numeric(st_area(hr) / 10000))  # Area in hectares
  }
  
  # --- Load Data ---
  showNotification("Loading data, please wait...", type = "message", duration = 10)
  
  bird_tracks <- st_read("preprocessing_export/bird_tracks.geojson")
  bird_data <- st_read("preprocessing_export/bird_data.geojson")
  DB_Scan_data <- st_read("preprocessing_export/DB_Scan_polygons.geojson")
  DB_Scan_matrix <- read.csv("preprocessing_export/DB_Scan_Matrix.csv", row.names = 1)
  
  # All bird IDs
  all_bird_ids <- unique(bird_tracks$id)
  bird_index <- reactiveVal(1)
  
  # Reactive value to store filtered bird IDs
  filtered_bird_ids <- reactiveVal(all_bird_ids)
  
  # Initial update of bird_id choices
  updateSelectInput(session, "bird_id", choices = all_bird_ids)
  updateSelectInput(session, "bird_id_1", choices = all_bird_ids)
  updateSelectInput(session, "bird_id_2", choices = all_bird_ids)
  
  # --- Multi-Season Birds Filtering ---
  get_multi_season_birds <- function() {
    multi_season_ids <- all_bird_ids[sapply(all_bird_ids, function(bird_id) {
      points <- bird_data[bird_data$id == bird_id, ]
      
      # Identify seasons
      seasons <- unique(
        case_when(
          points$month >= 2 & points$month <= 6 ~ "Breeding Time",
          points$month >= 7 & points$month <= 10 ~ "Harvesting Time",
          points$month %in% c(11, 12, 1) ~ "Winter",
          TRUE ~ NA_character_
        )
      )
      
      # Check for multiple seasons
      length(na.omit(seasons)) > 1
    })]
    
    return(multi_season_ids)
  }
  
  
  # Update Bird IDs based on Checkbox
  observeEvent(input$filter_seasons, {
    if (input$filter_seasons) {
      filtered_ids <- get_multi_season_birds()
    } else {
      filtered_ids <- all_bird_ids
    }
    
    # Reset index and update reactive value
    bird_index(1)
    filtered_bird_ids(filtered_ids)
    
    # Update select input with filtered IDs
    updateSelectInput(session, "bird_id", choices = filtered_ids, selected = filtered_ids[1])
  })
  
  # --- Next/Previous Button Logic ---
  update_bird_selection <- function(direction) {
    ids <- filtered_bird_ids()
    idx <- bird_index()
    
    new_index <- switch(
      direction,
      "next" = ifelse(idx < length(ids), idx + 1, 1),
      "prev" = ifelse(idx > 1, idx - 1, length(ids))
    )
    
    # Update index and select input
    bird_index(new_index)
    updateSelectInput(session, "bird_id", selected = ids[new_index])
  }
  
  # Next Button
  observeEvent(input$next_bird, {
    update_bird_selection("next")
  })
  
  # Previous Button
  observeEvent(input$prev_bird, {
    update_bird_selection("prev")
  })
  
  # Update index when bird_id is manually changed
  observeEvent(input$bird_id, {
    ids <- filtered_bird_ids()
    index <- which(ids == input$bird_id)
    if (length(index) > 0) bird_index(index)
  })

  observeEvent(input$bird_id_2, {
    # Nur bird_id_1 aktualisieren, wenn sich bird_id_2 ändert
    if (!is.null(input$bird_id_2)) {
      updateSelectInput(session, "bird_id_1", choices = setdiff(bird_ids, input$bird_id_2))
    }
  }, ignoreInit = TRUE)
  
  
  # Dynamische Auswahl für Season-Filter
  output$season_toggle_ui <- renderUI({
    req(input$bird_id)
    
    # Daten für den ausgewählten Vogel filtern
    bird_months <- bird_data[bird_data$id == input$bird_id, "month"]
    
    # Extrahiere die Spalte als Vektor und konvertiere zu numerisch
    bird_months <- unlist(bird_months)
    bird_months <- as.numeric(bird_months)
    
    # Nur gültige Monate behalten
    bird_months <- bird_months[!is.na(bird_months)]
    
    season_choices <- list("Full Dataset" = "full_season")
    
    if (any(bird_months >= 2 & bird_months <= 6)) {
      season_choices[["Breeding Time"]] <- "breeding_season"
    }
    if (any(bird_months >= 7 & bird_months <= 10)) {
      season_choices[["Harvesting Time"]] <- "harvesting_season"
    }
    if (any(bird_months %in% c(11, 12, 1))) {
      season_choices[["Winter"]] <- "winter_season"
    }
    
    radioButtons("season_toggle", "Available Seasons:", choices = season_choices)
  })
  
  
output$bird_info <- renderUI({
    find_related_birds <- function(matrix_data, bird_id) {
      if (!(bird_id %in% rownames(matrix_data))) {
        return("None")
      }
      
      # Finde die Zeile, die der bird_id entspricht
      bird_row <- matrix_data[as.character(bird_id), ]
      
      # Extrahiere die IDs der ähnlichen Vögel (Spalten mit Wert 1)
      similar_birds <- colnames(matrix_data)[which(bird_row == 1)]
      
      # Formatierung der Ausgabe
      similar_birds <- sub("^X", "", similar_birds)
      
      return(similar_birds)
    }
    
    req(input$bird_id)
    selected_points <- bird_data[bird_data$id == input$bird_id, ]
    related_birds <- find_related_birds(DB_Scan_matrix, input$bird_id)
    
    if (nrow(selected_points) > 0) {
      bird_info <- paste0(
        "<table style='width:100%; border-collapse: collapse;'>",
        "<tr><th style='text-align:left; padding: 4px; background-color: #f2f2f2;'>Attribute</th><th style='text-align:left; padding: 4px; background-color: #f2f2f2;'>Value</th></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Bird ID:</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", input$bird_id, "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Number of Points:</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", nrow(selected_points), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Date Range:</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", 
        format(min(selected_points$datetime, na.rm = TRUE), "%Y-%m-%d"), " - ", format(max(selected_points$datetime, na.rm = TRUE), "%Y-%m-%d"), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Median Time Diff.:</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", 
        -1 * round(median(selected_points$timediff, na.rm = TRUE), 5), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Altitude Range (m):</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", 
        min(selected_points$altitude, na.rm = TRUE), " - ", max(selected_points$altitude, na.rm = TRUE), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Median Step Length (m):</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", 
        round(median(selected_points$steplength, na.rm = TRUE), 2), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Weight (g):</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", unique(selected_points$weight), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Wing Length (cm):</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", unique(selected_points$wing_length), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Bill Depth (cm):</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", unique(selected_points$bill_depth), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Bill Length (cm):</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", unique(selected_points$bill_length), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Tarsus Length (cm):</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", unique(selected_points$tarsus_length), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Stage at Capture:</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", unique(selected_points$stage.at.capture), "</td></tr>",
        "<tr><td style='padding: 4px; border-bottom: 1px solid #ddd;'>Birds (ID) with overlapping clusters:</td><td style='padding: 4px; border-bottom: 1px solid #ddd;'>", 
        paste(related_birds[1:10], collapse = ", "), "</td></tr>",
        "</table>"
      )
      HTML(bird_info)
    } else {
      HTML("<em>No data available for the selected bird.</em>")
    }
  })
  

  # Pie Chart of Seasonal Distribution
  output$season_pie_chart <- renderPlot({
    req(input$bird_id)
    
    # Filter data for the selected bird
    selected_points <- bird_data[bird_data$id == input$bird_id, ]
    
    # Check if data is available
    if (nrow(selected_points) == 0) {
      return(NULL)
    }
    
    # Assign season in one pass using dplyr
    selected_points <- selected_points %>%
      mutate(season = case_when(
        month >= 2 & month <= 6 ~ "Breeding Time",
        month >= 7 & month <= 10 ~ "Harvesting Time",
        month %in% c(11, 12, 1) ~ "Winter",
        TRUE ~ "Unknown"
      ))
    
    # Aggregate counts by season
    season_data <- selected_points %>%
      group_by(season) %>%
      summarise(count = n(), .groups = "drop") %>%
      filter(season != "Unknown") %>%
      arrange(desc(season)) %>%
      mutate(
        ypos = cumsum(count) - 0.5 * count
      )
    
    # Plot the pie chart
    ggplot(season_data, aes(x = "", y = count, fill = season)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = count, y = ypos), color = "black", size = 5) +
      theme_minimal() +
      theme(
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        panel.grid = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 12)
      ) +
      labs(fill = "Season") +
      scale_fill_brewer(palette = "Set3")
  })
  
  # DBScan Clusters - Number and Size Overview Plot in Hektar (Dynamisch wie Home Range Plot)
  output$dbscan_clusters_plot <- renderPlot({
    req(input$bird_id, input$season_toggle)
    
    # Daten für den ausgewählten Vogel
    selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id, ]
    
    # Überprüfen, ob Daten vorhanden sind
    if (nrow(selected_dbscan) == 0) {
      plot.new()
      text(0.5, 0.5, "No DBScan data available", cex = 1.5)
      return()
    }
    
    # Definiere die Saisons und deren Filter
    season_mapping <- list(
      "full_season" = "All Seasons",
      "breeding_season" = "Breeding Time",
      "harvesting_season" = "Harvesting Time",
      "winter_season" = "Winter"
    )
    
    # Ausgewählte Saison und zugehöriger Label
    selected_season_label <- season_mapping[[input$season_toggle]]
    
    # Clustergrößen in Hektar für jede Saison berechnen
    cluster_sizes <- selected_dbscan %>%
      mutate(season_label = case_when(
        season == "Breeding Time" ~ "Breeding Time",
        season == "Harvesting Time" ~ "Harvesting Time",
        season == "Winter" ~ "Winter",
        TRUE ~ "All Seasons"
      )) %>%
      group_by(season_label) %>%
      summarise(total_area = sum(st_area(geometry)) / 10000, .groups = "drop")
    
    # Sicherstellen, dass alle Saisons vorhanden sind (mit 0 füllen, falls leer)
    all_seasons <- c("All Seasons", "Breeding Time", "Harvesting Time", "Winter")
    cluster_sizes <- merge(
      data.frame(season_label = all_seasons), 
      cluster_sizes, 
      by = "season_label", 
      all.x = TRUE
    )
    
    # Fehlende Werte mit 0 füllen und als numerischer Vektor konvertieren
    cluster_sizes$total_area <- as.numeric(ifelse(is.na(cluster_sizes$total_area), 0, cluster_sizes$total_area))
    
    # Farben aus Set3
    colors <- RColorBrewer::brewer.pal(n = 4, name = "Set3")
    
    # Matplotlib Plotting
    par(mfrow = c(1, 1), mar = c(5, 5, 4, 1), cex.main = 1.2, cex.lab = 1.1)
    
    # Barchart - Clustergrößenvergleich in Hektar
    barplot(
      height = as.numeric(cluster_sizes$total_area),   # Sicherstellen, dass es ein numerischer Vektor ist
      names.arg = cluster_sizes$season_label,
      col = colors,
      xlab = "Season",
      ylab = "Total Cluster Size (ha)",
      las = 2,
      cex.names = 0.8
    )
  })
    
  
  output$home_range_plot <- renderPlot({
    req(input$bird_id)
    
    # Data for selected bird
    selected_points <- bird_data[bird_data$id == input$bird_id, ]
    if (nrow(selected_points) < 3) return(NULL)
    
    # Filter by season
    breeding_points <- selected_points[selected_points$month >= 2 & selected_points$month <= 6, ]
    harvesting_points <- selected_points[selected_points$month >= 7 & selected_points$month <= 10, ]
    winter_points <- selected_points[selected_points$month %in% c(11, 12, 1), ]
    
    # Calculate Home Range Sizes
    hr_sizes <- data.frame(
      Season = c("Breeding Time", "Harvesting Time", "Winter"),
      Area = c(
        calculate_home_range(breeding_points),
        calculate_home_range(harvesting_points),
        calculate_home_range(winter_points)
      )
    )
    
    # Remove NA values for empty seasons
    hr_sizes <- hr_sizes[!is.na(hr_sizes$Area), ]
    
    # Plot
    ggplot(hr_sizes, aes(x = Season, y = Area, fill = Season)) +
      geom_bar(stat = "identity", width = 0.6) +
      scale_fill_brewer(palette = "Set3") +
      labs(title = "Home Range Size Comparison by Season", y = "Area (ha)", x = "") +
      theme_minimal() +
      theme(
        legend.position = "none",
        text = element_text(size = 14)
      )
  })
  
  
  
  # Initialisiere die Karte nur einmal
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 9.581463104990185, lat = 46.81053552453712, zoom = 12) %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri Satellite") %>%
      addScaleBar(position = "bottomleft")
  })
  
  observe({
    req(input$bird_id)
    
    # Fallback-Wert für `season_toggle`, falls nicht gesetzt
    season_toggle <- ifelse(is.null(input$season_toggle), "full_season", input$season_toggle)
    
    # Daten filtern
    selected_data <- bird_tracks[bird_tracks$id == input$bird_id, ]
    selected_points <- bird_data[bird_data$id == input$bird_id, ]
    selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id, ]
    
    centroid_coords <- st_centroid(st_union(selected_points))
    centroid_coords <- st_coordinates(centroid_coords)
    bbox <- st_bbox(selected_points)
    bbox <- as.list(bbox)

    season_title <- switch(season_toggle,
                           "breeding_season" = "Breeding Season",
                           "harvesting_season" = "Harvesting Season",
                           "winter_season" = "Winter Season",
                           "full_season" = "Full Dataset")
    
    # Season Filter
    if (season_toggle == "breeding_season") {
      selected_points <- selected_points[selected_points$month >= 2 & selected_points$month <= 6, ]
      selected_data <- selected_data[selected_data$month >= 2 & selected_data$month <= 6, ]
      selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id & DB_Scan_data$season == "Breeding Time", ]
      
    } else if (season_toggle == "harvesting_season") {
      selected_data <- selected_data[selected_data$month >= 7 & selected_data$month <= 10, ]
      selected_points <- selected_points[selected_points$month >= 7 & selected_points$month <= 10, ]
      selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id & DB_Scan_data$season == "Harvesting Time", ]
      
    } else if (season_toggle == "winter_season") {
      selected_data <- selected_data[selected_data$month %in% c(11, 12, 1), ]
      selected_points <- selected_points[selected_points$month %in% c(11, 12, 1), ]
      selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id & DB_Scan_data$season == "Winter", ]
      
    } else {
      selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id & DB_Scan_data$season == "All Seasons", ]
    }
    
    # Home Range berechnen
    mbp <- calculate_95_mcp(selected_points)
    
    # Überprüfe Home Range
    if (!is.null(mbp)) {
      cat("Home Range vorhanden\n")
    } else {
      cat("Kein Home Range berechnet\n")
    }
    
    # Leaflet Proxy verwenden, um die Karte zu aktualisieren
    legend_colors <- c("#FF5733", "#1E90FF")
    legend_labels <- c("Points", "Track")
    
    
    map <- leafletProxy("map") %>%
      clearGroup("Points") %>%
      clearGroup("Track") %>%
      clearGroup("Home Range") %>%
      clearGroup("DBScan Clusters") %>%
      removeControl("map_title") %>%
      addControl(
        html = paste0("<div style='font-size: 16px; font-weight: bold; padding: 5px; background-color: rgba(255, 255, 255, 0.8); border-radius: 5px;'><b>View Mode:</b> ", season_title, "</div>"), 
        position = "topleft", 
        layerId = "map_title"
      ) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Esri Satellite"),
        overlayGroups = c("Points", "Track", "Home Range", "DBScan Clusters"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomleft"
      ) %>%
      addPolylines(data = selected_data, color = "#1E90FF", weight = 2, group = "Track") %>%
      addCircleMarkers(data = selected_points, color = "#FF5733", radius = 1, group = "Points") %>%
      fitBounds(
        lng1 = bbox$xmin,
        lat1 = bbox$ymin,
        lng2 = bbox$xmax,
        lat2 = bbox$ymax
      )
    
    # Conditionally add Home Range
    if (!is.null(mbp) && !st_is_empty(mbp)) {
      map <- map %>% addPolygons(data = mbp, color = "#FF5733", weight = 2, fillOpacity = 0.2, group = "Home Range")
      legend_colors <- c(legend_colors, "#FF5733")
      legend_labels <- c(legend_labels, "Home Range")
    }
    
    # Conditionally add DBScan Clusters
    if (nrow(selected_dbscan) > 0) {
      map <- map %>% addPolygons(data = selected_dbscan, color = "green", weight = 2, fillOpacity = 0.4, group = "DBScan Clusters")
      legend_colors <- c(legend_colors, "green")
      legend_labels <- c(legend_labels, "DBScan Clusters")
    }
    
    map %>%
      addLegend(position = "bottomright", 
                colors = legend_colors, 
                labels = legend_labels, 
                title = "Legend",
                layerId = "legend")

    
  })
  
  
  
  output$compare_map <- renderLeaflet({
    req(input$bird_id_1, input$bird_id_2)
    
    # Daten filtern
    bird1_data <- bird_tracks[bird_tracks$id == input$bird_id_1, ]
    bird2_data <- bird_tracks[bird_tracks$id == input$bird_id_2, ]
    bird1_points <- bird_data[bird_data$id == input$bird_id_1, ]
    bird2_points <- bird_data[bird_data$id == input$bird_id_2, ]
    bird1_selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id_1, ]
    bird2_selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id_2, ]
    
    # Home Ranges von den Points berechnen
    mbp1 <- calculate_95_mcp(bird1_points)
    mbp2 <- calculate_95_mcp(bird2_points)

    # Überschneidung zwischen den Birds berechnen
    intersection <- NULL
    if (!is.null(mbp1) && !is.null(mbp2)) {
      intersection <- st_intersection(mbp1, mbp2)
    }
    has_intersection <- !is.null(intersection) && length(intersection) > 0 && !st_is_empty(intersection)
    
    # Überschneidung zwischen den Birds berechnen
    intersection_db <- NULL
    if (!is.null(bird1_selected_dbscan) && !is.null(bird2_selected_dbscan)) {
      intersection_db <- st_intersection(bird1_selected_dbscan, bird2_selected_dbscan)
    }
    has_intersection_db <- !is.null(intersection_db) && length(intersection_db) > 0 && !st_is_empty(intersection_db)
    
    
    
    # Heatmap-Daten vorbereiten
    heatmap_data <- rbind(bird1_points, bird2_points)
    heatmap_coords <- data.frame(
      lng = st_coordinates(heatmap_data)[,1],
      lat = st_coordinates(heatmap_data)[,2]
    )
    
    # Basiskarte initialisieren
    leaflet_map <- leaflet() %>%
      addTiles(group = "Base Map") %>%
      addPolylines(data = bird1_data, color = "#FF5733", weight = 1, group = "Bird 1 Track") %>%
      addPolylines(data = bird2_data, color = "#1E90FF", weight = 1, group = "Bird 2 Track")
    
    # Convex Polygon Analyse
    if (input$analysis_toggle == "convex") {
      leaflet_map <- leaflet_map %>%
        addPolygons(data = mbp1, color = "#FF5733", weight = 2, fillOpacity = 0.1, group = "Bird 1 Home Range") %>%
        addPolygons(data = mbp2, color = "#1E90FF", weight = 2, fillOpacity = 0.1, group = "Bird 2 Home Range")
    }
    
    # Intersection Analyse
    if (input$analysis_toggle == "intersection") {
      leaflet_map <- leaflet_map %>%
        addPolygons(data = bird1_selected_dbscan, color = "#FF5733", weight = 2, fillOpacity = 0.4, group = "Cluster Bird 1") %>%
        addPolygons(data = bird2_selected_dbscan, color = "#1E90FF", weight = 2, fillOpacity = 0.4, group = "Cluster Bird 2")
    }
    
    # Intersection anzeigen, wenn aktiviert und vorhanden
    if (input$analysis_toggle == "convex" && has_intersection) {
      leaflet_map <- leaflet_map %>%
        addPolygons(data = intersection, color = "#32CD32", weight = 2, fillOpacity = 0.5, group = "Intersection")
    } else if (input$analysis_toggle == "convex") {
      leaflet_map <- leaflet_map %>%
        addControl(html = paste0(
          "<strong style='color:red;'>No intersection of the Home Ranges between bird ",
          input$bird_id_1, " and bird ", input$bird_id_2, "</strong>"), position = "topleft")
    }
    
    if (input$analysis_toggle == "intersection" && !is.na(has_intersection_db) && has_intersection_db) {
      leaflet_map <- leaflet_map %>%
        addPolygons(data = intersection_db, color = "#32CD32", weight = 2, fillOpacity = 0.5, group = "Intersection")
    } else if (input$analysis_toggle == "intersection") {
      leaflet_map <- leaflet_map %>%
        addControl(html = paste0(
          "<strong style='color:red;'>No intersection of the DBScan Clusters between bird ",
          input$bird_id_1, " and bird ", input$bird_id_2, "</strong>"), position = "topleft")
    }
    
    # Heatmap zuletzt hinzufügen, wenn aktiviert
    if (input$analysis_toggle == "heatmap") {
      leaflet_map <- leaflet_map %>%
        addHeatmap(data = heatmap_coords, lng = ~lng, lat = ~lat,
                   blur = 20, max = 0.5, radius = 15, group = "Heatmap")
    }
    
    # Dynamische Legende erstellen
    legend_groups <- c("Bird 1 Track", "Bird 2 Track")
    legend_colors <- c("#FF5733", "#1E90FF")
    legend_labels <- c("Bird 1 Track", "Bird 2 Track")
    
    if (input$analysis_toggle == "convex") {
      legend_groups <- c(legend_groups, "Bird 1 Polygon", "Bird 2 Polygon")
      legend_colors <- c(legend_colors, "#FF5733", "#1E90FF")
      legend_labels <- c(legend_labels, "Bird 1 Polygon", "Bird 2 Polygon")
    
      if (has_intersection) {
        legend_groups <- c(legend_groups, "Intersection")
        legend_colors <- c(legend_colors, "#32CD32")
        legend_labels <- c(legend_labels, "Intersection")
      }
    }
    
    if (input$analysis_toggle == "intersection") {
      legend_groups <- c(legend_groups, "Cluster Bird 1", "Cluster Bird 2")
      legend_colors <- c(legend_colors, "#FF5733", "#1E90FF")
      legend_labels <- c(legend_labels, "Cluster Bird 1", "Cluster Bird 2")
      
      if (!is.na(has_intersection_db) && has_intersection_db) {
        legend_groups <- c(legend_groups, "Intersection")
        legend_colors <- c(legend_colors, "#32CD32")
        legend_labels <- c(legend_labels, "Intersection")
      }
    }
    
    # Legende und Steuerung hinzufügen
    leaflet_map %>%
      addLayersControl(baseGroups = c("Base Map"),
                       overlayGroups = c("Heatmap", legend_groups),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addLegend(position = "bottomright", colors = legend_colors,
                labels = legend_labels, title = "Legend") %>%
      addScaleBar(position = "bottomleft")
  })
  
  output$analysis_plot <- renderPlot({
    req(input$bird_id_1, input$bird_id_2, input$analysis_toggle)
    
    bird1_points <- bird_data[bird_data$id == input$bird_id_1, ]
    bird2_points <- bird_data[bird_data$id == input$bird_id_2, ]
    
    mbp1 <- calculate_95_mcp(bird1_points)
    mbp2 <- calculate_95_mcp(bird2_points)
    
    intersection <- NULL
    
    if (!is.null(mbp1) && !is.null(mbp2)) {
      intersection <- st_intersection(mbp1, mbp2)
    }
    
    if (input$analysis_toggle == "convex") {
      sizes <- c(
        if (!is.null(mbp1)) st_area(mbp1)/10000 else units::set_units(0, ha),
        if (!is.null(mbp2)) st_area(mbp2)/10000 else units::set_units(0, ha),
        if (!is.null(intersection)) st_area(intersection)/10000 else units::set_units(0, ha)
      )
      
      if (length(sizes) == 2) {
        sizes <- c(sizes, units::set_units(0, ha))
      }
      
      
      labels <- c("Bird 1", "Bird 2", "Intersection")
      
      ggplot(data.frame(Size = as.numeric(sizes), Label = labels), aes(x = Label, y = Size)) +
        geom_bar(stat = "identity", fill = c("#FF5733", "#1E90FF", "#32CD32")) +
        labs(title = "Home Ranges Overview", y = "Area (hectar)", x = "") +
        theme_minimal()
      
    } else if (input$analysis_toggle == "intersection") {
      bird1_clusters <- DB_Scan_data[DB_Scan_data$id == input$bird_id_1, ]
      bird2_clusters <- DB_Scan_data[DB_Scan_data$id == input$bird_id_2, ]
      intersecting_clusters <- st_intersection(bird1_clusters, bird2_clusters)
      sizes <- c(nrow(bird1_clusters), nrow(bird2_clusters), nrow(intersecting_clusters))
      labels <- c("Bird 1", "Bird 2", "Intersection")
      ggplot(data.frame(Size = sizes, Label = labels), aes(x = Label, y = Size)) +
        geom_bar(stat = "identity", fill = c("#FF5733", "#1E90FF", "#32CD32")) +
        labs(title = "DB Cluster Overview", y = "Number of Clusters", x = "") +
        theme_minimal()
    }
  })
  
}

shinyApp(ui = ui, server = server)