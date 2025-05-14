library(shiny)
library(sf)
library(leaflet)
library(geojsonsf)
library(leaflet.extras)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  tabsetPanel(
    tabPanel("Single Bird View",
             sidebarLayout(
               sidebarPanel(
                 width = 5,
                 selectInput("bird_id", "Select Bird ID:", choices = NULL),
                 HTML("<h3>Selected Bird Information:</h3>"),
                 htmlOutput("bird_info")
               ),
               mainPanel(
                 width = 7,
                 leafletOutput("map", height = "80vh")
               )
             )
    ),
    tabPanel("Compare Birds",
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 selectInput("bird_id_1", "Select Bird 1 ID:", choices = NULL, selected = ".458"),
                 selectInput("bird_id_2", "Select Bird 2 ID:", choices = NULL, selected = "7314"),
                 radioButtons("analysis_toggle", "Select Analysis:",
                              choices = list("Heatmap" = "heatmap",
                                             "Convex Polygon" = "convex",
                                             "DBScan Analysis" = "intersection"),
                              selected = "convex"),
                 plotOutput("analysis_plot", height = "300px")
               ),
               mainPanel(
                 width = 8,
                 leafletOutput("compare_map", height = "80vh")
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
  DB_Scan_matrix <- read.csv("preprocessing_export/DB_Scan_Matrix.csv", row.names = 1)
  
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
      bird_info <- list(
        paste("<strong>Bird ID:</strong> ", input$bird_id),
        paste("<strong>Number of Points:</strong> ", nrow(selected_points)),
        paste("<strong>Date Range:</strong> ", format(min(selected_points$datetime, na.rm = TRUE), "%Y-%m-%d"), " - ", format(max(selected_points$datetime, na.rm = TRUE), "%Y-%m-%d")),
        paste("<strong>Median Time Diff.:</strong> ", -1 * round(median(selected_points$timediff, na.rm = TRUE), 5)),
        paste("<strong>Altitude Range (m):</strong> ", min(selected_points$altitude, na.rm = TRUE), " - ", max(selected_points$altitude, na.rm = TRUE)),
        paste("<strong>Median Step Length (m):</strong> ", round(median(selected_points$steplength, na.rm = TRUE), 2)),
        paste("<strong>Weight (g):</strong> ", unique(selected_points$weight)),
        paste("<strong>Wing Length (cm):</strong> ", unique(selected_points$wing_length)),
        paste("<strong>Bill Depth (cm):</strong> ", unique(selected_points$bill_depth)),
        paste("<strong>Bill Length (cm):</strong> ", unique(selected_points$bill_length)),
        paste("<strong>Tarsus Length (cm):</strong> ", unique(selected_points$tarsus_length)),
        paste("<strong>Stage at Capture:</strong> ", unique(selected_points$stage.at.capture)),
        paste("<strong>Birds with Overlap in Clusters (Bird ID):</strong><br>", paste(related_birds, collapse = ", "))
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
    

    mbp <- if (nrow(selected_points) > 2) st_convex_hull(st_union(selected_points)) else NULL
    
    leaflet_map <- leaflet() %>%
      addTiles(group = "Base Map") %>%
      addPolygons(data = mbp, color = "#FF5733", weight = 2, fillOpacity = 0.1, group = "Bounding Polygon") %>%
      addPolylines(data = selected_data, color = "#1E90FF", weight = 2, group = "Track") %>%
      addCircleMarkers(data = selected_points, color = "#FF5733", radius = 1, group = "Points") %>%
      
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
    bird1_selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id_1, ]
    bird2_selected_dbscan <- DB_Scan_data[DB_Scan_data$id == input$bird_id_2, ]
    
    # Bounding Polygons von den Points berechnen
    mbp1 <- if (nrow(bird1_points) > 2) st_convex_hull(st_union(bird1_points)) else NULL
    mbp2 <- if (nrow(bird2_points) > 2) st_convex_hull(st_union(bird2_points)) else NULL
    
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
        addPolygons(data = mbp1, color = "#FF5733", weight = 2, fillOpacity = 0.1, group = "Bird 1 Polygon") %>%
        addPolygons(data = mbp2, color = "#1E90FF", weight = 2, fillOpacity = 0.1, group = "Bird 2 Polygon")
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
          "<strong style='color:red;'>No intersection of the convex polygons between bird ",
          input$bird_id_1, " and bird ", input$bird_id_2, "</strong>"), position = "topleft")
    }
    
    # Intersection Cluster anzeigen, wenn aktiviert und vorhanden
    print(has_intersection_db)
    
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
    
    mbp1 <- if (nrow(bird1_points) > 2) st_convex_hull(st_union(st_as_sf(bird1_points))) else NULL
    mbp2 <- if (nrow(bird2_points) > 2) st_convex_hull(st_union(st_as_sf(bird2_points))) else NULL
    
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
        labs(title = "Bounding Polygons Overview", y = "Area (hectar)", x = "") +
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