ui <- fluidPage(
  titlePanel("Interactive Map and Filtered Plot"),
  leafletOutput("map"),
  plotOutput("filtered_plot")
)

server <- function(input, output) {
  # Sample data for demonstration purposes
  data <- data.frame(
    Area = c("Area 1", "Area 2", "Area 3"),
    Lat = c(40.7128, 34.0522, 41.8781),
    Long = c(-74.0060, -118.2437, -87.6298)
  )
  browser()
  
  # Initialize a reactiveValues to store clicked areas
  clicked_areas <- reactiveValues(selected = character(0))
  
  # Create the interactive map
  output$map <- renderLeaflet({
    leaflet(data = data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Long,
        lat = ~Lat,
        label = ~Area,
        group = "areas"
      )
  })
  
  # Update the clicked areas when a marker is clicked
  observeEvent(input$map_marker_click, {
    marker_id <- input$map_marker_click$id
    if (is.null(marker_id)) {
      # Handle the case where no marker is clicked (i.e., clicking on empty space)
      return()
    }
    
    if (marker_id %in% clicked_areas$selected) {
      clicked_areas$selected <- clicked_areas$selected[clicked_areas$selected != marker_id]
    } else {
      clicked_areas$selected <- c(clicked_areas$selected, marker_id)
    }
  })
  
  # Create the filtered plot based on the clicked areas
  output$filtered_plot <- renderPlot({
    if (length(clicked_areas$selected) == 0) {
      # Handle the case when no areas are selected
      plot(1, 1, type = "n", xlab = "X", ylab = "Y")
      text(1, 1, labels = "No areas selected")
    } else {
      # Your actual plotting code using filtered_data
      filtered_data <- data[data$Area %in% clicked_areas$selected, ]
      plot(1, 1, type = "n", xlab = "X", ylab = "Y")
      text(1, 1, labels = filtered_data$Area)
    }
  })
  
}

shinyApp(ui, server)
