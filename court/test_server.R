library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Add any input controls if needed
    ),
    mainPanel(
      plotlyOutput("scatterplot"),
      plotlyOutput("bar_chart")
    )
  )
)

server <- function(input, output, session) {
  # Define your data (replace this with your actual data)
  data <- data.frame(
    x_data = 1:10,
    y_data = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3),
    x_variable = LETTERS[1:10],
    y_variable = c(2, 4, 1, 7, 2, 5, 8, 3, 6, 2)
  )
  
  # Create the scatterplot
  output$scatterplot <- renderPlotly({
    plot_ly(data, x = ~x_data, y = ~y_data, type = "scatter", mode = "markers",
            source = "scatter_selected") %>%
      layout(
        clickmode = "event+select"
      )
  })
  
  # Create the bar chart
  output$bar_chart <- renderPlotly({
    plot_ly(data, x = ~x_variable, y = ~y_variable, type = "bar",
            source = "bar_selected") %>%
      layout(
        clickmode = "event+select"
      )
  })
  
  # Capture selected data from the scatterplot
  scatter_selected_data <- reactive({
    selected_data <- event_data("plotly_selected", source = "scatter_selected")
    if (!is.null(selected_data)) {
      filtered_scatter_data <- data[data$x_data %in% selected_data$x, ]
      return(filtered_scatter_data)
    } else {
      return(NULL)
    }
  })
  
  # Update the bar chart based on selected data from the scatterplot
  observe({
    if (!is.null(scatter_selected_data())) {
      selected_data <- scatter_selected_data()
      output$bar_chart <- renderPlotly({
        plot_ly(selected_data, x = ~x_variable, y = ~y_variable, type = "bar",
                source = "bar_selected") %>%
          layout(
            clickmode = "event+select"
          )
      })
    }
  })
}

shinyApp(ui, server)
