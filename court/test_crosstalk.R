library(plotly)
library(crosstalk)

# Create dummy data
set.seed(123)
data <- data.frame(
  x = rnorm(100),
  y = rnorm(100)
)

# Create scatterplot
scatter <- plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'markers') %>%
  layout(
    xaxis = list(title = "X"),
    yaxis = list(title = "Y"),
    title = "Scatterplot with Linked Histogram"
  )

# Create histogram
histogram <- plot_ly(data, x = ~x, type = 'histogram') %>%
  layout(
    xaxis = list(title = "X"),
    yaxis = list(title = "Count"),
    title = "Histogram"
  )

# Create shared data object
shared_data <- SharedData$new(data)

# Create scatterplot with histogram linked to the selected points
scatter_linked <- scatter %>%
  highlight(on = "plotly_selected") %>%
  event_register(event = "plotly_selected", source = shared_data)

# Create linked histogram
histogram_linked <- histogram %>%
  highlight(on = "plotly_selected") %>%
  event_register(event = "plotly_selected", source = shared_data)

# Create linked histogram
histogram_linked <- histogram %>%
  highlight(on = "plotly_selected") %>%
  event_register(event = "plotly_selected", source = shared_data)

# Create a subplot with scatterplot and histogram
subplot <- subplot(scatter_linked, histogram_linked, nrows = 2, heights = c(0.7, 0.3))

# Create a dashboard to display the subplot
dashboard <- bscols(subplot)

# Display the dashboard
dashboard
