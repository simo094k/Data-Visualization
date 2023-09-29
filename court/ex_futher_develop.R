library(shiny)
library(plotly)
library(tidyverse)

#data_path <- "C:/Users/Marc/OneDrive - Aarhus universitet/dataviz/Material_idea_generation/all_nba_data.csv"
#df <- read.csv(data_path)
#df2 <- df %>% select(c("player", "shotX", "shotY", "date", "shot_type", "distance", "made"))

df2 <- all_nba_data

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
  
  
  df_player <- df2 %>% dplyr::filter(player == "Ja Morant")  # Bogdan Bogdanović  
  df_player$made_factor <- as.factor(ifelse(df_player$made == "True", "Made", "Not made"))
  
  # Create the scatterplot
  output$scatterplot <- renderPlotly({
    plot_ly(df_player, x = ~shotX, y = ~shotY, color = ~made_factor, 
            type = "scatter", mode = "markers", source = "scatter_selected",
            customdata = ~made_factor) %>%
      layout(
        clickmode = "event+select"
      )
  })
  
  # Create the bar chart
  output$bar_chart <- renderPlotly({
    plot_ly(df_player, x = ~distance, color = ~made_factor, type = "histogram",
            source = "bar_selected", barmode = "stack", 
            yaxis = list(title = 'Count'), customdata = ~made_factor) %>%
      layout(
        clickmode = "event+select"
      )
  })
  
  # Capture selected data from the scatterplot
  scatter_selected_data <- reactive({
    selected_data <- event_data("plotly_selected", source = "scatter_selected")
    trace <- unique(selected_data$customdata)
    
    if (!is.null(selected_data)) {
      filtered_scatter_data <- df_player[df_player$shotX %in% selected_data$x &
                                         df_player$shotY %in% selected_data$y &
                                         df_player$made_factor %in% trace, ]
      return(filtered_scatter_data)
    } else {
      return(NULL)
    }
  })
  
  bar_selected_data <- reactive({
    selected_data <- event_data("plotly_selected", source = "bar_selected")
    if (!is.null(selected_data)) {
      min_dist = min(selected_data$x)
      max_dist = max(selected_data$x)
      filtered_bar_data <- df_player[min_dist < df_player$distance & 
                                       df_player$distance < max_dist, ]
      return(filtered_bar_data)
    } else {
      return(NULL)
    }
  })
  
  common_selected_data <- reactive({
    if (!is.null(scatter_selected_data())) {
      selected_data <- scatter_selected_data() # You can also use bar_selected_data() if needed
      return(selected_data)
    }
    else if (!is.null(bar_selected_data())) {
      selected_data <- bar_selected_data()
      return(selected_data)
    }

    
  })
  
  # Update the bar chart based on selected data from the scatterplot
  observe({
    
    selected_data <- common_selected_data()
    if (!is.null(scatter_selected_data())) {
      output$bar_chart <- renderPlotly({
        plot_ly(selected_data, x = ~distance, color = ~made_factor, 
                type = "histogram", source = "bar_selected", barmode = "stack", 
                yaxis = list(title = 'Count')) %>%
          layout(
            clickmode = "event+select"
          )
      })
    }
    
    else if (!is.null(bar_selected_data())) {
      output$scatterplot <- renderPlotly({
        plot_ly(selected_data, x = ~shotX, y = ~shotY, color = ~made_factor, 
                type = "scatter", mode = "markers", 
                source = "scatter_selected") %>%
          layout(
            clickmode = "event+select"
          )
      })
    }
  })
}

shinyApp(ui, server)
