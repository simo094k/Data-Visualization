library(shiny)
library(dplyr)


ui <- fluidPage(
  
  selectInput("mydropdown", "Select Species", choices = unique(datasets::iris$Species)),
  
  plotOutput("plot_1"),
  
  plotOutput("plot_2")
  
)

server <- function(input, output, session) {
  
  #Reactive
  df_filtered <- reactive({
    
    df <- filter(datasets::iris, Species == input$mydropdown)
    
    return(df)
    
  })
  
  #Plot 1
  output$plot_1 <- renderPlot({
    
    plot(df_filtered()$Petal.Length)
    
  })
  
  #Plot 2
  output$plot_2 <- renderPlot({
    
    plot(df_filtered()$Petal.Width)
    
  })
  
}

shinyApp(ui, server)