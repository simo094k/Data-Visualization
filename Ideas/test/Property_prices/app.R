library(shinydashboard)
library(shinyWidgets)

load("PropertyData.RData") #Load environment to get the necessary data

ui <- dashboardPage(
  dashboardHeader(title = "Property Prices"), # Name of the dashboard
  
  dashboardSidebar( #If we want different "tabs"/pages, it can be done inside here
    #Further, everything we want of the left pane should be in here (eg. filters).
    
    # sidebarMenu(
    #   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    # ),
    
    br(),
    sliderTextInput( 
      inputId = "mySliderText", #The id is used to reference/connect it to some action in the server
      label = "Year:",
      choices = unique(landsdel_ejendomme_alle_aar1$Aar),
      selected = max(unique(landsdel_ejendomme_alle_aar1$Aar)), #Selected as default
      grid = FALSE, dragRange = FALSE
      
    )
  ),
  dashboardBody( #Inside here, we can design where and how the plots are shown!
    fluidRow(
      box(plotlyOutput(
        outputId = "Map_choropleth", height = 350
      ))
    ),
    fluidRow(
      box(plotOutput("plot1", height = 250))
    ),
    fluidRow(
      box(
        title = "Controls",
        sliderInput(inputId = "slider", label = "Number of observations:", min = 1, max = 100, value = 50)
      ),
      verbatimTextOutput("text")
    )
  )
)

server <- function(input, output) {
  #In the server, the fun happens. Here, we define what is actually shown.
  # It is a very good idea to keep it as structured as possible, and if the code
  # for each plot becomes to large, one can consider to place it in a seperate
  # file and refer to that file here instead.
  
  
  #Choropleth map that depends on the users input in the slider (to define what year to use)
  #browser() # This is used to debug and "step into" function calls
  dfInput <- reactive({
    landsdel_ejendomme_alle_aar1%>%
      dplyr::filter(Aar==input$mySliderText)
  })
  
  output$Map_choropleth <- renderPlotly({
    #browser()
    data <- dfInput()
    label_choice <- paste0("Developments in property prices, ", unique(data$Aar))
    s<-ggplot(data = data,
              aes(fill = Indeks, 
                  geometry = geometry)) +
      geom_sf() +
      theme_void() +
      labs(title=label_choice,
           subtitle = "Subtitle")  +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.caption = element_text(size = 8, hjust = 1)) +
      coord_fixed(ratio = 1.3)
    
    ss<-ggplotly(s)
  })
  
  
  
  # Histogram to test the interactivity of the users slider on the histogram
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$text <- renderPrint({
    data <- dfInput()
    label_choice <- paste0("Developments in property prices, ", unique(data$Aar))
    print(label_choice)
  })

}

shinyApp(ui, server)