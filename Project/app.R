library(shinydashboard)
library(shinyWidgets)
#library(plotly)
library(ggplot2)
library(magrittr)
library(leaflet)
library(leafletDK)

#load("PropertyData.RData") #Load environment to get the necessary data


data_without_amt_houses_sold <- readr::read_csv("data/data_without_amt_houses_sold.csv")

samlet_data <- landsdel_house_samlet %>% 
  dplyr::mutate(MetricName=dplyr::case_when(Metric == "Index" ~ "Index",
                                            Metric == "pct_q" ~ "Change compared to the previous quarter (pct)",
                                            Metric == "pct_y" ~ "Change compared to the same quarter of the previous year (pct)"))

#write.csv(x = landsdel_house_samlet, file = "landsdel_house_samlet")

ui <- dashboardPage(
  dashboardHeader(title = "Property Prices", 
                  titleWidth = 300), # Name of the dashboard
  
  dashboardSidebar( #If we want different "tabs"/pages, it can be done inside here
    #Further, everything we want of the left pane should be in here (eg. filters).
    
    # sidebarMenu(
    #   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    # ),
    shinyjs::useShinyjs(),
    br(),
    sliderTextInput( 
      inputId = "mySliderText", #The id is used to reference/connect it to some action in the server
      label = "Year",
      choices = unique(samlet_data$Time),
      selected = max(unique(samlet_data$Time)), #Selected as default
      grid = FALSE, dragRange = FALSE
    ),
    br(),
    selectInput(inputId = "propType", 
                label = "Propterty type", 
                choices = unique(samlet_data$Type), 
                selected = samlet_data$Type == "House" 
    ),
    selectInput(inputId = "metricType", 
                label = "Metric type", 
                choices =  rlang::set_names(unique(samlet_data$Metric), unique(samlet_data$MetricName)), 
                selected = "pct_y" 
    ),width = 300,collapsed = F
    
  ),
  dashboardBody( #Inside here, we can design where and how the plots are shown!
    fluidRow(
      
      splitLayout(style = "border: 1px solid silver:", cellWidths = c("65%","35%"), 
                  leaflet::leafletOutput(
                    outputId = "Map_choropleth", height = 750#, width = "65%"
                  ),
                  plotly::plotlyOutput("plot1", height = 300) 
      )
      
      
      # box(leaflet::leafletOutput(
      #   outputId = "Map_choropleth", height = 500, width = "65%"
      # ),width = "75%"),
      # box(plotOutput(outputId = "plot2"), height = 500, width = "35%")
    )
    # ,
    # fluidRow(
    #   box(
    #     title = "Controls",
    #     sliderInput(inputId = "slider", label = "Number of observations:", min = 1, max = 100, value = 50)
    #   ),
    #   verbatimTextOutput("text")
    # )
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
    samlet_data%>%
      dplyr::filter(Area != "Hele landet" & Time==input$mySliderText & Type == input$propType & Metric == input$metricType)
  })
  
  output$Map_choropleth <- leaflet::renderLeaflet({
    #browser()
    data <- dfInput()
    # pal <- leaflet::colorNumeric(
    #   palette = "Blues",
    #   domain = samlet_data%>%
    #                    dplyr::filter(Area != "Hele landet")%>%dplyr::select(Value)
    # )

 
      leafletDK::ruralDK(value = "Value",
                       id = "Area", 
                       data = data, 
                       legend = T, 
                       legendtitle = paste0("Index, ", unique(data$Time)),
                       pal = "Blues", 
                       logcol = T
                      )%>%
      leaflet.extras::setMapWidgetStyle(list(background = "white"))
      # %>%
      #   leaflet::addLegend(
      #     position = 'bottomleft',
      #     colors = pal(data$Value),
      #     labels = data$Value, opacity = 1,
      #     values = ~data$Value,
      #     title = 'An Obvious Legend'
      #   )
      
      
    # label_choice <- paste0("Developments in property prices, ", unique(data$Time))
    # ggplotly(
    #   ggplot(data = data,
    #           aes(fill = Value, 
    #               geometry = geometry,
    #               text = paste0(Area,": ", Value))) +
    #   geom_sf() +
    #   theme_void() +
    #   labs(title=label_choice,
    #        subtitle = "Subtitle")  +
    #   theme(
    #     plot.title = element_text(size = 12, hjust = 0.5),
    #     plot.subtitle = element_text(size = 10, hjust = 0.5),
    #     plot.caption = element_text(size = 8, hjust = 1),
    #     legend.position="bottom") +
    #   coord_fixed(ratio = 1.3)
    #   )
  })
  
  
  
  # Histogram to test the interactivity of the users slider on the histogram
  df_linechart <- reactive({
    
    samlet_data%>%
      dplyr::filter(Area =="Hele landet" & Metric == input$metricType)%>%
      dplyr::mutate(Aar1 = Time)%>%
      tidyr::separate(Time, into = c("Year", "Quarter"), sep = "K")%>%
      dplyr::mutate(Quarter = as.numeric(Quarter),
                    Date = lubridate::ymd(paste0(Year, "-01-01")) + months(3) * (Quarter - 1))
  })
  
  output$plot1 <- plotly::renderPlotly({
    data <- df_linechart()
    plotly::ggplotly(ggplot2::ggplot(data = data, 
                                     mapping = aes(x = Date, y = Value, color = Area#, text = paste0("Date: ", Aar1)
                                                   ))+
      geom_line(size=1.5) +
        ggplot2::ggtitle(unique(data$MetricName)) +
      ggplot2::scale_colour_manual(values = "#6495ed") + 
        ggthemes::theme_hc()+
        theme(legend.position="none", 
              axis.text.x=element_blank(),
              axis.text.y=element_blank()))
  })
  
  
  
  # output$text <- renderPrint({
  #   data <- dfInput()
  #   label_choice <- paste0("Developments in property prices, ", unique(data$Time))
  #   print(label_choice)
  # })

}

shinyApp(ui, server)