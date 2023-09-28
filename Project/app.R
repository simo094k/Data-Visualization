library(shinydashboard)
library(shinyWidgets)
#library(plotly)
library(ggplot2)
library(magrittr)
library(leaflet)
library(leafletDK)

#load("PropertyData.RData") #Load environment to get the necessary data


data_without_amt_houses_sold <- readr::read_csv("data/data_without_amt_houses_sold.csv")

samlet_data <- data_without_amt_houses_sold %>% 
  dplyr::mutate(Metric = dplyr::case_when(Metric == "Pct_y" | Metric == "pct_y" ~ "Pct_y",
                                           Metric == "Pct_q" | Metric == "pct_q" ~ "Pct_q", TRUE ~ Metric),
                MetricName=dplyr::case_when(Metric == "Index" ~ "Index",
                                            Metric == "Pct_q" ~ "Change compared to the previous quarter (pct)",
                                            Metric == "Pct_y" ~ "Change compared to the same quarter of the previous year (pct)"))

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
      inputId = "time", #The id is used to reference/connect it to some action in the server
      label = "Year",
      choices = unique(samlet_data$Time),
      selected = max(unique(samlet_data$Time)), #Selected as default
      grid = FALSE, dragRange = FALSE
    ),
    br(),
    selectInput(inputId = "propType", 
                label = "Propterty type", 
                choices = unique(samlet_data$Type), 
                selected = "House" 
    ),
    selectInput(inputId = "metricType", 
                label = "Metric type", 
                choices =  rlang::set_names(unique(samlet_data$Metric), unique(samlet_data$MetricName)), 
                selected = "Index" 
    ),width = 300,collapsed = F
    
  ),
  dashboardBody( #Inside here, we can design where and how the plots are shown!
    fluidRow(
      column(width = 8, 
             fluidRow(
               style = "height:100vh; width:102.5%;",
               leaflet::leafletOutput(
                 outputId = "Map_choropleth", height = 770#, width = "65%"
               )
             )
             
        
      ),
      column(width = 4,
             fluidRow(
               style = "height:200px; margin-bottom:110px;",
               plotly::plotlyOutput("linechart", height = 300)
             ),
             # fluidRow(
             #   style = "height:100px;" 
             # ),
             
             fluidRow(
               style = "height:400px;",
               plotly::plotlyOutput("barchart", height = 460)
             )
        
      )
      
      # splitLayout(style = "border: 1px solid silver:", cellWidths = c("65%","35%"), 
      #             leaflet::leafletOutput(
      #               outputId = "Map_choropleth", height = 750#, width = "65%"
      #             ),
      #             splitLayout(
      #             plotly::plotlyOutput("linechart", height = 300), plotly::plotlyOutput("barchart", height = 300), ellArgs = list(style = "horizontal-align: bottom") )
      # )
      
      
      # box(leaflet::leafletOutput(
      #   outputId = "Map_choropleth", height = 500, width = "65%"
      # ),width = "75%"),
      # box(plotOutput(outputId = "barchart"), height = 500, width = "35%")
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
      dplyr::filter(Area != "Hele landet" 
                    & Time==input$time 
                    & Type == input$propType 
                    & Metric == input$metricType)
  })
  
  library(htmlwidgets)
  library(htmltools)
  
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 35%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))
  
  title <- tags$div(
    tag.map.title, HTML("This is a title for the map")
  )
  
  
  
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
      leaflet.extras::setMapWidgetStyle(list(background = "white"))%>%
        setView(lat = 56.606422, lng = 11.947966, zoom = 7.15)%>%
        addControl(title, position = "topleft", className="map-title")
      
      
      # Add the GeoJSON layer for Jylland
      
      #m <- addGeoJSON(m, data = "jylland.geojson")
      
      # Define a reactive variable to track clicks
      
      
      # click_data <- reactiveValues(clicked = FALSE)
      
      
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
  
  #observeEvent(input$Map_choropleth_click, { # update the location selectInput on map clicks
  #  p <- input$Map_choropleth_click
  #  print(p)
  #  print("niblle")
  #})
  #print("jo")
  
  observeEvent(input$Map_choropleth_shape_click, {
    event <- input$Map_choropleth_shape_click
    print(event)
    print("no")
    print(event$id)
    #if (event$id == "jylland") {
    #  click_data$clicked <- !click_data$clicked
    #  # Toggle the fill color of the Jylland layer between red and default
    #  fillColor <- if (click_data$clicked) "red" else "default"
    #  #updateShapeStyle(m, layerId = "jylland", fillColor = fillColor)
    #}
  })
  
  
  # barchart to test the interactivity of the users slider on the barchart
  df_linechart <- reactive({
    
    samlet_data%>%
      dplyr::filter(Area =="Hele landet" 
                    & Metric == input$metricType 
                    & Type == input$propType)%>%
      dplyr::mutate(Aar1 = Time)%>%
      tidyr::separate(Time, into = c("Year", "Quarter"), sep = "K")%>%
      dplyr::mutate(Quarter = as.numeric(Quarter),
                    Date = lubridate::ymd(paste0(Year, "-01-01")) + months(3) * (Quarter - 1))
  })
  
  output$linechart <- plotly::renderPlotly({
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
  
  
  df_barchart <- reactive({
    samlet_data%>%
      dplyr::filter(Area !="Hele landet" 
                    & Metric == input$metricType 
                    & Type == input$propType
                    & Time == input$time)
  })
  
  output$barchart <- plotly::renderPlotly({
    data <- df_barchart()
    
    plotly::ggplotly(ggplot2::ggplot(data =data, 
                                     mapping = aes(x = forcats::fct_reorder(Area, Value), y = Value, text = paste0(Area, "\n", Time, ": ", Value)
                                     ))+
                       geom_bar(stat="identity", fill="#6495ed", alpha=.6) +
                       coord_flip()+
                       scale_x_discrete(name ="",  position = "top")+
                       ggplot2::ggtitle(unique(data$MetricName), "tets") +
                       # scale_y_continuous(name = "Value",
                       #                    breaks = seq(0, -max(data$Value)*1.10, by=-25),  # y axis values (before coord_flip) 
                       #                    labels = seq(0, max(data$Value)*1.10, by=25))+  # show non-negative values
                      # xlab("") +
                       #ggplot2::scale_colour_manual(values = "#6495ed") + 
                       ggthemes::theme_hc()
                       # theme(legend.position="none",
                       #       axis.text.x=element_blank(),
                       #       axis.text.y=element_blank())
                     ,tooltip = "text", 
                     )
  })
  
  # output$text <- renderPrint({
  #   data <- dfInput()
  #   label_choice <- paste0("Developments in property prices, ", unique(data$Time))
  #   print(label_choice)
  # })

}

shinyApp(ui, server)