library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(magrittr)

load("data/basketball.RData") #Load environment to get the necessary data

  


ui <- fluidPage(
  tags$style(HTML("
          .navbar .navbar-header {float: left; width:15% }
          .navbar .navbar-nav {float: left; }
          .container {min-width: 1250px}
        ")
  ),
  shinyjs::useShinyjs(),
  navbarPage(title = tagList(("Basketball"),
                             actionLink(inputId = "sidebar_button",
                                        label = NULL,
                                        icon = icon("bars"))), 
             theme=shinythemes::shinytheme("flatly"),
             # tags$head(tags$style(HTML('
             #                /* HEADER */
             #                .nav>li>a {
             #                padding: 1vh 8vw;;
             #                }'
             # ))),
             
  tabPanel(title = "Player",
           sidebarLayout(
             div(class="sidebar",
             sidebarPanel(
               htmlOutput("players", width = 50, height = 50),
               selectInput(inputId = "selectplayer", 
                           choices = all_nba_data %>% 
                             dplyr::filter(!is.na(pictures)) %>% 
                             select(Player=player)%>%unique()%>%arrange(Player), 
                           selected = "LeBron James", 
                           label = "", 
                           selectize = T),
               br(),
               h5("Filters"),
               
                 sliderTextInput(
                   inputId = "time", #The id is used to reference/connect it to some action in the server
                   label = "Year",
                   choices = c(1, 2, 3),
                   selected = 2, #Selected as default
                   grid = FALSE, dragRange = FALSE
                 ), width = 2
               
             )),
             mainPanel(h2("Indhold for players"))
           )),
  
  
  tabPanel(title = "Team",
           sidebarLayout(
             div(class="sidebar",
                 sidebarPanel(
                   h4("Filters"),
                   htmlOutput("picture1", width = 50, height=50),
                  
                   sliderTextInput(
                     inputId = "time", #The id is used to reference/connect it to some action in the server
                     label = "Year",
                     choices = c(1, 2, 3, 4),
                     selected = 2, #Selected as default
                     grid = FALSE, dragRange = FALSE
                   ), width = 2
                   
                 )),
             mainPanel(h2("Indhold for Team"))
           ))
  
  ))
  
  
server <- function(input, output) {
  #In the server, the fun happens. Here, we define what is actually shown.
  # It is a very good idea to keep it as structured as possible, and if the code
  # for each plot becomes to large, one can consider to place it in a seperate
  # file and refer to that file here instead.
  
  
  observeEvent(input$sidebar_button,{
    shinyjs::toggle(selector = ".sidebar")
  })
  
  src <- reactive({
    #browser()
    src <- all_nba_data %>% 
      dplyr::filter(!is.na(pictures)) %>% dplyr::select(player, pictures)%>%
      dplyr::filter(player == input$selectplayer)%>%dplyr::group_by(player)%>%unique()
    img <- c('<img src="',src$pictures,'"width="100%", height="250px">')
  })
  
 # browser()
  
  
  output$players<-renderText({ src()})
  
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