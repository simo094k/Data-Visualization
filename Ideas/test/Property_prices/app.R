library(shinydashboard)
library(shinyWidgets)
#library(plotly)
library(ggplot2)
library(magrittr)
library(leaflet)
library(leafletDK)

#load("PropertyData.RData") #Load environment to get the necessary data

landsdel_ejendomme_alle_aar <- readr::read_delim("landsdel_ejendomme_alle_aar.csv", 
                                                 delim = ";", escape_double = FALSE, col_names = FALSE, 
                                                 trim_ws = TRUE)%>%
  dplyr::rename("Aar" = X1, 
                "Hele landet" = X2,
                "Byen København" = X3,
                "Københavns omegn"= X4,
                "Nordsjælland" = X5,
                "Bornholm" = X6,
                "Østsjælland" = X7,
                "Vest- og Sydsjælland" = X8,
                "Fyn" = X9,
                "Sydjylland" = X10,
                "Østjylland" = X11,
                "Vestjylland" = X12,
                "Nordjylland" = X13)%>%
  tidyr::pivot_longer(!Aar, names_to = "Landsdele", values_to = "Indeks")

  

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
      choices = unique(landsdel_ejendomme_alle_aar$Aar),
      selected = max(unique(landsdel_ejendomme_alle_aar$Aar)), #Selected as default
      grid = FALSE, dragRange = FALSE
    )
  ),
  dashboardBody( #Inside here, we can design where and how the plots are shown!
    fluidRow(
      
      splitLayout(style = "border: 1px solid silver:", cellWidths = c("65%","35%"), 
                  leaflet::leafletOutput(
                    outputId = "Map_choropleth", height = 1000#, width = "65%"
                  ),
                  plotly::plotlyOutput("plot1", height = 500) 
      ),
      fluidpa
      
      
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
    landsdel_ejendomme_alle_aar%>%
      dplyr::filter(Landsdele != "Hele landet" & Aar==input$mySliderText)
  })
  
  output$Map_choropleth <- leaflet::renderLeaflet({
    #browser()
    data <- dfInput()
    # pal <- leaflet::colorNumeric(
    #   palette = "Blues",
    #   domain = landsdel_ejendomme_alle_aar%>%
    #                    dplyr::filter(Landsdele != "Hele landet")%>%dplyr::select(Indeks)
    # )

 
      leafletDK::ruralDK(value = "Indeks",
                       id = "Landsdele", 
                       data = data, 
                       legend = T, 
                       legendtitle = paste0("Index, ", unique(data$Aar)),
                       pal = "Blues", 
                       logcol = T
                      )%>%
      leaflet.extras::setMapWidgetStyle(list(background = "white"))
      # %>%
      #   leaflet::addLegend(
      #     position = 'bottomleft',
      #     colors = pal(data$Indeks),
      #     labels = data$Indeks, opacity = 1,
      #     values = ~data$Indeks,
      #     title = 'An Obvious Legend'
      #   )
      
      
    # label_choice <- paste0("Developments in property prices, ", unique(data$Aar))
    # ggplotly(
    #   ggplot(data = data,
    #           aes(fill = Indeks, 
    #               geometry = geometry,
    #               text = paste0(Landsdele,": ", Indeks))) +
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
    landsdel_ejendomme_alle_aar%>%
      dplyr::filter(Landsdele =="Hele landet")%>%
      dplyr::mutate(Aar1 = Aar)%>%
      tidyr::separate(Aar, into = c("Year", "Quarter"), sep = "K")%>%
      dplyr::mutate(Quarter = as.numeric(Quarter),
                    Date = lubridate::ymd(paste0(Year, "-01-01")) + months(3) * (Quarter - 1))
  })
  
  output$plot1 <- plotly::renderPlotly({
    data <- df_linechart()
    plotly::ggplotly(ggplot2::ggplot(data = data, 
                                     mapping = aes(x = Date, y = Indeks, color = Landsdele#, text = paste0("Date: ", Aar1)
                                                   ))+
      geom_line(size=1.5) +
      ggplot2::scale_colour_manual(values = "#6495ed") + 
        ggthemes::theme_hc()+
        theme(legend.position="none"))
  })
  
  
  
  # output$text <- renderPrint({
  #   data <- dfInput()
  #   label_choice <- paste0("Developments in property prices, ", unique(data$Aar))
  #   print(label_choice)
  # })

}

shinyApp(ui, server)