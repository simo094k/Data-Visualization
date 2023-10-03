library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(magrittr)
library(tidyverse)

#load("data/basketball.RData") #Load environment to get the necessary data


all_nba_data <- all_nba_data%>%
  dplyr::mutate(quarter=dplyr::case_when(grepl("overtime", quarter)==T ~ "Overtime", TRUE ~ quarter),
                made_factor = as.factor(ifelse(made == "True", "Made", "Not made")))


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
                                        icon = icon("basketball"))), 
             theme=shinythemes::shinytheme("cosmo"),
             
  tabPanel(title = "Player",
           sidebarLayout(
             div(class="sidebar",
             sidebarPanel(
               htmlOutput("players", width = 50, height = 50),
               selectInput(inputId = "selectPlayer", 
                           choices = all_nba_data %>% 
                             dplyr::filter(!is.na(pictures)) %>% 
                             select(Player=player)%>%unique()%>%
                             arrange(Player), 
                           selected = "LeBron James", 
                           label = NULL, 
                           selectize = T),
               #br(),
               h5("Filters"),
                 selectizeInput(inputId = "quarters", 
                               choices = all_nba_data %>% 
                                 dplyr::select(quarter)%>%
                                 unique(), 
                             selected = "1st quarter", 
                             multiple = T, 
                             #selectize = F, 
                             label = "Game period"
                              ),
               
               sliderInput(inputId = "timeRemaining",
                           min = 0.0, 
                           max = 12.0,
                           value = c(0,12),
                           step = 0.5,
                           ticks = F, 
                           label = "Time remaining in Q (min)"),
               
               sliderInput(inputId = "distanceToRim",
                           min = 0.0,
                           max = 39,
                           value = c(0,39),
                           step = 1.0,
                           ticks = F,
                           label = "Shot distance to basket (ft)"),
               
               selectizeInput(inputId = "gamestatus", 
                              choices = all_nba_data %>% 
                                dplyr::select(status)%>%
                                unique(), 
                              selected = c("trails", "tied", "leads"), 
                              multiple = T, 
                              #selectize = F, 
                              label = "Game status"
               )
               
               
               
               , width = 2)),
             mainPanel(
                       h2("Indhold for players"),
                       plotlyOutput("scatterplot"),
                       plotlyOutput("bar_chart"))
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
                   )
                   
                   , width = 2)),
             mainPanel(h2("Indhold for Team"))
           ))
  
  ))
  
  
server <- function(input, output, session) {
  #In the server, the fun happens. Here, we define what is actually shown.
  # It is a very good idea to keep it as structured as possible, and if the code
  # for each plot becomes to large, one can consider to place it in a seperate
  # file and refer to that file here instead.
  
  
  observeEvent(input$sidebar_button,{
    shinyjs::toggle(selector = ".sidebar")
  })
  
  playerInputMax <- reactive({
    all_nba_data%>%
      dplyr::filter(player==input$selectPlayer) %>% 
      select(distance)%>%max()
  })
  playerInputMin <- reactive({
    all_nba_data%>%
      dplyr::filter(player==input$selectPlayer) %>% 
      select(distance)%>%min()
  })
  
  observe(updateSliderInput(session, 
                            inputId = "distanceToRim", 
                            min = playerInputMin(),
                            max = playerInputMax(), 
                            value =c(playerInputMin(), playerInputMax()) ))
  
  src <- reactive({
   # browser()
    src <- all_nba_data %>% 
      dplyr::filter(!is.na(pictures)) %>% 
      dplyr::select(player, pictures)%>%
      dplyr::filter(player == input$selectPlayer)%>%dplyr::group_by(player)%>%unique()
    img <- c('<img src="',src$pictures,'"width="100%", height="250px">')
  })
  
  
  
  
  
  output$players<-renderText({ src()})
  
 
  
  
  #------- Main panel --------------
  
  
  df_players <- reactive({
    all_nba_data %>%
      dplyr::filter(player == input$selectPlayer)  # Bogdan Bogdanović
  })

  


  # Create the scatterplot
  output$scatterplot <- renderPlotly({
    df_player <- df_players()
    plot_ly(df_player, x = ~shotX, y = ~shotY, color = ~made_factor,
            type = "scatter", mode = "markers", source = "scatter_selected",
            customdata = ~made_factor) %>%
      layout(
        clickmode = "event+select"
      )
  })

  # Create the bar chart
  output$bar_chart <- renderPlotly({
    df_player <- df_players()
    plot_ly(df_player, x = ~distance, color = ~made_factor, type = "histogram",
            source = "bar_selected", 
            yaxis = list(title = 'Count'), customdata = ~made_factor) %>%
      layout(
        clickmode = "event+select"
      )
  })

  # Capture selected data from the scatterplot
  scatter_selected_data <- reactive({
    df_player <- df_players()
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
    df_player <- df_players()
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
                type = "histogram", source = "bar_selected",
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