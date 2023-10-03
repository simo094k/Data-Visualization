library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(magrittr)
library(tidyverse)

#Load the data, but when you have loaded it once, comment the below line out.
#load("data/basketball.RData") #Load environment to get the necessary data


# all_nba_data <- all_nba_data%>%
#   dplyr::mutate(quarter=dplyr::case_when(grepl("overtime", quarter)==T ~ "Overtime", TRUE ~ quarter),
#                 made_factor = as.factor(ifelse(made == T, "Made", "Not made")))


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
                             dplyr::filter(!is.na(pictures) & num>=600) %>% 
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
                       plotlyOutput("bar_chart"),
                       plotlyOutput("radarplot"))
           )),
  
  
  tabPanel(title = "Team",
           sidebarLayout(
             div(class="sidebar",
                 sidebarPanel(
                   htmlOutput("team", width = 50, height = 50),
                   selectInput(inputId = "selectTeam", 
                               choices = all_nba_data %>% 
                                 dplyr::filter(!is.na(pictures_team)) %>% 
                                 select("Team name" = team_name)%>%unique()%>%
                                 arrange("Team name"), 
                               selected = "Boston Celtics", 
                               label = NULL, 
                               selectize = T),
                   #br(),
                   h5("Filters"),
                   selectizeInput(inputId = "quartersTeam", 
                                  choices = all_nba_data %>% 
                                    dplyr::select(quarter)%>%
                                    unique(), 
                                  selected = "1st quarter", 
                                  multiple = T, 
                                  #selectize = F, 
                                  label = "Game period"
                   ),
                   
                   sliderInput(inputId = "timeRemainingTeam",
                               min = 0.0, 
                               max = 12.0,
                               value = c(0,12),
                               step = 0.5,
                               ticks = F, 
                               label = "Time remaining in Q (min)"),
                   
                   sliderInput(inputId = "distanceToRimTeam",
                               min = 0.0,
                               max = 39,
                               value = c(0,39),
                               step = 1.0,
                               ticks = F,
                               label = "Shot distance to basket (ft)"),
                   
                   selectizeInput(inputId = "gamestatusTeam", 
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
               h2("Indhold for team"),
               plotlyOutput("scatterplot_team"),
               plotlyOutput("bar_chart_team"),
               plotlyOutput("radarplot_team"))
           )),
  
  
  #LEAGUE
  tabPanel(title = "League",
           sidebarLayout(
             div(class="sidebar",
                 sidebarPanel(
                   tags$style(".well {background-color:white;}"),
                   htmlOutput("league", width = 50, height = 50),
                   h5("Filters"),
                   selectizeInput(inputId = "seasonLeague", 
                                  choices = all_nba_data %>% 
                                    dplyr::select(season)%>%
                                    unique(), 
                                  selected = c("2009/10", "2010/11", "2011/12", "2012/13", "2013/14",
                                               "2014/15", "2015/16", "2016/17", "2017/18", "2018/19",
                                               "2019/20", "2020/21"), 
                                  multiple = T, 
                                  #selectize = F, 
                                  label = "Pick seasons"
                   ),
                   selectizeInput(inputId = "positionLeague", 
                                  choices = c("Center", "Forward", "Guard"), 
                                  selected = c("Center", "Forward", "Guard"), 
                                  multiple = T, 
                                  #selectize = F, 
                                  label = "Game period"
                   )
                   
                   , width = 2)),
             mainPanel(
               h2("Indhold for league"))
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
  
  #-----------------------PLAYER TAB-------------------
  
  # -----PLAYER sidebar------
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
  
 
  #---------TEAM sidebar----------
  
  teamInputMax <- reactive({
    all_nba_data%>%
      dplyr::filter(team_name==input$selectTeam) %>% 
      select(distance)%>%max()
  })
  teamInputMin <- reactive({
    all_nba_data%>%
      dplyr::filter(team_name==input$selectTeam) %>% 
      select(distance)%>%min()
  })
  
  observe(updateSliderInput(session, 
                            inputId = "distanceToRimTeam", 
                            min = playerInputMin(),
                            max = playerInputMax(), 
                            value =c(playerInputMin(), playerInputMax()) ))
  
  
  src_team <- reactive({
    # browser()
    src <- all_nba_data %>% 
      dplyr::filter(!is.na(pictures_team)) %>% 
      dplyr::select(team_name, pictures_team)%>%
      dplyr::filter(team_name == input$selectTeam)%>%dplyr::group_by(team_name)%>%unique()
    img <- c('<img src="',src$pictures_team,'"width="100%", height="250px">')
  })
  
  output$team<-renderText({ src_team()})
  
  
  #-----LEAGUE sidebar
  
  output$league<-renderText({ c('<img src="','https://images.squarespace-cdn.com/content/v1/59c81157a8b2b0dc32a75c6f/1506287496945-0CJTL627EUO6OB9ZYKIJ/ada1027c456aed126d673601ace9099d_icon.png?format=300w','"width="100%", height="250px">')})
  
  
  
  #-------PLAYER Main panel --------------
  
  
  df_players <- reactive({
    all_nba_data %>%
      dplyr::filter(player == input$selectPlayer &
                    quarter %in% input$quarters &
                    time_remaining >= input$timeRemaining[1] & time_remaining <= input$timeRemaining[2] &
                    distance >= input$distanceToRim[1] & distance <= input$distanceToRim[2] &
                    status %in% input$gamestatus)  
  })



  # Create the scatterplot
  output$scatterplot <- renderPlotly({
    df_player <- df_players()
    scatter <- plot_ly(df_player, x = ~shotX, y = ~shotY, color = ~made_factor,
            type = "scatter", mode = "markers", source = "scatter_selected",
            customdata = ~made_factor) 
    
    scatter <- scatter%>%
      # add_segments(x = 0, xend = 0, y = -4, yend = 23.75, 
      #              line = list(color = 'black', width = 2, dash = 'dash'),
      #              showlegend = FALSE) %>%
      layout(
        clickmode = "event+select",
        xaxis = list(range=list(0,50)),
        yaxis=list(range=list(-4,47.75))
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
  
  
  #Radar plot
  radar_data <- reactive({
    all_nba_data %>% 
      dplyr::filter(player == input$selectPlayer) %>%
      dplyr::summarise(dunksPerGame = sum(distance == 0) / length(unique(date)),
                       threePointersPerGame = sum(shot_type == "3-pointer") 
                       / length(unique(date)),
                       twoPointersPerGame = sum(shot_type == "2-pointer") 
                       / length(unique(date)),
                       ShotsUnderPressurePerGame = length(
                         quarter == "4th quarter" 
                         & time_remaining <= 5.0 
                         & (abs(as.integer(strsplit(score, "-")[[1]][1]) 
                                - as.integer(strsplit(score, "-")[[1]][2]))) <= 10)
                       / sum(quarter == "4th quarter" 
                             & time_remaining <= 5.0 
                             & (abs(as.integer(strsplit(score, "-")[[1]][1]) 
                                    - as.integer(strsplit(score, "-")[[1]][2]))) <= 10),
                       pointsPerGame = (2*sum(made == T 
                                              & shot_type == "2-pointer")
                                        + 3*sum(made == T 
                                                & shot_type == "3-pointer"))
                       / length(unique(date)))
  })
  
  output$radarplot<- renderPlotly({
    #browser()
    radar_data2 <- radar_data()
    plot_ly( 
      type = 'scatterpolar',
      r = unlist(radar_data2),
      theta = colnames(radar_data2),
      fill = 'toself'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,ceiling(max(radar_data2)))
          )
        ),
        showlegend = F
      )
  })
  
  
  #-----------TEAM Main panel
  
  

}

shinyApp(ui, server)