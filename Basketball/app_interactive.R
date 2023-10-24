library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(magrittr)
library(tidyverse)

source("court_plot.R")
source("generate_scatter_plot.R")
source("generate_heatmap_plot.R")
source("generate_linechart_plot.R")

#Load the data, but when you have loaded it once, comment the below line out.
#load("data/basketball.RData") #Load environment to get the necessary data

dict <<- list("0" = "2-pointer", "1" = "3-pointer")
change_names <<- list("2-pointer" = "two_pointer",
                      "3-pointer" = "three_pointer")

#all_nba_data <- all_nba_data%>%mutate(quarter=dplyr::case_when(grepl("overtime", quarter)==T ~ "Overtime", TRUE ~ quarter),
#                                      made_factor = ifelse(made_factor == "Not made", "missed", "made"),
#                                      shotX = shotX - 23.62167)

#all_nba_data <- all_nba_data%>%
# dplyr::mutate(quarter=dplyr::case_when(grepl("overtime", quarter)==T ~ "Overtime", TRUE ~ quarter),
#               made_factor = as.factor(ifelse(made == T, "Made", "Not made")),
#               shotX = shotX - 23.62167)


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
               
               selectizeInput(inputId = "seasons", 
                              choices = all_nba_data %>% 
                                dplyr::select(season)%>%
                                unique()%>%arrange(desc(season)), 
                              selected = "2020/21", 
                              multiple = T, 
                              #selectize = F, 
                              label = "Seasons"
               ),
               
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
               ),
               radioGroupButtons(inputId = "charttype",
                                  label = "Chart type", 
                                  choices = c("Scatter", "Heatmap", "Hexagonal"), 
                                  selected = "Scatter",
                                  size = "sm", 
                                  justified = T,
                                  checkIcon = list(
                                   yes = icon("square-check"),
                                   no = icon("square")
                                  ),  status = "primary"),
               
               
               #UI related to scatter (only shown if scatter is selected)
               uiOutput("scatter_size_slider")
               
               
               
               
               , width = 2)),
             mainPanel(
               fluidRow(h2("Indhold for players"),
                 column(width = 8, 
                        fluidRow(
                          style = "width:102.5%;",
                          plotlyOutput("scatterplot",width = "100%")
                        ),br(),br(),
                        fluidRow(
                          style = "width:102.5%;",
                          plotlyOutput("line_chart"))
                 ),
                 column(width=4,
                        plotlyOutput("radarplot"))
               )
                       )
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
               plotlyOutput("line_chart_team"),
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
  
  playerRecentSeason <- reactive({
    all_nba_data%>%
      dplyr::filter(player==input$selectPlayer) %>% 
      select(season)%>%unique()%>%arrange(desc(season))%>%head(1)
  })
  
  playerSeasons <- reactive({
    all_nba_data %>% 
      dplyr::filter(player==input$selectPlayer) %>% 
      dplyr::select(season)%>%
      unique()%>%arrange(desc(season))
  })
  
  observe(updateSliderInput(session, 
                            inputId = "distanceToRim", 
                            min = playerInputMin(),
                            max = playerInputMax(), 
                            value =c(playerInputMin(), playerInputMax()) ))
  
  observe(updateSelectizeInput(session,
                              inputId = "seasons",
                              choices = playerSeasons(),
                              selected = playerRecentSeason() ))
  
  
  
  output$scatter_size_slider = renderUI({
    req(input$charttype == "Scatter")
    
    sliderInput("scatter_size",
                "Dot size",
                min = 0,
                max = 1,
                ticks = F, 
                value = 1,
                step = 0.1)
  })
  
  
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
                    season %in% input$seasons &
                    quarter %in% input$quarters &
                    time_remaining >= input$timeRemaining[1] & time_remaining <= input$timeRemaining[2] &
                    distance >= input$distanceToRim[1] & distance <= input$distanceToRim[2] &
                    status %in% input$gamestatus)  
  })

  


  # Create the scatterplot
  output$scatterplot <- renderPlotly({
    df_player <- df_players()
    
    if(input$charttype == "Scatter"){
      #browser()
      req(input$scatter_size)  
      withProgress({  # create_scatter
        create_scatter(df_player, court = plot_court(), 
                       size = input$scatter_size, source="scatter_selected")%>%
          layout( clickmode = "event+select",
                  plot_bgcolor='rgba(0,0,0,0)',
                  paper_bgcolor='rgba(0,0,0,0)',
                  legend='rgba(0,0,0,0)',
                  autosize = F, width = "100%", height = 600, margin = list(
                    l = 0,
                    r = 0,
                    b = 25,
                    t = 10,
                    pad = 2
                  ))
        
      }, message = "Calculating...")
      
    }else if(input$charttype == "Heatmap"){
     create_heatmap(df_player, court = plot_court(), source="scatter_selected") %>%
        layout(
          clickmode = "event+select",
          # xaxis = list(range=list(0,50)),
          # yaxis=list(range=list(-4,47.75)),
          plot_bgcolor='rgba(0,0,0,0)',
          paper_bgcolor='rgba(0,0,0,0)',
          legend='rgba(300,200,255,0)',
          autosize = F, width = "100%", height = 600, margin = list(
            l = 50,
            r = 50,
            b = 0,
            t = 50,
            pad = 2
          )
        )
    }else{print("Not implemented")}
    
    
  })

  # Create the line chart
  output$line_chart <- renderPlotly({
    df_player <- df_players()
    plot <- create_linechart(data=df_player, source="line_trace")
    plot <- plot %>% layout(
      xaxis = list(title = "Season"),
      yaxis = list(title = "Number of Shots Made"),
      legend = list(title = "Shot Type"),
      title = "2- and 3-Pointer shot average per game",
      clickmode = "event+select"
    )
    plot
  })
  

  # Capture selected data from the scatterplot
  scatter_selected_data <- reactive({
    df_player <- df_players()
    selected_data <- event_data("plotly_selected", source = "scatter_selected")
    test_sel <<- selected_data
    print(selected_data)
    trace <- unique(selected_data$curveNumber)
    trace <- ifelse(trace == 1, "Made", "Not made")
    
    if (!is.null(selected_data)) {
      filtered_scatter_data <- df_player[df_player$shotX %in% selected_data$x &
                                           df_player$shotY %in% selected_data$y &
                                           df_player$made_factor %in% trace, ]
      print(filtered_scatter_data)
      return(filtered_scatter_data)
    } else {
      return(NULL)
    }
  })
  
  # Capture traces from lineplot
  line_selected_data <- reactive({
    df_player <- df_players()
    selected_data <- event_data("plotly_selected", source = "line_trace")
    trace <- unique(selected_data$customdata)
    if (!is.null(selected_data)) {
      curves <- unique(selected_data$curveNumber)
      vals <- c()
      
      for(i in curves){
        vals <- c(dict[[as.character(i)]], vals)
      }
      
      filtered_scatter_data <- df_player %>% dplyr::filter(season %in% c(unique(selected_data$x)),
                                                           shot_type %in% vals)
      return(filtered_scatter_data)
    } else {
      return(NULL)
    }
  })

  common_selected_data <- reactive({
    if (!is.null(scatter_selected_data())) {
      selected_data <- scatter_selected_data() # You can also use bar_selected_data() if needed
      return(selected_data)
    }
    else if(!is.null(line_selected_data())) {
      selected_data <- line_selected_data() # You can also use bar_selected_data() if needed
      return(selected_data)
    }
  })

  # Update the bar chart based on selected data from the scatterplot
  observe({
    selected_data <- common_selected_data()
    if (!is.null(scatter_selected_data())) {
      output$line_chart <- renderPlotly({
        plot <- create_linechart(data=selected_data, source="line_trace")
        plot <- plot %>% layout(
          xaxis = list(title = "Season"),
          yaxis = list(title = "Number of Shots Made"),
          legend = list(title = "Shot Type"),
          title = "2- and 3-Pointer shot average per game",
          clickmode = "event+select"
        )
        plot
      })
    }
    else if (!is.null(line_selected_data())) {
      output$scatterplot <- renderPlotly({
        if(input$charttype == "Scatter"){
          create_scatter(selected_data, court = plot_court(), 
                         size = input$scatter_size, source="scatter_selected")%>%
          layout(clickmode = "event+select",
                  plot_bgcolor='rgba(0,0,0,0)',
                  paper_bgcolor='rgba(0,0,0,0)',
                  legend='rgba(0,0,0,0)',
                  autosize = F, width = "100%", height = 600, margin = list(
                    l = 0,
                    r = 0,
                    b = 25,
                    t = 10,
                    pad = 2
                  ))
  
        }else if(input$charttype == "Heatmap"){
          create_heatmap(selected_data, court = plot_court(), 
                         source="scatter_selected") %>%
            layout(
              clickmode = "event+select",
              # xaxis = list(range=list(0,50)),
              # yaxis=list(range=list(-4,47.75)),
              plot_bgcolor='rgba(0,0,0,0)',
              paper_bgcolor='rgba(0,0,0,0)',
              legend='rgba(300,200,255,0)',
              autosize = F, width = "100%", height = 600, margin = list(
                l = 50,
                r = 50,
                b = 0,
                t = 50,
                pad = 2
              )
            )
        }else{print("Not implemented")}
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