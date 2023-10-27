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

dict <<- list("0" = "2-pointer", "1" = "3-pointer")
change_names <<- list("2-pointer" = "two_pointer",
                      "3-pointer" = "three_pointer")

#Load the data, but when you have loaded it once, comment the below line out.
#load("data/basketball.RData") #Load environment to get the necessary data

#all_nba_data <- all_nba_data%>%mutate(quarter=dplyr::case_when(grepl("overtime", quarter)==T ~ "Overtime", TRUE ~ quarter),
#                                      made_factor = ifelse(made_factor == "Not made", "missed", "made"),
#                                      shotX = shotX - 23.62167)


ui <- fluidPage(
  tags$style(HTML("
          .navbar .navbar-header {float: left; width:15% }
          .navbar .navbar-nav {float: left; }
          .container {min-width: 1250} 
          
          
          .option[data-value=made], .item[data-value=made]{
          background: red !important;
          color: white !important;
        }
        .option[data-value=missed], .item[data-value=missed]{
          background: green !important;
          color: white !important;
        }
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
               
               pickerInput(inputId = "seasons", 
                              choices = all_nba_data %>% 
                                dplyr::select(season)%>%
                                unique()%>%arrange(desc(season)), 
                              selected = "2020/21", 
                              multiple = T, 
                              #selectize = F, 
                              label = "Seasons",
                           options = list(`actions-box` = TRUE, 
                                          "style-base" = "form-control", 
                                          style = "")
               ),
               
                 pickerInput(inputId = "quarters", 
                               choices = all_nba_data %>% 
                                 dplyr::select(quarter)%>%
                                 unique(), 
                             selected = "1st quarter", 
                             multiple = T, 
                             #selectize = F, 
                             label = "Game period",
                             options = list(`actions-box` = TRUE, 
                                            "style-base" = "form-control", 
                                            style = "")
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
               
               selectizeInput(inputId = "made", 
                              choices = c("made", "missed"), 
                              selected = c("made", "missed"), 
                              multiple = T, 
                              #selectize = F, 
                              label = "Shot made"
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
                 column(width = 6, style='padding-left:0px; padding-right:1px; padding-top:0px; padding-bottom:5px',
                        fluidRow(
                          #style = "width:102.5%;",
                          plotlyOutput("scatterplot",width = "100%")
                        ),br(),br(),
                        fluidRow(
                          )
                 ),
                 column(width=6,offset = 0, style='padding-left:0px; padding-right:1px; padding-top:0px; padding-bottom:5px',
                        fluidRow(#style = "width:102.5%;",
                                 plotlyOutput("line_chart")),
                        br(),br(),
                        fluidRow(#style = "width:102.5%;",
                          plotlyOutput("radarplot",width = "100%")
                        )
                        )
               )
               , width = 10)
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
  
  observe(updatePickerInput(session,
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
                    status %in% input$gamestatus &
                    made_factor %in% input$made) 
  })



  # Create the scatterplot
  output$scatterplot <- renderPlotly({
    df_player <- df_players()
    # browser()
    if(input$charttype == "Scatter"){
     # browser()
      req(input$scatter_size)
      withProgress({
        create_scatter(df_player, court = plot_court(), 
                       size = input$scatter_size, source="scatter_selected")%>%
          layout( clickmode = "event+select",
                  plot_bgcolor='rgba(0,0,0,0)',
                  paper_bgcolor='rgba(0,0,0,0)',
                  legend='rgba(0,0,0,0)',
                  autosize = F, margin = list(
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
          autosize = F, margin = list(
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
    # browser()
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
    trace <- unique(selected_data$curveNumber)
    trace <- ifelse(trace == 1, "made", "missed")
    
    if (!is.null(selected_data)) {
      #browser()
      filtered_scatter_data <- df_player[df_player$shotX %in% selected_data$x &
                                           df_player$shotY %in% selected_data$y &
                                           df_player$made_factor %in% trace, ]
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
      #browser()
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
                   autosize = F, margin = list(
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
              autosize = F, margin = list(
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
      dplyr::filter(player == input$selectPlayer & 
                      season %in% input$seasons) %>%
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
  
  get_avg <- function(selected_season, data){
    league_avg <- data %>%
      dplyr::filter(season == selected_season) %>%
      group_by(date, id_team) %>%
      dplyr::summarise(
        dunksPerGame = sum(distance == 0) / length(unique(player)),
        threePointersPerGame = sum(shot_type == "3-pointer") / length(unique(player)),
        twoPointersPerGame = sum(shot_type == "2-pointer") / length(unique(player)),
        ShotsUnderPressurePerGame = sum(
          quarter == "4th quarter" & time_remaining <= 5.0 & (abs(as.integer(strsplit(score, "-")[[1]][1]) - as.integer(strsplit(score, "-")[[1]][2]))) <= 10
        ) / length(unique(player)),
        pointsPerGame = (2*sum(made == TRUE & shot_type == "2-pointer") + (3*sum(made == TRUE & shot_type == "3-pointer"))) / length(unique(player))
      ) %>%
      ungroup() %>%
      select(-c(date, id_team)) %>%
      colMeans()
    
    # Create a data frame with the 'season' variable
    league_avg$season <- selected_season
    league_avg_df <- data.frame(t(league_avg))
    
    return(league_avg_df)
  }
  
  filter_players_by_position <- function(data, input_position) {
    filtered_data <- data[grepl(input_position, data$position, ignore.case = TRUE), ]
    return(filtered_data)
  }
  
  player_position <- reactive({
    chosen_player_position <- all_nba_data %>% filter(player == input$selectPlayer) %>% select(c(position)) %>% unique(.)
    chosen_player_position <- chosen_player_position$position
    
    seasons <- unique(all_nba_data$season)
    
    
    compare_league <- F
    if(compare_league == T) {
      compare_legend <<- 'League Average'
      compare_df <-
        lapply(seasons, get_avg, data = all_nba_data) %>%
        do.call(rbind, .) %>%
        unnest(everything()) %>%
        as.data.frame()
    } else{
      compare_legend <<- chosen_player_position
      compare_df <-
        lapply(seasons,
               get_avg,
               data = filter_players_by_position(all_nba_data, chosen_player_position)) %>%
        do.call(rbind, .) %>%
        unnest(everything()) %>%
        as.data.frame()}
  })
  
 
  
  

  
  
  
  compare_radar <- reactive({
    compare_df <- player_position()
    compare_df %>% filter(season %in% input$seasons)
    
  })

  
  
  
  output$radarplot<- renderPlotly({
    #browser()
    radar_data2 <- radar_data()
    
    compare_radar2 <- compare_radar()
    
    
    fig <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      mode = 'markers'
    ) 
    fig <- fig %>%
      add_trace(
        r = unlist(radar_data2),
        theta = colnames(radar_data2),
        name = input$selectPlayer
      ) 
    
    fig <- fig %>%
      add_trace(
        r = colMeans(compare_radar2 %>% select(-c(season))),
        theta = compare_radar2 %>% select(-c(season)) %>% colnames(.),
        name = compare_legend
      )
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = 
              c(0,max(max(ceiling(max(radar_data2))), max(ceiling(compare_radar2 %>% select(-c(season))))))
          )
        ),
        showlegend = T
      )
    
    fig
    
  })
  
  
  #-----------TEAM Main panel
  
  

}

shinyApp(ui, server)