library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(viridis)
library(gridExtra)
library(dplyr)

source("court_plot.R")
source("generate_scatter_plot.R")
source("generate_heatmap_plot.R")
source("generate_linechart_plot.R")
source("generate_line_league.R")


dict <<- list("0" = "two_pointer", "1" = "three_pointer", "2" = "two_pointer", "3" = "NULL")
change_names <<- list("2-pointer" = "two_pointer",
                      "3-pointer" = "three_pointer")

#Load the data, but when you have loaded it once, comment the below line out.
#load("data/basketball.RData") #Load environment to get the necessary data

#all_nba_data <- all_nba_data%>%mutate(team_name = ifelse(team_name == "New Orleans Hornets", "New Orleans Pelicans", team_name))


#Dont uncomment the following.
# all_nba_data <- all_nba_data%>%mutate(quarter=dplyr::case_when(grepl("overtime", quarter)==T ~ "Overtime", TRUE ~ quarter),
#                                      made_factor = ifelse(made_factor == "Not made", "Missed", "Made"),
#                                      shot_type = ifelse(shot_type == "2-pointer", "two_pointer",
#                                                         "three_pointer"),
#                                      shotX = shotX - 24.0, shotY = shotY + 1.1)


# UI ----------------------------------------------------------------------
#Label-size for the filters:
label_size_filters <- 22
dropdown_size_filters <- 20

ui <- fluidPage(
  tags$style(HTML("
          .navbar .navbar-header {float: left; width:15% }
          .navbar .navbar-nav {float: left; font-size:30px; margin:5px}
          .container {min-width: 1250} 
          .navbar-brand {font-size:40px; height: 60px; padding-left:35px
          a#sidebar_button {padding-left:15}}
          
          
        #   .option[data-value=made], .item[data-value=made]{
        #   background: red !important;
        #   color: white !important;
        # }
        # .option[data-value=missed], .item[data-value=missed]{
        #   background: green !important;
        #   color: white !important;
        # }
        ")
  ),
  tags$head(tags$style('
   body {
      font-family: Arial; 
      font-size: 40px:
      fint-style:bold;
      
   }'
  )),
  tags$style(type='text/css', 
             paste0(".selectize-input { font-size: ",label_size_filters,"px; line-height: ",label_size_filters,"px;} 
             .selectize-dropdown { font-size: ",dropdown_size_filters,"px; line-height: ",dropdown_size_filters,"px; }
             .picker-input { font-size: ",label_size_filters,"px; line-height: ",label_size_filters,"px;}
             .slider-input  { font-size: ",label_size_filters,"px; line-height: ",label_size_filters,"px;}
             .my-class {font-size: ",label_size_filters,"px; line-height: ",label_size_filters,"px;}
             .irs--shiny .irs-min, .irs--shiny .irs-max {font-size: ",label_size_filters,"px;}
             .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {font-size: ",label_size_filters-4,"px;}
             .irs--shiny .irs-handle {top:20px; width:17px: height: 17px;}
             .btn {font-size: 20px}
              .pretty .state label, .pretty .state label {font-size:larger}
              .col-sm-10 {width: 87.5%}
              .fa, .fa-brands, .fa-classic, .fa-regular, .fa-sharp, .fa-solid, .fab, .far, .fas {line-height:0.9}")),
  
  shinyjs::useShinyjs(),
  navbarPage(title = tagList(("HoopViz"),
                             actionLink(inputId = "sidebar_button",
                                        label = NULL,
                                        icon = icon("basketball"))), 
             theme=shinythemes::shinytheme("cosmo"),

## PLAYER UI ---------------------------------------------------------------

             
             tabPanel(title = "Player",
                      sidebarLayout(
                        div(class="sidebar", style="width: 75%;",
                            sidebarPanel(
                              htmlOutput("players", width = 50, height = 50
                              ),
                              selectInput(inputId = "selectPlayer", 
                                          choices = all_nba_data %>% 
                                            dplyr::filter(!is.na(pictures) & num>=600) %>% 
                                            dplyr::select(Player=player)%>%unique()%>%
                                            dplyr::arrange(Player), 
                                          selected = "LeBron James", 
                                          label = NULL, 
                                          selectize = T),
                              #br(),
                              h2("Filters"),
                              
                              pickerInput(inputId = "seasons", 
                                          choices = all_nba_data %>% 
                                            dplyr::select(season)%>%
                                            unique()%>%arrange(desc(season)), 
                                          selected = "2020/21", 
                                          multiple = T, 
                                          #selectize = F, 
                                          label = div(style = paste0("font-size:",label_size_filters,"px"),"Seasons"),
                                          options = list(`actions-box` = TRUE, 
                                                         "style-base" = "form-control", 
                                                         style = "my-class"),
                                          choicesOpt = list(
                                            style = rep_len(paste0("font-size: ",label_size_filters,"px; line-height: ",1,";"), 10)
                                          ) # choices style
                              ),
                              
                              pickerInput(inputId = "quarters", 
                                          choices = all_nba_data %>% 
                                            dplyr::select(quarter)%>%
                                            unique(), 
                                          selected = c("1st quarter", "2nd quarter", "3rd quarter", "4th quarter", "Overtime"), 
                                          multiple = T, 
                                          #selectize = F, 
                                          label = div(style = paste0("font-size:",label_size_filters,"px"),"Game period"),
                                          options = list(`actions-box` = TRUE, 
                                                         "style-base" = "form-control", 
                                                         style = "my-class"),
                                          choicesOpt = list(
                                            style = rep_len(paste0("font-size: ",label_size_filters,"px; line-height: ",1,";"), 5)
                                          ) # choices style
                                          
                              ),
                              
                              sliderInput(inputId = "timeRemaining",
                                          min = 0.0, 
                                          max = 12.0,
                                          value = c(0,12),
                                          step = 0.5,
                                          ticks = F, 
                                          label = div(style = paste0("font-size:",label_size_filters,"px"),"Time remaining in quarter (min)")),
                              
                              sliderInput(inputId = "distanceToRim",
                                          min = 0.0,
                                          max = 39,
                                          value = c(0,39),
                                          step = 1.0,
                                          ticks = F,
                                          label = div(style = paste0("font-size:",label_size_filters,"px"),"Shot distance to basket (ft)")),
                              
                              
                              selectizeInput(inputId = "gamestatus", 
                                             choices = all_nba_data %>% 
                                               dplyr::select(status)%>%
                                               unique(), 
                                             selected = c("trails", "tied", "leads"), 
                                             multiple = T, 
                                             #selectize = F, 
                                             label = div(style = paste0("font-size:",label_size_filters,"px"),"Game status")
                              ),
                              
                              # selectizeInput(inputId = "made", 
                              #                choices = c("made", "missed"), 
                              #                selected = c("made", "missed"), 
                              #                multiple = T, 
                              #                #selectize = F, 
                              #                label = "Shot made"
                              # ),
                              
                              radioGroupButtons(inputId = "charttype",
                                                label = div(style = paste0("font-size:",label_size_filters,"px"),"Court type"), 
                                                choices = c("Dot Map", "Heat Map"), 
                                                selected = "Dot Map",
                                                size = "normal", 
                                                justified = T,
                                                checkIcon = list(
                                                  yes = icon("square-check"),
                                                  no = icon("square")
                                                ),  status = "primary"),
                              
                              
                              #UI related to scatter (only shown if scatter is selected)
                              uiOutput("scatter_size_slider")
                              
                              , width = 2)),
                        mainPanel(br(),br(),
                                  fluidRow(
                                    column(width = 6, style='padding-left:0px; padding-right:1px; padding-top:0px; padding-bottom:5px',
                                           br(),br(),br(),
                                           fluidRow(
                                             #style = "width:102.5%;",
                                             plotlyOutput("scatterplot",width = "110%")
                                           ),br(),br()
                                    ),
                                    column(width=6,offset = 0, style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:5px',
                                           fluidRow(#style = "width:102.5%;", 
                                             plotlyOutput("line_chart"),
                                             ),
                                           br(), br(), 
                                           
                                           fluidRow(#style = "width:102.5%;",
                                             column(width = 2,offset = 10, br(), br(), 
                                                    prettyRadioButtons(
                                                      inputId = "radarPick",
                                                      label = NULL,
                                                      choices = c("Position average", "League average"), selected ="Position average", 
                                                      outline = TRUE,
                                                      plain = TRUE,
                                                      icon = tags$i(icon("basketball",style="font-size: 24px"))
                                                      
                                                    )
                                             ),
                                             plotlyOutput("radarplot",width = "110%")
                                             
                                           )
                                    )
                                  )
                                  , width = 10)
                      )),
            

## TEAM UI -----------------------------------------------------------------

             
             tabPanel(title = "Team",
                      sidebarLayout(
                        div(class="sidebar", style="width: 75%;",
                            sidebarPanel(
                              htmlOutput("team", width = 50, height = 50),
                              selectInput(inputId = "selectTeam", 
                                          choices = all_nba_data %>% 
                                            dplyr::filter(!is.na(pictures_team)) %>% 
                                            dplyr::select("Team name" = team_name)%>%unique()%>%
                                            dplyr::arrange("Team name"), 
                                          selected = "Boston Celtics", 
                                          label = NULL, 
                                          selectize = T),
                              #br(),
                              h2("Filters"),
                              pickerInput(inputId = "seasonsTeam", 
                                          choices = all_nba_data %>% 
                                            dplyr::select(season)%>%
                                            unique()%>%arrange(desc(season)), 
                                          selected = "2020/21", 
                                          multiple = T, 
                                          #selectize = F, 
                                          label = div(style = paste0("font-size:",label_size_filters,"px"),"Seasons"),
                                          options = list(`actions-box` = TRUE, 
                                                         "style-base" = "form-control", 
                                                         style = "my-class"),
                                          choicesOpt = list(
                                            style = rep_len(paste0("font-size: ",label_size_filters,"px; line-height: ",1,";"), 10)
                                          ) # choices style
                              ),
                              
                              pickerInput(inputId = "quartersTeam", 
                                          choices = all_nba_data %>% 
                                            dplyr::select(quarter)%>%
                                            unique(), 
                                          selected = c("1st quarter", "2nd quarter", "3rd quarter", "4th quarter", "Overtime"), 
                                          multiple = T, 
                                          #selectize = F, 
                                          label = div(style = paste0("font-size:",label_size_filters,"px"),"Game period"),
                                          options = list(`actions-box` = TRUE, 
                                                         "style-base" = "form-control", 
                                                         style = "my-class"),
                                          choicesOpt = list(
                                            style = rep_len(paste0("font-size: ",label_size_filters,"px; line-height: ",1,";"), 10)
                                          ) # choices style
                              ),
                              
                              sliderInput(inputId = "timeRemainingTeam",
                                          min = 0.0, 
                                          max = 12.0,
                                          value = c(0,12),
                                          step = 0.5,
                                          ticks = F, 
                                          label = div(style = paste0("font-size:",label_size_filters,"px"),"Time remaining in quarter (min)")),
                              
                              sliderInput(inputId = "distanceToRimTeam",
                                          min = 0.0,
                                          max = 39,
                                          value = c(0,39),
                                          step = 1.0,
                                          ticks = F,
                                          label = div(style = paste0("font-size:",label_size_filters,"px"),"Shot distance to basket (ft)")),
                              
                              
                              selectizeInput(inputId = "gamestatusTeam", 
                                             choices = all_nba_data %>% 
                                               dplyr::select(status)%>%
                                               unique(), 
                                             selected = c("trails", "tied", "leads"), 
                                             multiple = T, 
                                             #selectize = F, 
                                             label = div(style = paste0("font-size:",label_size_filters,"px"),"Game status")
                              ),
                              
                              # selectizeInput(inputId = "made", 
                              #                choices = c("made", "missed"), 
                              #                selected = c("made", "missed"), 
                              #                multiple = T, 
                              #                #selectize = F, 
                              #                label = "Shot made"
                              # ),
                              
                              radioGroupButtons(inputId = "charttypeTeam",
                                                label = div(style = paste0("font-size:",label_size_filters,"px"),"Court type"), 
                                                choices = c("Dot Map", "Heat Map"), 
                                                selected = "Dot Map",
                                                size = "normal", 
                                                justified = T,
                                                checkIcon = list(
                                                  yes = icon("square-check"),
                                                  no = icon("square")
                                                ),  status = "primary"),
                              
                              
                              #UI related to scatter (only shown if scatter is selected)
                              uiOutput("scatter_size_sliderTeam"),
                             br(),br()
                              
                              , width = 2)),
                        mainPanel(
                          br(),br(),
                          fluidRow(
                            column(width = 6, style='padding-left:0px; padding-right:1px; padding-top:0px; padding-bottom:5px',
                                   br(),br(),br(),br(),br(),br(),br(),
                                   fluidRow(
                                     #style = "width:102.5%;",
                                     plotlyOutput("scatterplot_team",width = "100%")
                                   ),br(),br()
                            ),
                            column(width=6,offset = 0, style='padding-left:0px; padding-right:0px; padding-top:0px; padding-bottom:5px',
                                   fluidRow(#style = "width:102.5%;",
                                     plotlyOutput("line_chart_team")),
                                    
                                   fluidRow(#style = "width:102.5%;",
                                     
                                     column(width = 4,offset = 8, br(),
                                            selectizeInput(inputId = "metricTeam", 
                                                           choices = c("Dunk rate", "Two pointer rate", "Three pointer rate", "Two pointer success rate","Three pointer success rate"), 
                                                           selected = "Three pointer rate", 
                                                           multiple = F, 
                                                           #selectize = F, 
                                                           label = div(style = paste0("font-size:",label_size_filters,"px"),"Choose metric"),width = "125%"
                                            ),div(style = "margin-top:-40px"),
                                            ),
                                     plotOutput("barchartTeam",width = "100%", height = 600)
                                     
                                   )
                            )
                          )
                          , width = 10)
                      )),
             

## LEAGUE UI ---------------------------------------------------------------

             
             #LEAGUE
             tabPanel(title = "League",
                      sidebarLayout(
                        div(class="sidebar", style="width: 75%;",
                            sidebarPanel(
                              tags$style(".well {background-color:white;}"),
                              htmlOutput("league", width = 50, height = 50)
                              
                              , width = 2)),
                        mainPanel(
                          column(width = 2,offset = 10,
                                 selectizeInput(inputId = "matrixplot_league_metric", 
                                                choices = c("Dunk rate", "Two pointer rate", "Three pointer rate", "Two pointer success rate","Three pointer success rate"), 
                                                selected = "Three pointer rate", 
                                                multiple = F, 
                                                #selectize = F, 
                                                label = div(style = paste0("font-size:",label_size_filters,"px"),"Choose metric")
                                 ),div(style = "margin-top:-40px"),
                                 ),
                          plotOutput("matrixplotLeague",width = "100%", height = 600),
                          br(),
                          br(),
                          br(),
                          column(width = 12,
                                 plotlyOutput("linechart_league",width = "100%", height = 600)
                                 )
                          , width = 10)
                      ))
             
  ))


# SERVER ------------------------------------------------------------------


server <- function(input, output, session) {
  #In the server, the fun happens. Here, we define what is actually shown.
  # It is a very good idea to keep it as structured as possible, and if the code
  # for each plot becomes to large, one can consider to place it in a seperate
  # file and refer to that file here instead.
  
  
  observeEvent(input$sidebar_button,{
    shinyjs::toggle(selector = ".sidebar")
  })


##PLAYER SIDEBAR ------------------------------------------------------------------
  playerInputMax <- reactive({
    all_nba_data %>%
      dplyr::filter(player == input$selectPlayer) %>%
      dplyr::select(distance) %>% max()
  })
  playerInputMin <- reactive({
    all_nba_data %>%
      dplyr::filter(player == input$selectPlayer) %>%
      dplyr::select(distance) %>% min()
  })
  playerInputMax <- reactive({
    all_nba_data %>%
      dplyr::filter(player == input$selectPlayer) %>%
      dplyr::select(distance) %>% max()
  })
  playerInputMin <- reactive({
    all_nba_data %>%
      dplyr::filter(player == input$selectPlayer) %>%
      dplyr::select(distance) %>% min()
  })

  playerRecentSeason <- reactive({
    #browser()
    all_nba_data%>%
      dplyr::filter(player==input$selectPlayer) %>% 
      dplyr::select(season)%>%unique()%>%dplyr::arrange(desc(season))%>%head(3)%>%as.vector()%>%unlist()
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
                            selected = playerRecentSeason() )
  )
  
  
  
  output$scatter_size_slider = renderUI({
    req(input$charttype == "Dot Map")
    
    sliderInput("scatter_size",
                div(style = paste0("font-size:",label_size_filters,"px"),"Dot size"),
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
  
  
  
  
##TEAM SIDEBAR ----------
  
  teamInputMax <- reactive({
    all_nba_data%>%
      dplyr::filter(team_name==input$selectTeam) %>% 
      dplyr::select(distance)%>%max()
  })
  teamInputMin <- reactive({
    all_nba_data%>%
      dplyr::filter(team_name==input$selectTeam) %>% 
      dplyr::select(distance)%>%min()
  })
  
  teamRecentSeason <- reactive({
    #browser()
    all_nba_data%>%
      dplyr::filter(team_name==input$selectTeam) %>% 
      dplyr::select(season)%>%unique()%>%dplyr::arrange(desc(season))%>%head(3)%>%as.vector()%>%unlist()
  })
  
  teamSeasons <- reactive({
    all_nba_data %>% 
      dplyr::filter(team_name==input$selectTeam) %>% 
      dplyr::select(season)%>%
      unique()%>%arrange(desc(season))
  })
  

  
  observe(updatePickerInput(session,
                            inputId = "seasonsTeam",
                            choices = teamSeasons(),
                            selected = teamRecentSeason() )
  )
  
  
  observe(updateSliderInput(session, 
                            inputId = "distanceToRimTeam", 
                            min = teamInputMin(),
                            max = teamInputMax(), 
                            value =c(teamInputMin(), teamInputMax()) ))
  
  output$scatter_size_sliderTeam = renderUI({
    req(input$charttype == "Dot Map")
    
    sliderInput("scatter_size_team",
                div(style = paste0("font-size:",label_size_filters,"px"),"Dot size"),
                min = 0,
                max = 1,
                ticks = F, 
                value = 0.8,
                step = 0.1)
  })
  
  
  src_team <- reactive({
    # browser()
    src <- all_nba_data %>% 
      dplyr::filter(!is.na(pictures_team)) %>% 
      dplyr::select(team_name, pictures_team)%>%
      dplyr::filter(team_name == input$selectTeam)%>%dplyr::group_by(team_name)%>%unique()
    img <- c('<img src="',src$pictures_team,'"width="100%", height="250px">')
  })
  
  output$team<-renderText({ src_team()})
  
  

# LEAGUE sidebar ----------------------------------------------------------


  
  output$league<-renderText({ c('<img src="','https://images.squarespace-cdn.com/content/v1/59c81157a8b2b0dc32a75c6f/1506287496945-0CJTL627EUO6OB9ZYKIJ/ada1027c456aed126d673601ace9099d_icon.png?format=300w','"width="100%", height="250px">')})
  
  
  
##PLAYER Main panel --------------
  
  
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
    # browser()
    if(input$charttype == "Dot Map"){
      # browser()
      req(input$scatter_size)
      withProgress({
        create_scatter(df_player, court = plot_court(), 
                       size = input$scatter_size, source="scatter_selected")%>%
          layout( clickmode = "event+select",
                  plot_bgcolor='rgba(0,0,0,0)',
                  paper_bgcolor='rgba(0,0,0,0)',
                  legend=list('rgba(0,0,0,0)', 
                              orientation = "h",   # show entries horizontally
                              xanchor = "center",  # use center of legend as anchor
                              x = 0.5, y=0.17),
                  autosize = F, margin = list(
                    l = 0,
                    r = 0,
                    b = 0,
                    t = 0,
                    pad = 2
                  ))
      }, message = "Calculating...")
      
    }else if(input$charttype == "Heat Map"){
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
    }
    
    
  })
  
  # Create the line chart
  output$line_chart <- renderPlotly({
    df_player <- df_players()
    # browser()
    plot <- create_linechart(data=df_player, sel_season=input$seasons, 
                             source="line_trace")
    plot <-  plot %>% layout(
      xaxis = list(title = list(text="Season", standoff=8, font = list(size=dropdown_size_filters)),tickfont = list(size = dropdown_size_filters)),
      yaxis = list(title = list(text="Number of shots made", standoff=11,font = list(size=dropdown_size_filters)),tickfont = list(size = dropdown_size_filters)),
      legend = list(title = "Shot Type",
                    orientation = "h",   # show entries horizontally
                    xanchor = "center",  # use center of legend as anchor
                    x = 0.5, y = -0.15,
                    font = list(size=dropdown_size_filters)),
      title = list(text="Average shot successes per game", y = 0.98, x = 0.5, xanchor = 'center', yanchor =  'top',  font = list(size=label_size_filters)),
      #margin = list(pad=-20),
      clickmode = "event+select",
      showlegend = TRUE
    )
    plot
  })
  
  # Capture selected data from the scatterplot
  scatter_selected_data <- reactive({
    df_player <- df_players()
    selected_data <- event_data("plotly_selected", source = "scatter_selected")
    trace <- unique(selected_data$curveNumber)
    trace <- ifelse(trace == 1, "Made", "Missed")
    
    if (!is.null(selected_data)) {
      # browser()
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
    selected_data <- event_data("plotly_selected", source="line_trace")
    trace <- unique(selected_data$customdata)
    #browser()
    if (!is.null(selected_data)) {
      curves <- unique(selected_data$curveNumber)
      vals <- c()
      
      for(i in curves){
        vals <- c(dict[[as.character(i)]], vals)
      }
      
      
      # "0" = "two_pointer", "1" = "three_pointer", "2" = "two_pointer", 
      # "3" = "NULL"
      
      # 2 = dunks
      # 1 = three_pointer
      
      if(2 %in% curves & not(1 %in% curves)){  # we have all 2-pointers
        if(all(curves == 2)){  # dunks
          filtered_scatter_data <- df_player %>% 
            dplyr::filter(season %in% c(unique(selected_data$x)),
                          distance < 2)
        }
        else{  # two and pointer and dunks
          #browser()
          filtered_scatter_data <- df_player %>% 
            dplyr::filter(season %in% c(unique(selected_data$x)),
                          shot_type %in% vals)
        }
      }
      else if(2 %in% curves & (1 %in% curves) & not(0 %in% curves)){  # Only dunks and three pointers
        filtered_scatter_data <- df_player %>% 
          dplyr::filter(season %in% c(unique(selected_data$x)),
                        distance < 2 | (distance >= 2 & shot_type == "three_pointer"))
      }
      else{ # we can have 3-pointers alone
        #browser()
        filtered_scatter_data <- df_player %>% dplyr::filter(season %in% c(unique(selected_data$x)),
                                                             shot_type %in% vals)
      }
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
        plot <- create_linechart(data=selected_data, sel_season=input$seasons,
                                 source="line_trace")
        plot <- plot %>% layout(
          xaxis = list(title = list(text="Season", standoff=8, font = list(size=dropdown_size_filters)),tickfont = list(size = dropdown_size_filters)),
          yaxis = list(title = list(text="Number of shots made", standoff=11,font = list(size=dropdown_size_filters)),tickfont = list(size = dropdown_size_filters)),
          legend = list(title = "Shot Type",
                        orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5, y = -0.15,
                        font = list(size=dropdown_size_filters)),
          title = list(text="Average shot successes per game", y = 0.98, x = 0.5, xanchor = 'center', yanchor =  'top',  font = list(size=label_size_filters)),
          #margin = list(pad=-20),
          clickmode = "event+select",
          showlegend = TRUE
        )
        plot
      })
    }
    else if (!is.null(line_selected_data())) {
      output$scatterplot <- renderPlotly({
        if(input$charttype == "Dot Map"){
          create_scatter(selected_data, court = plot_court(), 
                         size = input$scatter_size, source="scatter_selected")%>%
            layout(clickmode = "event+select",
                   plot_bgcolor='rgba(0,0,0,0)',
                   paper_bgcolor='rgba(0,0,0,0)',
                   legend=list('rgba(0,0,0,0)',
                               orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5,
                               font = list(size=dropdown_size_filters)),
                   autosize = F, margin = list(
                     l = 0,
                     r = 0,
                     b = 0,
                     t = 10,
                     pad = 2
                   ))
          
        }else if(input$charttype == "Heat Map"){
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
      dplyr::summarise(dunksPerGame = sum(distance < 2) / length(unique(date)),
                       threePointersPerGame = sum(shot_type == "three_pointer") 
                       / length(unique(date)),
                       twoPointersPerGame = sum(shot_type == "two_pointer") 
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
                                              & shot_type == "two_pointer")
                                        + 3*sum(made == T 
                                                & shot_type == "three_pointer"))
                       / length(unique(date)))
  })
  
  get_avg <- function(selected_season, data){
    league_avg <- data %>%
      dplyr::filter(season == selected_season) %>%
      group_by(date, id_team) %>%
      dplyr::summarise(
        dunksPerGame = sum(distance < 2) / length(unique(player)),
        threePointersPerGame = sum(shot_type == "three_pointer") / length(unique(player)),
        twoPointersPerGame = sum(shot_type == "two_pointer") / length(unique(player)),
        ShotsUnderPressurePerGame = sum(
          quarter == "4th quarter" & time_remaining <= 5.0 & (abs(as.integer(strsplit(score, "-")[[1]][1]) - as.integer(strsplit(score, "-")[[1]][2]))) <= 10
        ) / length(unique(player)),
        pointsPerGame = (2*sum(made == TRUE & shot_type == "two_pointer") + (3*sum(made == TRUE & shot_type == "three_pointer"))) / length(unique(player))
      ) %>%
      ungroup() %>%
      dplyr::select(-c(date, id_team)) %>%
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
    chosen_player_position <- all_nba_data %>% filter(player == input$selectPlayer) %>% dplyr::select(c(position)) %>% unique(.)
    chosen_player_position <- chosen_player_position$position
    
    seasons <- unique(all_nba_data$season)
    
    #browser()
    if(input$radarPick == "League average") {
      compare_legend <<- 'League Average'
      compare_df <- 
        lapply(seasons, get_avg, data = all_nba_data) %>%
        do.call(rbind, .) %>%
        unnest(everything()) %>%
        as.data.frame()
    } else if(input$radarPick == "Position average"){
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
    #browser()
    compare_df <- player_position()
    compare_df %>% filter(season %in% input$seasons) 
    
  })
  
  
  
  
  output$radarplot<- renderPlotly({
    #browser()
    radar_data2 <- radar_data()  %>% dplyr::rename("Two pointers" = twoPointersPerGame, 
                                                   "Three pointers" = threePointersPerGame,
                                                   "Dunks" = dunksPerGame,
                                                   "Points" = pointsPerGame,
                                                   "Shots under pressure" = ShotsUnderPressurePerGame)
    
    compare_radar2 <- compare_radar() %>% dplyr::rename("Two pointers" = twoPointersPerGame, 
                                                        "Three pointers" = threePointersPerGame,
                                                        "Dunks" = dunksPerGame,
                                                        "Points" = pointsPerGame,
                                                        "Shots under pressure" = ShotsUnderPressurePerGame)
    
    
    fig <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      mode = 'markers'
    ) 
    fig <- fig %>%
      add_trace(
        r = unlist(radar_data2),
        theta = colnames(radar_data2),
        name = input$selectPlayer,
        marker = list(color = c("#1b9e77")),
        fillcolor = "rgba(27,158,119,0.3)"
      ) 
    
    fig <- fig %>%
      add_trace(
        r = colMeans(compare_radar2 %>% dplyr::select(-c(season))),
        theta = compare_radar2 %>% dplyr::select(-c(season)) %>% colnames(.),
        name = compare_legend,
        marker = list(color = c("#7570b3")),
        fillcolor = "rgba(117,112,179,0.3)"
      )
    fig <- fig %>%
      layout(
        title = list(text="Average shot attempts per game", x=0.52, font=list(size=label_size_filters)),
        polar = list(
          radialaxis = list(
            visible = T,
            range = 
              c(0,max(max(ceiling(max(radar_data2))), max(ceiling(compare_radar2 %>% dplyr::select(-c(season))))))
          ),
          angularaxis = list(tickfont = list(size = dropdown_size_filters))
        ),
        showlegend = T,
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5,
                      font = list(size=dropdown_size_filters)),
        margin = list(t=90, pad=-20)
      )%>%
      layout(plot_bgcolor='rgb(254, 247, 234)') %>% 
      layout(paper_bgcolor='transparent')%>%
      layout(width = 1000)%>%
    config(displayModeBar = FALSE)
    
    fig
    
  })
  

##TEAM Main panel ---------------------------------------------------------

  
  #-----------TEAM Main panel
  
  
  
  df_teams <- reactive({
    all_nba_data %>%
      dplyr::filter(team_name == input$selectTeam &
                      season %in% input$seasonsTeam &
                      quarter %in% input$quartersTeam &
                      time_remaining >= input$timeRemainingTeam[1] & time_remaining <= input$timeRemainingTeam[2] &
                      distance >= input$distanceToRimTeam[1] & distance <= input$distanceToRimTeam[2] &
                      status %in% input$gamestatusTeam) 
  })
  
  
  
  # Create the scatterplot
  output$scatterplot_team <- renderPlotly({
    df_team <- df_teams()
    # browser()
    if(input$charttypeTeam == "Dot Map"){
      # browser()
      req(input$scatter_size_team)
      withProgress({
        create_scatter(df_team, court = plot_court(), 
                       size = input$scatter_size_team, source="scatter_selected_team")%>%
          layout( clickmode = "event+select",
                  plot_bgcolor='rgba(0,0,0,0)',
                  paper_bgcolor='rgba(0,0,0,0)',
                  legend=list('rgba(0,0,0,0)', 
                              orientation = "h",   # show entries horizontally
                              xanchor = "center",  # use center of legend as anchor
                              x = 0.5, y=0.17),
                  autosize = F, margin = list(
                    l = 0,
                    r = 0,
                    b = 0,
                    t = 0,
                    pad = 2
                  ))
      }, message = "Calculating...")
      
    }else if(input$charttypeTeam == "Heat Map"){
      create_heatmap(df_team, court = plot_court(), source="scatter_selected_team") %>%
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
    }
    
    
  })
  
  # Create the line chart
  output$line_chart_team <- renderPlotly({
    df_team <- df_teams()
    # browser()
    plot <- create_linechart(data=df_team, sel_season=input$seasonsTeam, 
                             source="line_trace_team")
    plot <-  plot %>% layout(
      xaxis = list(title = list(text="Season", standoff=8, font = list(size=dropdown_size_filters)),tickfont = list(size = dropdown_size_filters)),
      yaxis = list(title = list(text="Number of shots made", standoff=11,font = list(size=dropdown_size_filters)),tickfont = list(size = dropdown_size_filters)),
      legend = list(title = "Shot Type",
                    orientation = "h",   # show entries horizontally
                    xanchor = "center",  # use center of legend as anchor
                    x = 0.5, y = -0.15,
                    font = list(size=dropdown_size_filters)),
      title = list(text="Average shot successes per game", y = 0.98, x = 0.5, xanchor = 'center', yanchor =  'top',  font = list(size=label_size_filters)),
      #margin = list(pad=-20),
      clickmode = "event+select",
      showlegend = TRUE
    )
    plot
  })
  
  # Capture selected data from the scatterplot
  scatter_selected_data_team <- reactive({
    df_team <- df_teams()
    selected_data <- event_data("plotly_selected", source = "scatter_selected_team")
    trace <- unique(selected_data$curveNumber)
    trace <- ifelse(trace == 1, "Made", "Missed")
    
    if (!is.null(selected_data)) {
      # browser()
      filtered_scatter_data <- df_team[df_team$shotX %in% selected_data$x &
                                           df_team$shotY %in% selected_data$y &
                                           df_team$made_factor %in% trace, ]
      return(filtered_scatter_data)
    } else {
      return(NULL)
    }
  })
  
  # Capture traces from lineplot
  line_selected_data_team <- reactive({
    df_team <- df_teams()
    selected_data <- event_data("plotly_selected", source="line_trace_team")
    trace <- unique(selected_data$customdata)
    #browser()
    if (!is.null(selected_data)) {
      curves <- unique(selected_data$curveNumber)
      vals <- c()
      
      for(i in curves){
        vals <- c(dict[[as.character(i)]], vals)
      }
      
      
      # "0" = "two_pointer", "1" = "three_pointer", "2" = "two_pointer", 
      # "3" = "NULL"
      
      # 2 = dunks
      # 1 = three_pointer
      #browser()
      
      if(2 %in% curves & not(1 %in% curves)){  # we have all 2-pointers
        if(all(curves == 2)){  # dunks
          filtered_scatter_data <- df_team %>% 
            dplyr::filter(season %in% c(unique(selected_data$x)),
                          distance < 2)
        }
        else{  # two and pointer and dunks
          #browser()
          filtered_scatter_data <- df_team %>% 
            dplyr::filter(season %in% c(unique(selected_data$x)),
                          shot_type %in% vals)
        }
      }
      else if(2 %in% curves & (1 %in% curves) & not(0 %in% curves)){  # Only dunks and three pointers
        filtered_scatter_data <- df_team %>% 
          dplyr::filter(season %in% c(unique(selected_data$x)),
                        distance < 2 | (distance >= 2 & shot_type == "three_pointer"))
      }
      else{ # we can have 3-pointers alone
        #browser()
        filtered_scatter_data <- df_team %>% dplyr::filter(season %in% c(unique(selected_data$x)),
                                                             shot_type %in% vals)
      }
      return(filtered_scatter_data)
    } else {
      return(NULL)
    }
  })
  
  common_selected_data_team <- reactive({
    if (!is.null(scatter_selected_data_team())) {
      selected_data <- scatter_selected_data_team() # You can also use bar_selected_data() if needed
      return(selected_data)
    }
    else if(!is.null(line_selected_data_team())) {
      selected_data <- line_selected_data_team() # You can also use bar_selected_data() if needed
      return(selected_data)
    }
  })
  
  # Update the bar chart based on selected data from the scatterplot
  observe({
    selected_data_team <- common_selected_data_team()
    #browser()
    if (!is.null(scatter_selected_data_team())) {
      #browser()
      output$line_chart_team <- renderPlotly({
        plot <- create_linechart(data=selected_data_team, sel_season=input$seasonsTeam,
                                 source="line_trace_team")
        plot <-  plot %>% layout(
          xaxis = list(title = list(text="Season", standoff=8, font = list(size=dropdown_size_filters)),tickfont = list(size = dropdown_size_filters)),
          yaxis = list(title = list(text="Number of shots made", standoff=11,font = list(size=dropdown_size_filters)),tickfont = list(size = dropdown_size_filters)),
          legend = list(title = "Shot Type",
                        orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5, y = -0.15,
                        font = list(size=dropdown_size_filters)),
          title = list(text="Average shot successes per game", y = 0.98, x = 0.5, xanchor = 'center', yanchor =  'top',  font = list(size=label_size_filters)),
          #margin = list(pad=-20),
          clickmode = "event+select",
          showlegend = TRUE
        )
        plot
      })
    }
    else if (!is.null(line_selected_data_team())) {
      output$scatterplot_team <- renderPlotly({
        if(input$charttypeTeam == "Dot Map"){
          create_scatter(selected_data_team, court = plot_court(), 
                         size = input$scatter_size_team, source="scatter_selected_team")%>%
            layout(clickmode = "event+select",
                   plot_bgcolor='rgba(0,0,0,0)',
                   paper_bgcolor='rgba(0,0,0,0)',
                   legend=list('rgba(0,0,0,0)',
                               orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5,
                               font = list(size=dropdown_size_filters)),
                   autosize = F, margin = list(
                     l = 0,
                     r = 0,
                     b = 0,
                     t = 10,
                     pad = 2
                   ))
          
        }else if(input$charttypeTeam == "Heat Map"){
          create_heatmap(selected_data_team, court = plot_court(), 
                         source="scatter_selected_team") %>%
            layout(
              clickmode = "event+select",
              # xaxis = list(range=list(0,50)),
              # yaxis=list(range=list(-4,47.75)),
              plot_bgcolor='rgba(0,0,0,0)',
              paper_bgcolor='rgba(0,0,0,0)',
              legend=list('rgba(300,200,255,0)',font = list(size=dropdown_size_filters)),
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
  # radar_data_team <- reactive({
  #   all_nba_data %>% 
  #     dplyr::filter(team_name == input$selectTeam & 
  #                     season %in% input$seasonsTeam) %>%
  #     dplyr::summarise(dunksPerGame = sum(distance < 2) / length(unique(date)),
  #                      threePointersPerGame = sum(shot_type == "three_pointer") 
  #                      / length(unique(date)),
  #                      twoPointersPerGame = sum(shot_type == "two_pointer") 
  #                      / length(unique(date)),
  #                      ShotsUnderPressurePerGame = length(
  #                        quarter == "4th quarter" 
  #                        & time_remaining <= 5.0 
  #                        & (abs(as.integer(strsplit(score, "-")[[1]][1]) 
  #                               - as.integer(strsplit(score, "-")[[1]][2]))) <= 10)
  #                      / sum(quarter == "4th quarter" 
  #                            & time_remaining <= 5.0 
  #                            & (abs(as.integer(strsplit(score, "-")[[1]][1]) 
  #                                   - as.integer(strsplit(score, "-")[[1]][2]))) <= 10),
  #                      pointsPerGame = (2*sum(made == T 
  #                                             & shot_type == "two_pointer")
  #                                       + 3*sum(made == T 
  #                                               & shot_type == "three_pointer"))
  #                      / length(unique(date)))
  # })
  # 
  # get_avg_team <- function(selected_season, data){
  #   league_avg <- data %>%
  #     dplyr::filter(season == selected_season) %>%
  #     group_by(date, id_team) %>%
  #     dplyr::summarise(
  #       dunksPerGame = sum(distance < 2) / length(unique(team_name)),
  #       threePointersPerGame = sum(shot_type == "three_pointer") / length(unique(team_name)),
  #       twoPointersPerGame = sum(shot_type == "two_pointer") / length(unique(team_name)),
  #       ShotsUnderPressurePerGame = sum(
  #         quarter == "4th quarter" & time_remaining <= 5.0 & (abs(as.integer(strsplit(score, "-")[[1]][1]) - as.integer(strsplit(score, "-")[[1]][2]))) <= 10
  #       ) / length(unique(team_name)),
  #       pointsPerGame = (2*sum(made == TRUE & shot_type == "two_pointer") + (3*sum(made == TRUE & shot_type == "three_pointer"))) / length(unique(team_name))
  #     ) %>%
  #     ungroup() %>%
  #     dplyr::select(-c(date, id_team)) %>%
  #     colMeans()
  #   
  #   # Create a data frame with the 'season' variable
  #   league_avg$season <- selected_season
  #   league_avg_df <- data.frame(t(league_avg))
  #   
  #   return(league_avg_df)
  # }
  # 
  # 
  # team_position <- reactive({
  #   
  #   seasons <- unique(all_nba_data$season)
  #   
  #   #browser()
  #   if(input$radarPickTeam == "League average") {
  #     compare_legend <<- 'League Average'
  #     compare_df <- 
  #       lapply(seasons, get_avg_team, data = all_nba_data) %>%
  #       do.call(rbind, .) %>%
  #       unnest(everything()) %>%
  #       as.data.frame()
  #   }
  # })
  # 
  # 
  # 
  # compare_radar_team <- reactive({
  #   #browser()
  #   compare_df <- team_position()
  #   compare_df %>% filter(season %in% input$seasonsTeam) 
  #   
  # })
  # 
  # 
  # 
  # 
  # output$radarplot_team<- renderPlotly({
  #   #browser()
  #   radar_data2_team <- radar_data_team()  %>% dplyr::rename("Two pointers" = twoPointersPerGame, 
  #                                                  "Three pointers" = threePointersPerGame,
  #                                                  "Dunks" = dunksPerGame,
  #                                                  "Points" = pointsPerGame,
  #                                                  "Shots under pressure" = ShotsUnderPressurePerGame)
  #   
  #   compare_radar2_team <- compare_radar_team() %>% dplyr::rename("Two pointers" = twoPointersPerGame, 
  #                                                       "Three pointers" = threePointersPerGame,
  #                                                       "Dunks" = dunksPerGame,
  #                                                       "Points" = pointsPerGame,
  #                                                       "Shots under pressure" = ShotsUnderPressurePerGame)
  #   
  #   
  #   fig_team <- plot_ly(
  #     type = 'scatterpolar',
  #     fill = 'toself',
  #     mode = 'markers'
  #   ) 
  #   fig_team <- fig_team %>%
  #     add_trace(
  #       r = unlist(radar_data2_team),
  #       theta = colnames(radar_data2_team),
  #       name = input$selectTeam,
  #       marker = list(color = c("#1b9e77")),
  #       fillcolor = "rgba(27,158,119,0.3)"
  #     ) 
  #   
  #   fig_team <- fig_team %>%
  #     add_trace(
  #       r = colMeans(compare_radar2_team %>% dplyr::select(-c(season))),
  #       theta = compare_radar2_team %>% dplyr::select(-c(season)) %>% colnames(.),
  #       name = compare_legend,
  #       marker = list(color = c("#7570b3")),
  #       fillcolor = "rgba(117,112,179,0.3)"
  #     )
  #   fig_team <- fig_team %>%
  #     layout(
  #       title = list(text="Shot attempts per game", x=0.52),
  #       polar = list(
  #         radialaxis = list(
  #           visible = T,
  #           range = 
  #             c(0,max(max(ceiling(max(radar_data2_team))), max(ceiling(compare_radar2_team %>% dplyr::select(-c(season))))))
  #         )
  #       ),
  #       showlegend = T,
  #       legend = list(orientation = "h",   # show entries horizontally
  #                     xanchor = "center",  # use center of legend as anchor
  #                     x = 0.5),
  #       margin = list(t=90, pad=20)
  #     )%>%
  #     config(displayModeBar = FALSE)
  #   
  #   fig_team
  #   
  # })
  
  
  df_barchart <- reactive({
    final_df%>% dplyr::rename("Dunk rate" = "dunksRate",
                              "Two pointer rate" = "twoPointersRate",
                              "Three pointer rate" = "threePointersRate",
                              "Two pointer success rate" = "twoPointerSuccess",
                              "Three pointer success rate" = "threePointerSuccess") %>%
      tidyr::pivot_longer(cols = c("Dunk rate", 
                                   "Two pointer rate", 
                                   "Three pointer rate", 
                                   "Two pointer success rate", 
                                   "Three pointer success rate"), 
                          names_to = "metric", 
                          values_to = "matric_value") %>% 
      dplyr::filter(season %in% input$seasonsTeam & 
                      metric==input$metricTeam) %>%
      group_by(team) %>%
      summarize(avg_metric_value = mean(matric_value)) %>%
      mutate(choosen_team=ifelse(team==input$selectTeam, "1", "0"),
             mean_metric_league = mean(avg_metric_value))%>% 
      mutate(avg_metric_value_minus_mean = avg_metric_value - mean_metric_league)

  })
  
  
  
  
  
  
  output$barchartTeam <- renderPlot({
    data <- df_barchart()
    
    ggplot2::ggplot(data =data, 
                    mapping = aes(x = forcats::fct_reorder(team, avg_metric_value_minus_mean), y = avg_metric_value_minus_mean, fill=choosen_team#, text = paste0(Area, "\n", Time, ": ", Value)
                    ))+
      geom_bar(stat="identity", alpha=.6) +
      coord_flip()+
      scale_x_discrete(name ="",  position = "top")+
      ggplot2::ggtitle( paste0(input$metricTeam," for ", req(input$selectTeam)," compared to league average ", "(", round(data$mean_metric_league,2), "%)" ) ) +
      ggplot2::ylab("Point percentage differences to the mean")+
      scale_fill_manual( values = c( "1"="red", "0"="#6495ed" ), guide="none" ) +
      ggthemes::theme_hc()+ ggthemes::scale_colour_hc()+
      theme( axis.text = element_text(color="gray20", size=30),
             axis.text.y  = element_text(size=dropdown_size_filters),
             axis.text.x  = element_text(size=dropdown_size_filters),
             legend.text = element_text(color="gray20", size=dropdown_size_filters),
             axis.title = element_text(size=dropdown_size_filters),
             plot.title = element_text(size=dropdown_size_filters)
              )
  })
  
  


# LEAGUE Main panel -------------------------------------------------------

  # get_all_teams_league <- function(team, df){
  #   
  #   get_avg_team <- function(selected_season, selected_team, data){
  #     output_df <- data %>% 
  #       dplyr::filter(team_name == selected_team, season == selected_season) %>%
  #       dplyr::summarise(dunksRate = (sum(distance < 2) / nrow(.)) * 100,
  #                        twoPointersRate = ((sum(shot_type == "three_pointer")) / (nrow(.))) * 100,
  #                        threePointersRate = ((sum(shot_type == "two_pointer")) / (nrow(.))) * 100,
  #                        twoPointerSuccess = ((sum(made == T 
  #                                                  & shot_type == "two_pointer")) / (sum(shot_type == "two_pointer"))) * 100,
  #                        threePointerSuccess = ((sum(made == T 
  #                                                    & shot_type == "three_pointer")) / (sum(shot_type == "three_pointer"))) * 100,
  #                        season = selected_season,
  #                        team = selected_team)
  #     
  #     return(output_df)
  #   }
  #   
  #   seasons <- unique(df$season)
  #   
  #   compare_df <- 
  #     lapply(seasons, get_avg_team, selected_team = team, data = df) %>%
  #     do.call(rbind, .) %>%
  #     unnest(everything()) %>%
  #     as.data.frame()
  #   
  #   return(compare_df)
  # }  
  # 
   unique_teams <- unique(all_nba_data$team_name)
  #   
  # final_df <- 
  #   lapply(unique_teams, get_all_teams_league, df = all_nba_data) %>%
  #   do.call(rbind, .) %>%
  #   unnest(everything()) %>%
  #   as.data.frame()
  # 
  matrixplot_league_metric <- reactive({
    final_df %>%
      dplyr::rename("Dunk rate" = "dunksRate",
                    "Two pointer rate" = "twoPointersRate",
                    "Three pointer rate" = "threePointersRate",
                    "Two pointer success rate" = "twoPointerSuccess",
                    "Three pointer success rate" = "threePointerSuccess") %>%
      tidyr::pivot_longer(cols = c("Dunk rate", 
                                   "Two pointer rate", 
                                   "Three pointer rate", 
                                   "Two pointer success rate", 
                                   "Three pointer success rate"), 
                   names_to = "metric", 
                   values_to = "matric_value") %>% 
      dplyr::filter(metric==input$matrixplot_league_metric) 
    
  })
  
  output$matrixplotLeague <- renderPlot({
   # browser()
    
    metric <- matrixplot_league_metric()
    browser()
     
    
    p <- ggplot2::ggplot(metric, aes(x = season, y = factor(team,levels = rev(sort(unique(team)))), fill = matric_value)) + 
      ggplot2::geom_tile(colour="white", size=1.5, stat="identity") + 
      scale_fill_gradientn("",colours=pals::parula(100)) +
      #viridis::scale_fill_viridis(option="B") +
       #scale_y_continuous(breaks=1:length(unique_teams), labels=unique_teams)+
      ggplot2::xlab("") + 
      ggplot2::ylab("") +
      ggplot2::ggtitle(paste0("Shot evolution for all teams (", req(input$matrixplot_league_metric), ")")) +
      ggplot2::theme(
        plot.title = element_text(color="gray20",hjust=0,vjust=1, size=dropdown_size_filters),
        plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill=NA,color="white", size=0.5, linetype="solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(), 
        axis.text = element_text(color="gray20", size=30),
        axis.text.y  = element_text(hjust=1, size=dropdown_size_filters),
        axis.text.x  = element_text(size=dropdown_size_filters),
        axis.title = element_text(size=dropdown_size_filters),
        legend.text = element_text(color="gray20", size=dropdown_size_filters),
        legend.background = element_rect(fill="white"),
        legend.position = "bottom",
        legend.title=element_blank()
      )+ggplot2::guides(fill = guide_colourbar(barwidth = 50, barheight = 2))
    
    p
    
    
     
  }
  )
  
  
  output$linechart_league <- plotly::renderPlotly({
    plot <- create_leauge_line(data=line_league_data)
    plot <- plot %>% layout(
      xaxis = list(title = list(text="Season", standoff=11)),
      yaxis = list(title = list(text="Indexed amount shots tried", standoff=11)),
      legend = list(title = "Shot Type",
                    orientation = "h",   # show entries horizontally
                    xanchor = "center",  # use center of legend as anchor
                    x = 0.5, y = -0.10),
      title = "Development in average shots tried per game for the entire league",
      clickmode = "event+select",
      showlegend = TRUE
    )
    plot
  })
  
  
  # Sorter alfabetisk
  # Skriv værdi i brackets, men vælg font/size mm. nøjsomt (tænk på luminance)
  # Lav legend bredere
  # Perula color scale kan han godt lide
  
}

shinyApp(ui, server)