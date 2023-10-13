library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(dplyr)

source("court_plot.R")

#load("data/basketball.RData") #Load environment to get the necessary data
dict <<- list("0" = "2-pointer", "1" = "3-pointer")
change_names <<- list("2-pointer" = "two_pointer",
                      "3-pointer" = "three_pointer")

#all_nba_data <- all_nba_data %>% mutate(shotX = shotX - 24)
all_nba_data <- all_nba_data%>%
  dplyr::mutate(quarter=dplyr::case_when(grepl("overtime", quarter)==T ~ "Overtime", TRUE ~ quarter),
                made_factor = as.factor(ifelse(made == T, "Made", "Not made")))


ui <- fluidPage(
  titlePanel("Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Add any input controls if needed
    ),
    mainPanel(
      plotlyOutput("scatterplot"),
      plotlyOutput("line_chart")
    )
  )
)


server <- function(input, output, session) {
  #In the server, the fun happens. Here, we define what is actually shown.
  # It is a very good idea to keep it as structured as possible, and if the code
  # for each plot becomes to large, one can consider to place it in a seperate
  # file and refer to that file here instead.
  
  
  #------- Main panel --------------
  
  df_players <- reactive({
    all_nba_data %>%
      dplyr::filter(player == "Kobe Bryant")  # "Ja Morant"
    
    #all_nba_data %>% mutate(shotX = shotX - round(mean(shotX), 1))
    #print(all_nba_data)
  })
  
  # Create the scatterplot
  output$scatterplot <- renderPlotly({
    df_player <- df_players()
    scatter <- ggplotly(
                          plot_court() +
                          geom_point(
                            data=df_player,
                            aes(x = shotX, 
                                y = shotY,
                                color = made_factor), 
                            alpha = 0.8,
                            size = 0.2), source = "scatter_selected")

    
    
    scatter <- scatter%>%
      # add_segments(x = 0, xend = 0, y = -4, yend = 23.75, 
      #              line = list(color = 'black', width = 2, dash = 'dash'),
      #              showlegend = FALSE) %>%
      layout(
        clickmode = "event+select",
        xaxis = list(range=list(-40,90)
                     # scaleanchor = "y", 
                     # scaleratio = 1,
                     # fixedrange = F
        ),
        yaxis=list(range=list(-4,47.75))
      )
  })
  
  output$line_chart <- renderPlotly({
    df <- df_players() %>%
      dplyr::group_by(season, shot_type) %>%
      dplyr::summarise(num_shot = n(), num_games = length(unique(date))) 
    
    df <- df %>%
      dplyr::group_by(season) %>%
      dplyr::mutate(num_games = max(num_games)) %>%
      dplyr::ungroup()
    
    
    df <- df %>% group_by(season, shot_type) %>%
      dplyr::summarise(std_shots = num_shot / num_games) %>% 
      dplyr::select(season, shot_type, std_shots)
    
    df_reshaped <- df %>%
      tidyr::pivot_wider(names_from = shot_type, values_from = std_shots)
    
    data_names <- df_reshaped %>% names()
    avail_names <- c()
    
    for(name in data_names){
      new_name <- change_names[[name]]
      if(!is.null(new_name)){
        df_reshaped <- df_reshaped %>% dplyr::rename(!!new_name := name)
        avail_names <- c(new_name, avail_names)
      }
    }
    df_reshaped <- as.data.frame(df_reshaped)
    flag <- T
    for(idx in 1:length(avail_names)){
      if(flag){
        plot <- plot_ly(df_reshaped, x = ~season, y = df_reshaped[[avail_names[idx]]], type = "scatter", 
                        mode = "lines+markers", name = avail_names[idx],
                        line = list(dash = "dash"), connectgaps = TRUE,
                        source = "line_trace")
        flag <- F
      }
      else{
        plot <- plot %>% add_trace(y = df_reshaped[[avail_names[idx]]], name = avail_names[idx], 
                                   line = list(dash = "dash"), connectgaps = TRUE)
      }
    }
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
        df <- selected_data %>%
          dplyr::group_by(season, shot_type) %>%
          dplyr::summarise(num_shot = n(), num_games = length(unique(date))) 
        
        df <- df %>%
          dplyr::group_by(season) %>%
          dplyr::mutate(num_games = max(num_games)) %>%
          dplyr::ungroup()
        
        
        df <- df %>% group_by(season, shot_type) %>%
          dplyr::summarise(std_shots = num_shot / num_games) %>% 
          dplyr::select(season, shot_type, std_shots)
        
        df_reshaped <- df %>%
          tidyr::pivot_wider(names_from = shot_type, values_from = std_shots)
        
        
        data_names <- df_reshaped %>% names()
        avail_names <- c()
        
        for(name in data_names){
          new_name <- change_names[[name]]
          if(!is.null(new_name)){
            df_reshaped <- df_reshaped %>% dplyr::rename(!!new_name := name)
            avail_names <- c(new_name, avail_names)
          }
        }
        df_reshaped <- as.data.frame(df_reshaped)
        flag <- T
        for(idx in 1:length(avail_names)){
          if(flag){
            plot <- plot_ly(df_reshaped, x = ~season, y = df_reshaped[[avail_names[idx]]], type = "scatter", 
                            mode = "lines+markers", name = avail_names[idx],
                            line = list(dash = "dash"), connectgaps = TRUE,
                            source = "line_trace")
            flag <- F
          }
          else{
            plot <- plot %>% add_trace(y = df_reshaped[[avail_names[idx]]], name = avail_names[idx], 
                                       line = list(dash = "dash"), connectgaps = TRUE)
          }
        }
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
        
        ggplotly(
            plot_court() + 
            geom_point(
              data=selected_data,
              aes(x = shotX, 
                  y = shotY,
                  color = made_factor), 
              alpha = 0.8,
              size = 0.2), source = "scatter_selected") %>%
          layout(
            clickmode = "event+select"
          )
      })
    }
  })
}

shinyApp(ui, server)