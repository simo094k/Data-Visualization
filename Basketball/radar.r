library(tidyverse)
library(plotly)

data <- read.csv("all_nba_data.csv")


radar_data1 <- data %>% 
  dplyr::filter(player == "Ja Morant") %>%
  dplyr::summarise(dunkPerc = sum(distance == 0) / n(),
                   threePointPerc = sum(shot_type == "3-pointer" 
                                        & made == "True") 
                   / sum(shot_type == "3-pointer"),
                   fieldGoalPerc = sum(shot_type == "2-pointer" 
                                       & made == "True") 
                   / sum(shot_type == "2-pointer"),
                   underPressure = sum(made == "True" 
                                       & quarter == "4th quarter" 
                                       & time_remaining <= 5.0 
                                       & (abs(as.integer(strsplit(score, "-")[[1]][1]) 
                                             - as.integer(strsplit(score, "-")[[1]][2]))) <= 8.)
                   / sum(quarter == "4th quarter" 
                         & time_remaining <= 5.0 
                         & (abs(as.integer(strsplit(score, "-")[[1]][1]) 
                                - as.integer(strsplit(score, "-")[[1]][2]))) <= 8.),
                   pointsPerGame = (2*sum(made == "True" 
                                       & shot_type == "2-pointer")
                   + 3*sum(made == "True" 
                           & shot_type == "3-pointer"))
                   / length(unique(date)))

radar_data2 <- data %>% 
  dplyr::filter(player == "Ja Morant") %>%
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
                                - as.integer(strsplit(score, "-")[[1]][2]))) <= 8.),
                   pointsPerGame = (2*sum(made == "True" 
                                          & shot_type == "2-pointer")
                                    + 3*sum(made == "True" 
                                            & shot_type == "3-pointer"))
                   / length(unique(date)))
                   
fig <- plot_ly(
  type = 'scatterpolar',
  r = unlist(radar_data2),
  theta = colnames(radar_data2),
  fill = 'toself'
) 
fig <- fig %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,ceiling(max(radar_data2)))
      )
    ),
    showlegend = F
  )

fig
