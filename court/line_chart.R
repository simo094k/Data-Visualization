


load("data/basketball.RData")

library(plotly)
library(ggplot2)
library(tidyverse)


change_names <- list("2-pointer" = "two_pointer",
                     "3-pointer" = "three_pointer")

jo <- all_nba_data %>%
  dplyr::filter(player == "Kobe Bryant")


df <- jo %>%
  group_by(season, shot_type) %>%
  summarise(num_shot = n(), num_games = length(unique(date))) 

df <- df %>%
  group_by(season) %>%
  mutate(num_games = max(num_games)) %>%
  ungroup()

df <- df %>% group_by(season, shot_type) %>%
  summarise(std_shots = num_shot / num_games) %>% 
  select(season, shot_type, std_shots)
  


# Convert the "season" column to date format
# df$season <- as.Date(paste0("01-", df$season), format = "%d-%Y/%y")


df_reshaped <- df %>%
  pivot_wider(names_from = shot_type, values_from = std_shots)

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

avail_names <- avail_names[1]



flag <- T
for(idx in 1:length(avail_names)){
  if(flag){
    plot <- plot_ly(df_reshaped, x = ~season, y = df_reshaped[[avail_names[idx]]], type = "scatter", 
                    mode = "lines+markers", name = avail_names[idx],
                    line = list(dash = "dash"), connectgaps = TRUE)
    flag <<- F
  }
  else{
    print("jo")
    plot <- plot %>% add_trace(y = df_reshaped[[avail_names[idx]]], name = avail_names[idx], 
                               line = list(dash = "dash"), connectgaps = TRUE)
  }
}


plot <- plot %>% layout(
  xaxis = list(title = "Season"),
  yaxis = list(title = "Number of Shots Made"),
  legend = list(title = "Shot Type"),
  title = "2- and 3-Pointer shot average per game"
)

plot







plot <- plot_ly(df_reshaped, x = ~season, y = ~df_reshaped[[avail_names[1]]], type = "scatter", 
                mode = "lines+markers", name = avail_names[1],
                line = list(dash = "dash"), connectgaps = TRUE)

plot <- plot %>% add_trace(y = ~df_reshaped[[avail_names[2]]], name = avail_names[2], 
                           line = list(dash = "dash"), connectgaps = TRUE)



  
plot_ly(df_reshaped, x = ~season, y = ~two_pointer, type = "scatter", mode = "lines+markers", name = "2-pointer",
        line = list(dash = "dash"), connectgaps = TRUE) %>%
  add_trace(y = ~three_pointer, name = "3-pointer", line = list(dash = "dash"), connectgaps = TRUE) %>%
  layout(
    xaxis = list(title = "Season"),
    yaxis = list(title = "Number of Shots Made"),
    legend = list(title = "Shot Type"),
    title = "2- and 3-Pointer shot average per game"
  )
  





df <- data.frame(
  season = c("2009/10", "2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16"),
  two = c(1647, 1411, 1290, 1188, 57, 529, 646),
  three = c(425, 389, 344, 403, 16, 183, 464)
)
  








packages = c("shiny", "ggplot2", "hexbin", "dplyr", "httr", "jsonlite")
install.packages(packages, repos = "https://cran.rstudio.com/")
library(shiny)
runGitHub("ballr", "toddwschneider")