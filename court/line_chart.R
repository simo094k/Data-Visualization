load("data/basketball.RData")

library(plotly)
library(ggplot2)
library(tidyverse)


change_names <- list("2-pointer" = "two_pointer",
                     "3-pointer" = "three_pointer")
sel_season <- c("2008/09", "2009/10", "2010/11", "2011/12", "2018/19")



jo <- all_nba_data %>%
  dplyr::filter(player == "Kobe Bryant", season == sel_season) %>% 
  dplyr::mutate(shot_type = ifelse(shot_type == "2-pointer", "two_pointer", 
                            "three_pointer"))
df <- jo %>%
  group_by(season, shot_type) %>%
  summarise(num_shot = n(), num_games = length(unique(date))) 



df_dunks <- jo %>% filter(distance > 2) %>% 
  group_by(season, shot_type) %>%
  summarise(num_shot = n(), num_games = length(unique(date))) %>%
  mutate(shot_type = ifelse(shot_type == "two_pointer", "dunks", shot_type))

df <- rbind(df, df_dunks)

all_combinations <- expand.grid(season = sel_season, 
                                shot_type = c("dunks", "two_pointer", 
                                              "three_pointer"))

df <- all_combinations %>%
  left_join(df, by = c("season", "shot_type")) %>%
  select(season, shot_type, num_shot, num_games) %>%
  arrange(season)

df <- df %>% dplyr::distinct()


df <- df %>%
  group_by(season) %>%
  mutate(num_games = max(num_games, na.rm = T)) %>%
  ungroup()

df <- df %>% group_by(season, shot_type) %>%
  summarise(std_shots = num_shot / num_games) %>% 
  select(season, shot_type, std_shots)


df_reshaped <- df %>%
  pivot_wider(names_from = shot_type, values_from = std_shots)

df_reshaped <- as.data.frame(df_reshaped)


if(all(is.na(df_reshaped$dunks))){
  df_reshaped <- df_reshaped %>% mutate(dunks = as.logical(dunks))
  df_reshaped <- df_reshaped %>% mutate(dunks = replace_na(dunks, FALSE))
}


if(all(is.na(df_reshaped$two_pointer))){
  df_reshaped <- df_reshaped %>% mutate(two_pointer = as.logical(two_pointer))
  df_reshaped <- df_reshaped %>% 
    mutate(two_pointer = replace_na(two_pointer, FALSE))
}

if(all(is.na(df_reshaped$three_pointer))){
  df_reshaped <- df_reshaped %>% 
    mutate(three_pointer = as.logical(three_pointer))
  df_reshaped <- df_reshaped %>% 
    mutate(three_pointer = replace_na(three_pointer, FALSE))
}

df_reshaped <- df_reshaped %>% mutate(all_season = FALSE)




plot <- plot_ly(df_reshaped, x = ~season, y = df_reshaped[["two_pointer"]], 
                type = "scatter", mode = "lines+markers", name = "two_pointer",
                line = list(dash = "dash"), connectgaps = TRUE,
                showlegend = TRUE)

plot <- plot %>% add_trace(y = df_reshaped[["three_pointer"]], name = "three_pointer", 
                           line = list(dash = "dash"), connectgaps = T,
                           showlegend = TRUE)

plot <- plot %>% add_trace(y = df_reshaped[["dunks"]], name = "dunks", 
                           line = list(dash = "dash"), connectgaps = T,
                           showlegend = TRUE)

plot <- plot %>% add_trace(y = df_reshaped[["all_season"]],
                           line = list(dash = "dash"), connectgaps = F,
                           showlegend = FALSE)

plot <- plot %>% layout(
  xaxis = list(title = "Season"),
  yaxis = list(title = "Number of Shots Made"),
  legend = list(title = "Shot Type"),
  title = "2- and 3-Pointer shot average per game"
)

plot











# Your dataframe
df_reshaped <- data.frame(
  season = c("2008/09", "2009/10", "2010/11", "2011/12"),
  dunks = c(NA, NA, NA, NA),
  three_pointer = c(NA, 1.06, 1.11, 1.19),
  two_pointer = c(NA, 4.17, 3.89, 4.31)
)

# Create an empty trace for "dunks" to show in the legend
empty_trace <- plot_ly(x = df_reshaped$season, y = rep(NA, nrow(df_reshaped)),
                       type = "scatter", mode = "lines+markers",
                       name = "dunks (No Data)", line = list(dash = "dash"),
                       connectgaps = FALSE, showlegend = TRUE)

# Create the other traces for "two_pointer" and "three_pointer"
two_pointer_trace <- plot_ly(df_reshaped, x = ~season, y = ~two_pointer,
                             type = "scatter", mode = "lines+markers",
                             name = "two_pointer", line = list(dash = "dash"),
                             connectgaps = TRUE, showlegend = TRUE)

three_pointer_trace <- plot_ly(df_reshaped, x = ~season, y = ~three_pointer,
                               type = "scatter", mode = "lines+markers",
                               name = "three_pointer", line = list(dash = "dash"),
                               connectgaps = TRUE, showlegend = TRUE)

# Combine the traces
plot <- empty_trace %>%
  add_trace(two_pointer_trace) %>%
  add_trace(three_pointer_trace)

# Layout settings
plot <- plot %>% layout(
  xaxis = list(title = "Season"),
  yaxis = list(title = "Number of Shots Made"),
  legend = list(title = "Shot Type"),
  title = "2- and 3-Pointer shot average per game"
)

# Display the plot
plot












library(plotly)

# Sample data frame
df_reshaped <- data.frame(
  season = c("2008/09", "2009/10", "2010/11", "2011/12"),
  dunks = c(NA, NA, NA, NA),
  three_pointer = c(NA, 1.062500, 1.109890, 1.185714),
  two_pointer = c(NA, 4.166667, 3.890110, 4.314286)
)

# Create a factor variable for the season
df_reshaped$season_factor <- factor(df_reshaped$season)

# Create the initial plot
plot <- plot_ly(df_reshaped, x = ~season_factor, y = ~two_pointer, type = "scatter", 
                mode = "lines+markers", name = "two_pointer",
                line = list(dash = "dash"), connectgaps = TRUE)

plot <- plot %>% add_trace(x = ~season_factor, y = ~three_pointer, name = "three_pointer", 
                           line = list(dash = "dash"), connectgaps = TRUE)

# Add the "dunks" trace with a legend entry
plot <- plot %>% add_trace(x = ~season_factor, y = ~dunks, name = "dunks", 
                           line = list(dash = "dash"), connectgaps = TRUE,
                           showlegend = TRUE)  # Specify showlegend to display the legend entry

# Customize the layout
plot <- plot %>% layout(
  xaxis = list(title = "Season"),
  yaxis = list(title = "Number of Shots Made"),
  legend = list(title = "Shot Type"),
  title = "2- and 3-Pointer shot average per game"
)

# Display the plot
plot








data_names <- df_reshaped %>% names()
avail_names <- c()

for(name in data_names){
  new_name <- change_names[[name]]
  if(!is.null(new_name)){
    df_reshaped <- df_reshaped %>% dplyr::rename(!!new_name := name)
    avail_names <- c(new_name, avail_names)
  }
}




avail_names <- avail_names[1]



avail_names <- c("two_pointer", "three_pointer", "dunks")


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




# Convert the "season" column to date format
# df$season <- as.Date(paste0("01-", df$season), format = "%d-%Y/%y")





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