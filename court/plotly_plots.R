
library(tidyverse)
library(plotly)

data_path <- "C:/Users/Marc/OneDrive - Aarhus universitet/dataviz/Material_idea_generation/all_nba_data.csv"

df <- read.csv(data_path)

df2 <- df %>% select(c("player", "shotX", "shotY", "date", "shot_type", "distance", "made"))

df_player <- df2 %>% filter(player == "Ja Morant") #   "Stephen Curry"  "Shaquille O'Neal"  "LeBron James"




dist <- c(17.8, 18.8, 19.8, 20.8, 21.8, 22.8, 23.8, 24.8, 25.8, 26.8, 27.8, 
          28.8, 17.2, 18.2, 19.2, 20.2, 21.2, 22.2, 23.2, 24.2, 25.2, 26.2,
          27.2, 28.2, 29.2)


min_dist = min(dist)
max_dist = max(dist)


scatter <- plot_ly(data = df_player, x = ~shotX, y = ~shotY)
scatter

made_df <- df_player %>% filter(made == "True")
not_made_df <- df_player %>% filter(made == "False")

hist <- plot_ly(data = made_df, x = ~distance, type = "histogram",  name = 'Made')
hist <- hist %>% add_trace(data = not_made_df, x = ~distance, type = "histogram", name = 'Not made')
hist <- hist %>% layout(barmode="stack", bargap=0.1)
hist


fig <- plot_ly(data = df_player, x = ~distance, type = "histogram")
fig








fig <- plot_ly(data = df_player, x = ~date, y = ~shotY)
fig








made_df <- df_player %>% filter(made == "True")
not_made_df <- df_player %>% filter(made == "False")



scatter <- plot_ly(data = df_player, x = ~shotX, y = ~shotY) %>%
  add_markers() %>%
  highlight("plotly_selected")


hist <- plot_ly(data = df_player, x = ~distance,  name = 'Made') %>% 
  #add_trace(data = not_made_df, x = ~distance, name = 'Not made') %>% 
  #layout(barmode="stack", bargap=0.1) %>% 
  add_histogram() %>% highlight("plotly_selected")

bscols(scatter, hist)


df_player$made_factor <- as.factor(ifelse(df_player$made == "True", "Made", "Not made"))
df_player2 <- SharedData$new(df_player)

# create a scatterplot
scatter <- plot_ly(df_player2, x = ~shotX, y = ~shotY, color = ~made_factor) %>%
  add_markers(alpha = 0.9) %>%
  highlight("plotly_selected")

# create a histogram
bar <- plot_ly(df_player2, x = ~distance, color = ~made_factor) %>%
  add_histogram() %>%
  layout(barmode = "stack", yaxis = list(title = 'Count')) %>%
  highlight("plotly_selected")

# create a bscols object for side-by-side displays
bscols(scatter, bar)






library(plotly)
library(crosstalk)

# convert 'cyl' to factor and create new levels
mtcars$cyl <- as.factor(mtcars$cyl)
levels(mtcars$cyl) <- c("4 cylinders", "6 cylinders", "8 or more cylinders")

# create a shared data frame
sd <- SharedData$new(mtcars)

# create a scatterplot
scatter <- plot_ly(sd, x = ~wt, y = ~mpg) %>%
  add_markers(alpha = 0.5) %>%
  highlight("plotly_selected")

# create a stacked bar chart
bar <- plot_ly(sd, x = ~cyl, split = ~cyl) %>%
  add_bars() %>%
  layout(barmode = "stack", yaxis = list(title = 'Count')) %>%
  highlight("plotly_selected")

# create side-by-side displays
bscols(scatter, bar)









library(crosstalk)
library(plotly)

# create a shared data frame
sd <- SharedData$new(mtcars)

# create a scatterplot
scatter <- plot_ly(sd, x = ~wt, y = ~mpg) %>%
  add_markers() %>%
  highlight("plotly_selected")

# create a histogram
hist <- plot_ly(sd, x = ~cyl) %>%
  add_histogram(histnorm = "percent") %>%
  layout(bargap=0.05) %>%
  highlight("plotly_selected")

# create a bscols object for side-by-side displays
bscols(scatter, hist)




library(crosstalk)
library(plotly)

# create a shared data frame
sd <- SharedData$new(mtcars)

# create a scatterplot
scatter <- plot_ly(sd, x = ~wt, y = ~mpg) %>%
  add_markers() %>%
  highlight("plotly_selected")

# create a histogram
hist <- plot_ly(sd, x = ~mpg) %>%
  add_histogram(histnorm = "percent") %>%
  layout(bargap=0.05) %>%
  highlight("plotly_selected")

# create a bscols object for side-by-side displays
bscols(scatter, hist)
