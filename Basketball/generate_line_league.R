create_leauge_line <- function(data, source=NULL){
  df_reshaped <- data
  
  plot <- plot_ly(df_reshaped, x = ~season, y = df_reshaped[["two_pointer"]], 
                  type = "scatter", mode = "lines+markers", name = "Two pointer",
                  line = list(dash = "dash", color = c("#7570b3")), connectgaps = TRUE,
                  showlegend = TRUE, source = source, 
                  marker = list(color = c("#7570b3")),
                  text = paste("Average amount of two point shots", "<br>", "made per game"))
  
  plot <- plot %>% add_trace(y = df_reshaped[["three_pointer"]], name = "Three pointer", 
                             line = list(dash = "dash", color = c("#d95f02")), connectgaps = T,
                             showlegend = is.numeric(df_reshaped[["three_pointer"]]), 
                             marker = list(color = c("#d95f02")),
                             text = paste("Average amount of three point shots", "<br>", "made per game"))
  
  plot <- plot %>% add_trace(y = df_reshaped[["dunks"]], name = "Dunks", 
                             line = list(dash = "dash", color = c("#1b9e77")), connectgaps = T,
                             showlegend = is.numeric(df_reshaped[["dunks"]]), 
                             marker = list(color = c("#1b9e77")),
                             text = paste("Average amount of dunks", "<br>", "made per game"))
  
  return(plot)
}


# ---- FOR ONLY MADE SHOTS:


#df <- all_nba_data %>%
#  dplyr::filter(made == T) %>%
#  dplyr::group_by(season, shot_type) %>%
#  dplyr::summarise(num_shot = n(), num_games = n_distinct(paste(date, id_team)))
#
#df_dunks <- all_nba_data %>% dplyr::filter((distance < 2) & (made == T)) %>% 
#  dplyr::group_by(season, shot_type) %>%
#  dplyr::summarise(num_shot = as.numeric(n()), 
#                   num_games = as.numeric(n_distinct(paste(date, id_team)))) %>%
#  dplyr::mutate(shot_type = ifelse(shot_type == "two_pointer", "dunks", shot_type))
#
#df_dunks <- df_dunks %>% filter(shot_type == "dunks")
#
#all_combinations <- expand.grid(season = unique(all_nba_data$season), 
#                                shot_type = c("dunks", "two_pointer", 
#                                              "three_pointer"))
#
#
#
#df <- rbind(df, df_dunks)
#
#df_data <- all_combinations %>%
#  dplyr::left_join(df, by = c("season", "shot_type")) %>%
#  dplyr::select(season, shot_type, num_shot, num_games) %>%
#  dplyr::arrange(season)
#
#df_data <- df_data %>% dplyr::distinct()
#
#df_data <- df_data %>%
#  dplyr::group_by(season) %>%
#  dplyr::mutate(num_games = max(num_games, na.rm = T)) %>%
#  dplyr::ungroup()
#
#df_data <- df_data %>% group_by(season, shot_type) %>%
#  dplyr::summarise(std_shots = num_shot / num_games)
#
#df_reshaped <- df_data %>%
#  tidyr::pivot_wider(names_from = shot_type, values_from = std_shots)
#
#df_reshaped <- as.data.frame(df_reshaped)
#
#
#norm_factor <- df_reshaped$dunks[1]/100
#df_reshaped$dunks <- df_reshaped$dunks / norm_factor
#
#norm_factor <- df_reshaped$three_pointer[1]/100
#df_reshaped$three_pointer <- df_reshaped$three_pointer / norm_factor
#
#norm_factor <- df_reshaped$two_pointer[1]/100
#df_reshaped$two_pointer <- df_reshaped$two_pointer / norm_factor
#
#
#plot <- plot_ly(df_reshaped, x = ~season, y = df_reshaped[["two_pointer"]], 
#                type = "scatter", mode = "lines+markers", name = "Two pointer",
#                line = list(dash = "dash", color = c("#7570b3")), connectgaps = TRUE,
#                showlegend = TRUE, source = source, 
#                marker = list(color = c("#7570b3")),
#                text = paste("Average amount of two point shots", "<br>", "made per game"))
#
#plot <- plot %>% add_trace(y = df_reshaped[["three_pointer"]], name = "Three pointer", 
#                           line = list(dash = "dash", color = c("#d95f02")), connectgaps = T,
#                           showlegend = is.numeric(df_reshaped[["three_pointer"]]), 
#                           marker = list(color = c("#d95f02")),
#                           text = paste("Average amount of three point shots", "<br>", "made per game"))
#
#plot <- plot %>% add_trace(y = df_reshaped[["dunks"]], name = "Dunks", 
#                           line = list(dash = "dash", color = c("#1b9e77")), connectgaps = T,
#                           showlegend = is.numeric(df_reshaped[["dunks"]]), 
#                           marker = list(color = c("#1b9e77")),
#                           text = paste("Average amount of dunks", "<br>", "made per game"))
#
#plot
