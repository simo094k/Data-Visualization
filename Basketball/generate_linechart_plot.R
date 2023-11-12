create_linechart <- function(data, sel_season, source=NULL){
  
  #browser()
  df <- data %>%
    dplyr::group_by(season, shot_type) %>%
    dplyr::summarise(num_shot = n(), num_games = length(unique(date))) 
  
  df_dunks <- data %>% filter(distance < 2) %>% 
    dplyr::group_by(season, shot_type) %>%
    dplyr::summarise(num_shot = n(), num_games = length(unique(date))) %>%
    dplyr::mutate(shot_type = ifelse(shot_type == "two_pointer", "dunks", shot_type))
  
  df <- rbind(df, df_dunks)
  
  all_combinations <- expand.grid(season = sel_season, 
                                  shot_type = c("dunks", "two_pointer", 
                                                "three_pointer"))
  
  df <- all_combinations %>%
    dplyr::left_join(df, by = c("season", "shot_type")) %>%
    dplyr::select(season, shot_type, num_shot, num_games) %>%
    dplyr::arrange(season)
  
  df <- df %>% dplyr::distinct()
  
  df <- df %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(num_games = max(num_games, na.rm = T)) %>%
    dplyr::ungroup()
  
  df <- df %>% group_by(season, shot_type) %>%
    dplyr::summarise(std_shots = num_shot / num_games) %>% 
    dplyr::select(season, shot_type, std_shots)
  
  df_reshaped <- df %>%
    tidyr::pivot_wider(names_from = shot_type, values_from = std_shots)
  
  df_reshaped <- as.data.frame(df_reshaped)
  
  if(all(is.na(df_reshaped$dunks))){
    df_reshaped <- df_reshaped %>% dplyr::mutate(dunks = as.logical(dunks))
    df_reshaped <- df_reshaped %>% dplyr::mutate(dunks = replace_na(dunks, FALSE))
  }
  
  if(all(is.na(df_reshaped$two_pointer))){
    df_reshaped <- df_reshaped %>% 
      dplyr::mutate(two_pointer = as.logical(two_pointer))
    df_reshaped <- df_reshaped %>% 
      dplyr::mutate(two_pointer = replace_na(two_pointer, FALSE))
  }
  
  if(all(is.na(df_reshaped$three_pointer))){
    df_reshaped <- df_reshaped %>% 
      dplyr::mutate(three_pointer = as.logical(three_pointer))
    df_reshaped <- df_reshaped %>% 
      dplyr::mutate(three_pointer = replace_na(three_pointer, FALSE))
  }
  
  df_reshaped <- df_reshaped %>% dplyr::mutate(all_season = FALSE)
  
  
  plot <- plot_ly(df_reshaped, x = ~season, y = df_reshaped[["two_pointer"]], 
                  type = "scatter", mode = "lines+markers", name = "two_pointer",
                  line = list(dash = "dash"), connectgaps = TRUE,
                  showlegend = TRUE, source = source)
  
  plot <- plot %>% add_trace(y = df_reshaped[["three_pointer"]], name = "three_pointer", 
                             line = list(dash = "dash"), connectgaps = T,
                             showlegend = TRUE)
  
  plot <- plot %>% add_trace(y = df_reshaped[["dunks"]], name = "dunks", 
                             line = list(dash = "dash"), connectgaps = T,
                             showlegend = TRUE)
  
  plot <- plot %>% add_trace(y = df_reshaped[["all_season"]],
                             line = list(dash = "dash"), connectgaps = F,
                             showlegend = FALSE)
  
  return(plot)
}
