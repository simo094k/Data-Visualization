create_linechart <- function(data, source=NULL){
  df <- data %>%
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
                      source = source)
      flag <- F
    }
    else{
      plot <- plot %>% add_trace(y = df_reshaped[[avail_names[idx]]], name = avail_names[idx], 
                                 line = list(dash = "dash"), connectgaps = TRUE)
    }
  }
  return(plot)
}