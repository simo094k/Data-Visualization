create_scatter <- function(df_player, court, alpha = 0.8, size = 0.2, source=NULL){
  ggplotly(
    court +
      geom_point(
        data=df_player,
        aes(x = shotX - mean(shotX), 
            y = shotY,
            color = made_factor), 
        alpha = alpha,
        size = size), source = source)
}