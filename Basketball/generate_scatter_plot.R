create_scatter <- function(df_player, court, alpha = 0.8, size = 0.2, source=NULL){
  ggplotly(
    court +
      geom_point(
        data=df_player,
        aes(x = shotX, 
            y = shotY,
            color = made_factor
            ), 
        alpha = alpha,
        size = size)+
      theme(legend.text = element_text(size = rel(0.6)) ) , source = source, width = 975, height = 1000)%>%
    layout(legend = list(
      orientation = "h",
      title=list(text='')
      #font = list(size=2)
    ),
    xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
    )%>%
    config(modeBarButtonsToRemove = c( "zoom2d", "pan2d", "toImage", "hoverCompareCartesian"))
}
