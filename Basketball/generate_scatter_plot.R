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
        size = size) + # c("#1b9e77", "#d95f02")
      scale_color_manual(values = c("#66c2a5", "#fc8d62")) + 
      theme(legend.text = element_text(size = rel(1.0)),
            legend.position = c(.95, .95),
            legend.justification = c("center", "bottom"),
            legend.box.just = "center",
            legend.margin = margin(6, 6, 6, 6)) , source = source, width = 975, height = 1025)%>%
    layout(
           legend = list(
      'rgba(0,0,0,0)', 
      orientation = "h",   # show entries horizontally
      xanchor = "center",  # use center of legend as anchor
      x = 0.5, y=0.17,
      title=list(text='', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'),
      bgcolor = 'rgba(0,0,0,0)'
      #font = list(size=2)
    ),
    xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
    )%>%
    config(modeBarButtonsToRemove = c( "zoom2d", "pan2d", "toImage", "hoverCompareCartesian"))
}
