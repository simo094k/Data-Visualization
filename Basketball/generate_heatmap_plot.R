create_heatmap <- function(df_player, court, court_theme=court_themes$light, source=NULL){
  ggplotly(
    court +
      geom_point(  # use scatterplot ontop of heatmap to select points :D
        data = df_player,
        aes(x = shotX, 
            y = shotY), 
        alpha = 0) +
      stat_density_2d_filled(
        data = df_player,
        aes(x = shotX, y = shotY, fill = stat(density / max(density)) ),
        geom = "raster", contour = FALSE, interpolate = TRUE, n = 200
      ) +
      geom_path(
        data = court_points,
        aes(x = x, y = y, group = desc),
        color = court_theme$lines
      ) +
      #scale_fill_distiller(palette=4, direction=1) +
      scale_fill_viridis_c(
        "",
        limits = c(0, 1),
        breaks = c(0, 1),
        labels = c("Low", "High"),
        option = "inferno",
        #option = "magna",
        guide = guide_colorbar(barwidth = 15),
        trans = "sqrt",na.value = "white"
      ) +
      theme(
        legend.text = element_text(size = rel(1.0)) 
      ),width = 950, height = 975, source=source)%>%
    layout(
    #   legend = list(
    #   #orientation = "h",
    #   bgcolor = 'rgba(0,0,0,0)'
    # ),
    title=list(text='Shot frequency', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'),
    xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)
    )%>%
    config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d", "toImage", "hoverClosestGl2d", "hoverCompareCartesian", "hoverClosestCartesian"))
  
}
