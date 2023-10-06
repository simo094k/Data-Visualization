create_heatmap <- function(df_player, court, court_theme=court_themes$light){
  ggplotly(
    court +
      stat_density_2d_filled(
        data = df_player,
        aes(x = shotX - mean(shotX), y = shotY, fill = stat(density / max(density))),
        geom = "raster", contour = FALSE, interpolate = TRUE, n = 200
      ) +
      geom_path(
        data = court_points,
        aes(x = x, y = y, group = desc),
        color = court_theme$lines
      ) +
      #scale_fill_distiller(palette=4, direction=1) +
      scale_fill_viridis_c(
        "Shot Frequency    ",
        limits = c(0, 1),
        breaks = c(0, 1),
        labels = c("lower", "higher"),
        option = "magna",
        guide = guide_colorbar(barwidth = 15)
      ) +
      theme(
        legend.text = element_text(size = rel(0.6)) 
      ))
  
}