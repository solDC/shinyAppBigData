ggplotMap <- function(california_fortified) {
  ggplot() +
    geom_polygon(data = california_fortified, aes(fill = Count, x = long, y = lat, group = group) , size=0, alpha=0.9) +
    theme_void() +
    scale_fill_viridis(trans = "log", breaks=c(1,3,5,10,20,50,100,200), name="Number of churns", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
    labs(
      title = "California",
      subtitle = "Number of churns per city"
    ) +
    # theme(
    #   text = element_text(color = "#22211d"),
    #   plot.background = element_rect(fill = "#f5f5f2", color = NA),
    #   panel.background = element_rect(fill = "#f5f5f2", color = NA),
    #   legend.background = element_rect(fill = "#f5f5f2", color = NA),
    #   
    #   plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    #   plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    #   plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    #   
    #   legend.position = c(0.3, 0.09)
    # ) +
    coord_map()
}