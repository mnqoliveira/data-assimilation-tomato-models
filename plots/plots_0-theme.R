# Saving plot theme


theme_vert <- theme(
  plot.title = element_text(size = 10),
  axis.text.x = element_text(angle = 90,
                             hjust = 1, 
                             colour = "black",
                             size = 8),
  axis.text.y = element_text(colour = "black", size = 8),
  axis.title.x = element_text(size = 9,
                              margin = margin(t = 0.1, r = 0, b = 0.1, l = 0,
                                              unit = "cm")),
  axis.title.y = element_text(size = 9, 
                              margin = margin(t = 0, r = 0.1, b = 0, l = 0,
                                              unit = "cm")),
  
  legend.text = element_text(size = 9),
  legend.title  = element_text(size = 9),
  legend.text.align = 1,
  legend.margin = margin(t = -0.5, r = 0, b = 0, l = 0.0, "cm"),
  
  legend.position = "bottom",
  legend.background=element_blank(),
  legend.key = element_blank(),
  
  panel.background = element_rect(fill = "gray95"),
  plot.margin = unit(c(0.1,0.1,0.01,0.1), "cm"),
  
  strip.text.x = element_text(size = 9,
                              margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1,
                                              unit = "cm")),
  strip.text.y = element_text(size = 9),
  
  panel.grid.major = element_line(colour = "grey60", size = 0.1, linetype = 2))

save(theme_vert, file = "./data/plot_theme_vert.RData")

theme_horiz <- theme(
  plot.title = element_text(size = 10),
  axis.text.x = element_text(angle = 90,
                             hjust = 1, 
                             colour = "black",
                             size = 8),
  axis.text.y = element_text(colour = "black", size = 8),
  axis.title.x = element_text(size = 9,
                              margin = margin(t = 0.1, r = 0, b = 0.1, l = 0,
                                              unit = "cm")),
  axis.title.y = element_text(size = 9, 
                              angle = 0, hjust = 1, 
                              margin = margin(t = 0, r = 0.1, b = 0, l = 0,
                                              unit = "cm")),
  
  legend.text = element_text(size = 9),
  legend.title  = element_text(size = 9),
  legend.text.align = 1,
  legend.margin = margin(t = -0.5, r = 0, b = 0, l = 0.0, "cm"),
  
  legend.position = "bottom",
  legend.background=element_blank(),
  legend.key = element_blank(),
  
  panel.background = element_rect(fill = "gray95"),
  plot.margin = unit(c(0.1,0.1,0.01,0.1), "cm"),
  
  strip.text.x = element_text(size = 9,
                              margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1,
                                              unit = "cm")),
  strip.text.y = element_text(size = 9),
  
  panel.grid.major = element_line(colour = "grey60", size = 0.1, linetype = 2))

save(theme_horiz, file = "./data/plot_theme_horiz.RData")
