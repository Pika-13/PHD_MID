#Code for graph visualization

theme_JRR <- function() { 
  theme(  #create theme
  plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  plot.background = element_blank(),
  axis.title.y = element_text(size = 14, face = "bold"), 
  axis.title.x = element_text(size = 14, face = "bold"), 
  axis.ticks = element_line(color = "black", linewidth = 0.5),
  axis.ticks.length = unit(0.1, "cm"),     # short ticks
  axis.text = element_text(size = 12),
  axis.line = element_line(color = "black", linewidth = 0.8),
  legend.title = element_text(size = 12, face = "bold"),  
  legend.text = element_text(size = 11),  
  legend.position = "bottom", legend.direction = "vertical",
  legend.background = element_rect(color = "gray80", fill = "gray95", linewidth = 0.3),
  legend.justification = c(1, 0.5), 
  legend.margin = margin(10, 10, 10, 30))
}