# Theme

custom_theme <- function(show_x_text = FALSE) {
  
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        axis.line = element_line(linewidth = 0.25, colour = "black"), 
        axis.ticks.x = element_blank(),
        axis.text.x = 
          if (show_x_text)
            element_text(size = 20) 
        else
          element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 22, vjust = 4),
        axis.title.x = element_text(size = 22), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 20), 
        plot.title = element_text(size = 25, hjust = 0.5, vjust = 5), 
        plot.margin = margin(1, 1, 0.5, 1, "cm")
  )
}



treatment_palette <- c(
  "VEH_1" = "#3DAEFF",
  "RG"    = "#F75E05",
  "BAY"   = "#009E73",
  "VEH_2" = "#C837AB",
  "1"     = "#EDB25A", 
  "15"    = "#A56A12"
)


get_treatment_colors <- function(data, var="Tratamiento") {
  present <- unique(as.character(data[[var]]))
  treatment_palette[names(treatment_palette) %in% present]
}