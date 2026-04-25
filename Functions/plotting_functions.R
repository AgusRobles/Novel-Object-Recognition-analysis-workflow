
# Graphical parameters 
plot_params <- list(
  jitter_size = 5,
  jitter_alpha = 0.8,
  jitter_width = 0.1, 
  line_width = 0.8,
  mean_size = 6,
  error_width = 0.05
)

shapes <- c("F" = 17, "M" = 16)


# Plotting functions

.build_exploration_plot <- function(datos, desc, params) {
  
  ggplot(
    datos,
    aes(
      x = interaction(Type_object, Treatment),
      y = Exploration,
      group = Animal,
      color = Treatment, 
      shape = Sex
    )
  ) +
    
    geom_jitter(
      size = params$jitter_size,
      alpha = params$jitter_alpha,
      position = position_dodge()
    ) +
    
    geom_line(
      aes(group = Animal), 
      linewidth = params$line_width, 
      alpha = params$jitter_alpha, 
      position = position_dodge()
    ) + 
    
    geom_point(
      desc, 
      mapping = aes(x = interaction(Type_object, Treatment), 
                    y = avg, group = Treatment), 
      color = "black", 
      size = params$mean_size, 
      inherit.aes = F
    ) +
    
    geom_errorbar(
      desc, 
      mapping = aes(
        x = interaction(Type_object, Treatment),
        y = avg, 
        ymin = avg - sem, 
        ymax = avg + sem, 
        group = Type_object), 
      width = params$error_width, 
      linewidth = params$line_width, 
      color = "black", 
      inherit.aes = F
    ) +
    
    geom_line(
      desc, 
      mapping = aes(
        x = interaction(Type_object, Treatment),
        y = avg, 
        group = Treatment), 
      color = "black", 
      linewidth = params$line_width, 
      inherit.aes = F
    ) +
    
    labs(x = "", 
         y = "Exploration (s)"
    ) +
    
    scale_x_discrete(
      labels = c("Fam", "Nov", "Fam", "Nov")
    )
  
}


.build_DI_plot <- function(datos, desc, params) {
  
  ggplot(
    datos,
    aes(
      x = Treatment,
      y = beta_di * 100,
      color = Treatment, 
      shape = Sex
    )
  ) +
    geom_jitter(size = params$jitter_size,
                alpha = params$jitter_alpha,
                width = params$jitter_width) +
    
    geom_point(desc, 
               mapping = aes(x = Treatment, y = avg * 100, group = Treatment), 
               color = "black", 
               size = params$mean_size, 
               inherit.aes = F) +
    
    geom_errorbar(desc, 
                  mapping = aes(x = Treatment, y = avg * 100, ymin = (avg - sem) * 100, ymax = (avg + sem) * 100, width = params$error_width), 
                  linewidth = params$line_width, 
                  color = "black", 
                  inherit.aes = F) +
    
    geom_hline(yintercept = 50, linetype = "dashed") +
    
    labs(x = "", 
         y = "DI (%)"
    )
}


.build_total_plot <- function(datos, desc, params, y_var, y_label = "Exploration (s)") {

  
  ggplot(
    datos,
    aes(
      x = Treatment,
      y = !!y_var,
      color = Treatment, 
      shape = Sex
    )
  ) +
    geom_jitter(size = params$jitter_size, width = params$jitter_width, alpha = params$jitter_alpha) +
    
    geom_point(desc, mapping = aes(x = Treatment, y = avg, group = Treatment), color = "black", size = params$mean_size, inherit.aes = F) +
    
    geom_errorbar(desc, mapping = aes(x = Treatment, y = avg, ymin = avg - sem, ymax = avg + sem, width = params$error_width), linewidth = params$line_width, color = "black", 
                  inherit.aes = F) +
    
    labs(x = "", 
         y = y_label)
}




# Axis detector

.detect_interaction_x <- function(p) {
  
  x_mapping <- rlang::get_expr(p$mapping$x)
  
  grepl("interaction", deparse(x_mapping))
}


# Plot selector

plot_behavior <- function(type,
                          datos,
                          desc,
                          y_var = NULL,
                          y_limits = NULL, 
                          colors = treatment_colors,
                          params = plot_params) {
  
  y_var <- rlang::enquo(y_var)
  
  p <- switch(
    type,
    
    exploration = .build_exploration_plot(datos, desc, params),
    DI          = .build_DI_plot(datos, desc, params),
    total       = .build_total_plot(datos, desc, params, y_var),
    
    stop("Unknown plot type")
  )
  
  p <- p +
    scale_color_manual(values = get_treatment_colors(datos)) +
    scale_shape_manual(values = shapes)
  
  show_x <- .detect_interaction_x(p)
  
  p <- p +
    custom_theme(show_x_text = show_x)
  
  
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  return(p)
}


# Plot types
plot_types <- c("exploration", "DI", "total")



