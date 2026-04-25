# Evaluación de supuestos
supuestos <- function(modelo) {
  
  par(mfrow = c(1, 2))
  
  res <- resid(modelo, type = "pearson")
  pred <- predict(modelo)
  
  # Homocedasticidad - Residuos vs Predichos
  plot(x = pred, y = res, 
       xlab = "Predichos", 
       ylab = "Residuos", 
       main = "Residuos vs Predichos")
  abline(0, 0)
  
  # Normalidad - QQPlot
  qqPlot(res,
         xlab = "Cuantiles teóricos",
         ylab = "Cuantiles muestra",
         main = "QQ-Plot")
  s_w <- shapiro.test(res)
  
  par(mfrow = c(1, 1))
  return(s_w)
}


# Estadistica descriptiva

#' @param datos Dataframe
#' @param grupos Variables que se usan para agrupar escrito como vector c() y entre ""
#' @param variable Variable sobre la que se quiere calcular la estadistica descriptiva entre ""

descriptiva <- function(datos, grupos, variable) {
  datos %>% 
    group_by(across(all_of(grupos))) %>%
    summarise(n = length(.data[[variable]]), 
              media = mean(.data[[variable]]), 
              SE = sd(.data[[variable]]), 
              min = min(.data[[variable]]), 
              max = max(.data[[variable]]),
              var = var(.data[[variable]])
              
    ) %>% 
    mutate(sem = SE / sqrt(n))
}