# Assumptions evaluation
assumptions <- function(model) {
  
  par(mfrow = c(1, 2))
  
  res <- resid(model, type = "pearson")
  pred <- predict(model)
  
  # Homoscedasticity - Residuals plot
  plot(x = pred, y = res, 
       xlab = "Predicted", 
       ylab = "Residuals", 
       main = "Residuals vs Predicted")
  abline(0, 0)
  
  # Normality - QQPlot
  qqPlot(res,
         xlab = "Theoretical Quantiles",
         ylab = "Sample Quantiles",
         main = "QQ-Plot")
  s_w <- shapiro.test(res)
  
  par(mfrow = c(1, 1))
  return(s_w)
}


# Descriptive statistics

#' @param data Dataframe
#' @param grups Variables used to group the analysis written as a vector: c("")
#' @param variable Response variable

descriptive <- function(data, grups, variable) {
  data %>% 
    group_by(across(all_of(grups))) %>%
    summarise(n = length(.data[[variable]]), 
              avg = mean(.data[[variable]]), 
              SE = sd(.data[[variable]]), 
              min = min(.data[[variable]]), 
              max = max(.data[[variable]]),
              var = var(.data[[variable]])
              
    ) %>% 
    mutate(sem = SE / sqrt(n))
}