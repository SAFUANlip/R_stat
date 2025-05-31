



# Define a reusable function to find best normality transform
find_best_transforms <- function(data) {
  transformed_data <- data
  summary_table <- data.frame(
    Variable = character(),
    Original_p = numeric(),
    Best_Transform = character(),
    Best_p = numeric(),
    Lambda = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (feature in colnames(data)) {
    x <- data[[feature]]
    x <- x[!is.na(x)]
    
    if (length(unique(x)) < 3) next  # Skip near-constant variables
    
    p_original <- tryCatch(shapiro.test(x)$p.value, error = function(e) NA)
    best_p <- p_original
    best_transform <- "None"
    best_data <- x
    best_lambda <- NA
    
    # Try Yeo-Johnson
    yj_model <- tryCatch(powerTransform(x ~ 1, family = "yjPower"), error = function(e) NULL)
    if (!is.null(yj_model)) {
      lambda_yj <- yj_model$lambda
      x_yj <- yjPower(x, lambda_yj)
      p_yj <- tryCatch(shapiro.test(x_yj)$p.value, error = function(e) NA)
      
      if (!is.na(p_yj) && p_yj > best_p) {
        best_p <- p_yj
        best_transform <- "YeoJohnson"
        best_data <- x_yj
        best_lambda <- lambda_yj
      }
    }
    
    # Save best transformed variable
    transformed_data[[feature]] <- best_data
    summary_table <- rbind(summary_table, data.frame(
      Variable = feature,
      Original_p = round(p_original, 4),
      Best_Transform = best_transform,
      Best_p = round(best_p, 4),
      Lambda = round(best_lambda, 4),
      stringsAsFactors = FALSE
    ))
  }
  
  return(list(
    transformed_data = transformed_data,
    summary = summary_table
  ))
}