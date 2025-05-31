library(MVN) # Can need separate installation of "gsl" software library
library(car)
library(mvtnorm)

b <- read.table('data_sim.txt')
head(b)
dim(b)

pollution <- read.table('pollution.txt')
head(pollution)
dim(pollution)


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
    
    # Try Box-Cox
    if (sum(x <= 0)){
      x_shifted <- x + abs(min(x)) + 1
      print("Shifted for Box-Cox transform")
    }
    else{
      x_shifted <- x
    }
    bc_model <- powerTransform(x_shifted ~ 1, family = "bcPower")
    if (!is.null(bc_model)) {
      lambda_bc <- bc_model$lambda
      x_bc <- bcPower(x_shifted, lambda_bc)
      p_bc <- tryCatch(shapiro.test(x_bc)$p.value, error = function(e) NA)
      
      if (!is.na(p_bc) && p_bc > best_p) {
        best_p <- p_bc
        best_transform <- "Box-Cox"
        best_data <- x_bc
        best_lambda <- lambda_bc
      }
    }
    
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

if (sum(pollution$PM2.5 < 0)){
  print("Have zero negative values")
}

if (sum(b$x < 0)){
  print("Have zero negative values")
}

transformed_res_b <- find_best_transforms(b)
transformed_res_b$summary

transformed_res_pollution <- find_best_transforms(pollution)
transformed_res_pollution$summary

pollution_transformed <- transformed_res_pollution$transformed_data

qqnorm(pollution_transformed$PM2.5)
qqnorm(pollution_transformed$PM10)

qqnorm(pollution$PM2.5)
qqnorm(pollution$PM10)

mvn(pollution)
mvn(pollution_transformed)

qqnorm(transformed_res_b$transformed_data$x)
qqnorm(transformed_res_b$transformed_data$y)
