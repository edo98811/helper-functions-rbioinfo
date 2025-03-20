# https://pubs.acs.org/doi/10.1021/acs.jproteome.4c00461

#' Calculate Coefficient of Variation (CV) for Proteomics Quality Control
#'
#' These functions calculate the Coefficient of Variation (CV) for proteomics quality control.
#' 
#' @param row_values A numeric vector of values for which the CV is to be calculated.
#' 
#' @return The CV value as a numeric value.
#' 
#' @details
#' \code{standardCV} calculates the CV using raw counts.
#' \code{geometricCV} calculates the CV using log-transformed data.
#' 
#' @examples
#' # Example usage:
#' raw_counts <- c(10, 12, 15, 20, 25)
#' log_counts <- log(raw_counts)
#' 
#' standardCV(raw_counts)
#' geometricCV(log_counts)
#' 
#' @export
standardCV <- function(row_values) {
  # with raw abundances
  mean_value <- mean(row_values)
  standard_dev <- sd(row_values)
  
  CV <- standard_dev/mean_value
}


#' Calculate Geometric Coefficient of Variation (CV)
#'
#' This function calculates the geometric coefficient of variation (CV) for a given set of row values.
#' The calculation is based on log-transformed data.
#'
#' @param row_values A numeric vector of values for which the geometric CV is to be calculated.
#' @return A numeric value representing the geometric CV.
#' @examples
#' values <- c(1, 2, 3, 4, 5)
#' geometricCV(values)
#' @export
geometricCV <- function(row_values) {
  # with log transformed data
  standard_dev <- sd(row_values)
  
  CV <- sqrt(exp(1)^(standard_dev^2) - 1)
}