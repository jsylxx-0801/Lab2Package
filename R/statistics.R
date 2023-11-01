#' @importFrom dplyr summarise
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
#' @export
statistics <- function(data, statistic = "mean") {
  # Check if the input statistic is valid
  if (!(statistic %in% c("mean", "median", "sd"))) {
    stop("Invalid statistic. Please choose 'mean', 'median', or 'sd'.")
  }
  
  # Define the calculations for each statistic
  calculations <- list(
    mean = function(x) mean(x, na.rm = TRUE),
    median = function(x) median(x, na.rm = TRUE),
    sd = function(x) sd(x, na.rm = TRUE)
  )
  
  # Calculate the specified statistic for Average Medicare Payments
  result <- data %>%
    summarise(
      summary_value = calculations[[statistic]](`Average Medicare Payments`)
    ) %>%
    pull(summary_value)
  
  return(result)
}
