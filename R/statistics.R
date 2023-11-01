#' @importFrom dplyr summarise
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
#' @export
statistics <- function(data, stat_type = "mean") {
  # Check if the input statistic is valid
  if (!(stat_type %in% c("mean", "median", "sd"))) {
    stop("Invalid statistic. Please choose 'mean', 'median', or 'sd'.")
  }
  
  # Define the calculations for each statistic
  calculate <- list(
    mean = function(x) mean(x, na.rm = TRUE),
    median = function(x) median(x, na.rm = TRUE),
    sd = function(x) sd(x, na.rm = TRUE)
  )
  
  # Calculate the specified statistic for Average Medicare Payments
  result <- data %>%
    summarise(
      stat_value = calculate[[stat_type]](`Average Medicare Payments`)
    ) %>%
    pull(stat_value)
  
  return(result)
}
