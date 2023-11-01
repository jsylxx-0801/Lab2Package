#' @importFrom dplyr %>%
#' @export
statistics <- function(data, type) {
  if (type != 'mean' && type != 'median' && type != 'sd') {
    stop("Invalid type. Choose from 'mean', 'sd', or 'median'.")
  }
  
  info <- data %>%
    group_by(DRG.Definition) %>%
    summarise(
      summary_value = get(type)(Average.Medicare.Payments, na.rm = TRUE)
    )
  
  return(info)
}
