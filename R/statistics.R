statistics <- function(data, type){
  medicare <- data$'Average Medicare Payments'
  if (type == "mean") {
    return(mean(medicare, na.rm = TRUE))
  } else if (type == "sd") {
    return(sd(medicare, na.rm = TRUE))
  } else if (type == "median") {
    return(median(medicare, na.rm = TRUE))
  } else {
    stop("Invalid type. Choose from 'mean', 'sd', or 'median'.")
  }
}
