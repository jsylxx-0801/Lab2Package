boxplot_drg <- function(data, payment_type = "Average Medicare Payments") {
  if (!payment_type %in% c("Average Medicare Payments", "Average Total Payments", "Average Covered Charges")) {
    stop("Invalid payment type")
  }
  
  ggplot(data, aes_string(x = "DRG Definition", y = payment_type)) +
    geom_boxplot(fill = "lightblue", color = "white") +
    scale_x_discrete(labels = unique(substr(data$`DRG Definition`, 1, 3))) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
    labs(x = "DRG Code",
         y = payment_type,
         title = paste("Plot of", payment_type, "by DRG code"))
}

