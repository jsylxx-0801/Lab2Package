boxplot <- function(data, payment = "Average Medicare Payments") {
  if (payment != "Average Medicare Payments" &
        payment != "Average Total Payments" &
        payment != "Average Covered Charges") {
    stop("Invalid payment type")
  }


# Boxplot

 NULL %>%
   ggplot(aes(x = data[["DRG Definition"]],
              y = data[[payment]])) +
   geom_boxplot(fill = "lightblue",
                color = "black") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 90,
                                    hjust = 1,
                                    size = 6),
         plot.title = element_text(hjust = 0.5,
                                   size = 15)) +
   labs(x = "DRG Code",
        y = payment,
        title = paste0("Boxplot of ",
                       payment,
                       " and DRG Code"))
}
