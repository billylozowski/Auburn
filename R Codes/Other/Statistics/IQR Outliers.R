# Remove outliers with the Z-score method
# Do this for each variable of interest

Q1 <- quantile(your_data$variable, 0.25)
Q3 <- quantile(your_data$variable, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
your_data <- your_data[your_data$variable >= lower_limit & your_data$variable <= upper_limit, ]