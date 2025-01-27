
# This code will calculate rm correlation statistics between the DV and all IVs (with Player as a grouping variable)

# Load the long data file (all participants' trials and events)

rm(list = ls())

# Load the desired file
file_path <- file.choose()

library(readxl)
data <- read_excel(file_path) 

# Load the rmcorr library
library(rmcorr)

# Assuming your data frame has the following columns:
# - Player: The player ID
# - Plate_Height: The plate height in meters
# - Other numeric variables (132 in total) representing different measurements

# Get the numeric column names (excluding Player and Plate_Height)
numeric_columns <- colnames(data)[!(colnames(data) %in% c("Player", "Plate Side (m)"))]

# Initialize an empty data frame to store the coefficients
coefficients_df <- data.frame(
  Variable = character(0),  # Initialize an empty character vector
  Correlation = numeric(0),  # Initialize an empty numeric vector
  P_Value = numeric(0)  # Initialize an empty numeric vector for p-values
)

# Calculate the correlation coefficient and p-value for each numeric variable
for (measure in numeric_columns) {
  result <- rmcorr(Player, `Plate Side (m)`, measure, dataset = data)
  coefficients_df <- rbind(coefficients_df, data.frame(
    Variable = measure,
    Correlation = result$r,
    P_Value = result$p
  ))
}

# Print the resulting data frame (optional)
print(coefficients_df)

# If you want to save it to a CSV file:
write.csv(coefficients_df, file = "Plate Side RMCorr.csv", row.names = FALSE)
