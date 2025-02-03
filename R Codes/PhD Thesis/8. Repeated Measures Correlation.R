
# This code will calculate rm correlation statistics between the DV and all IVs (with Player as a grouping variable)

# Load "Master File" (all participants' trials and events)

# rm(list = ls())

# Load the desired file
file_path <- file.choose()

library(readr)
data <- read_csv(file_path) 

# Convert a range of columns to numeric
data[, 3:ncol(data)] <- lapply(data[, 3:ncol(data)], as.numeric)
data$Participant <- as.factor(data$Participant)

data <- data[complete.cases(data), ]

# Load necessary library
library(rmcorr)

# Get the numeric column names (excluding Participant and AIM)
numeric_columns <- colnames(data)[!(colnames(data) %in% c("File Name", "Player", "AIM"))]

# Convert Participant to factor
data$Player <- as.factor(data$Player)

# Remove rows with NA values in the relevant columns
clean_data <- data[complete.cases(data[, c("Player", "AIM", numeric_columns)]), ]

# Convert numeric columns to numeric
for (measure in numeric_columns) {
  clean_data[[measure]] <- as.numeric(as.character(clean_data[[measure]]))
  if (any(is.na(clean_data[[measure]]))) {
    warning(paste("Non-numeric values found in variable:", measure, "converted to NA"))
  }
}

# Initialize an empty data frame to store the coefficients
coefficients_df <- data.frame(
  Variable = character(0),  # Initialize an empty character vector
  Correlation = numeric(0),  # Initialize an empty numeric vector
  P_Value = numeric(0)  # Initialize an empty numeric vector for p-values
)

# Calculate the correlation coefficient and p-value for each numeric variable
for (measure in numeric_columns) {
  # Skip columns with only NA values after conversion
  if (all(is.na(clean_data[[measure]]))) {
    warning(paste("Skipping variable with all NA values:", measure))
    next
  }
  
  result <- rmcorr(Player, AIM, measure, dataset = data)
  coefficients_df <- rbind(coefficients_df, data.frame(
    Variable = measure,
    Correlation = result$r,
    P_Value = result$p
  ))
}

# Print the coefficients data frame
print(coefficients_df)


# If you want to save it to a CSV file:
write.csv(coefficients_df, file = "AIM RMCorr (main data).csv", row.names = FALSE)
