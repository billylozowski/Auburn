
# Load the necessary packages
library(readr)
library(rmcorr)
library(dplyr)

# Load "Cleaned Master File" (all participants' trials and events)
file_path <- file.choose()

data <- read_csv(file_path) 

# # transform shoulder abduction @ MAW
# data$`Shoulder Abduction @ MAW - t` <- data$`Shoulder Abduction @ MAW` ^2

# Convert a range of columns to numeric, starting from column 3 onward
data[, 3:ncol(data)] <- lapply(data[, 3:ncol(data)], as.numeric)

# Convert Player column to a factor
data$Player <- as.factor(data$Player)

# Get the numeric column names (excluding File Name, Participant, and AIM)
numeric_columns <- colnames(data)[!(colnames(data) %in% c("File Name", "Player"))]

# Initialize an empty data frame to store the coefficients
coefficients_df <- data.frame(
  Variable = character(0),  # Initialize an empty character vector
  Correlation = numeric(0),  # Initialize an empty numeric vector
  P_Value = numeric(0)  # Initialize an empty numeric vector for p-values
)

# Calculate the correlation coefficient and p-value for each numeric variable
for (measure in numeric_columns) {
  # Subset data to remove NA values just for the current variable and AIM, and exclude the first two columns
  subset_data <- data[!is.na(data[[measure]]) & !is.na(data$AIM), c("Player", "AIM", measure)]
  
  # Skip if the subsetted data has too few data points
  if (nrow(subset_data) < 2) {
    warning(paste("Skipping variable due to insufficient non-NA values:", measure))
    next
  }
  
  # Run rmcorr on the subset data
  result <- rmcorr(Player, AIM, measure, dataset = subset_data)
  coefficients_df <- rbind(coefficients_df, data.frame(
    Variable = measure,
    Correlation = result$r,
    P_Value = result$p
  ))
}

# Save as a .csv file
write.csv(coefficients_df, file = "AIM RMCorr (main data).csv", row.names = FALSE)


############################## CORRELATION MATRIX ##############################

# library(corrplot)
# 
# # Subset the data to only include numeric columns (excluding specific columns)
# numeric_data <- data[, !(colnames(data) %in% c("File Name", "Player"))]
# 
# # Calculate the correlation matrix
# correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
# 
# # Save the correlation matrix to a CSV file
# write.csv(correlation_matrix, file = "Aim 3 Correlation Matrix.csv", row.names = TRUE)


################################################################################

# Initialize an empty matrix to store correlations
rmcorr_matrix <- matrix(NA, nrow = length(numeric_columns), ncol = length(numeric_columns))
colnames(rmcorr_matrix) <- numeric_columns
rownames(rmcorr_matrix) <- numeric_columns

# Loop through each pair of variables
for (i in seq_along(numeric_columns)) {
  for (j in seq_along(numeric_columns)) {
    if (i != j) {  # Skip self-correlations
      # Subset the data for the current pair of variables
      var1 <- numeric_columns[i]
      var2 <- numeric_columns[j]
      subset_data <- data[!is.na(data[[var1]]) & !is.na(data[[var2]]), c("Player", var1, var2)]
      
      # Check if there's enough data to calculate the correlation
      if (nrow(subset_data) > 1) {
        # Calculate repeated-measures correlation
        result <- rmcorr("Player", var1, var2, dataset = subset_data)
        rmcorr_matrix[i, j] <- result$r  # Store correlation coefficient
      }
    }
  }
}

 # Save the correlation matrix to a CSV file
 write.csv(rmcorr_matrix, file = "Aim 3 RM Correlation Matrix.csv", row.names = TRUE, na = "")
 
 # Save identified variables for mixed model analysis
 df_lme4 <- data %>%
   select(1:8, `Trunk Flexion @ BR`, `COM Velocity (X) (MAW to BR)`, `Trunk Flexion Velocity (MAW to BR)`, 
          `Trunk Flexion Velocity (max)`, `Trunk Flexion Velocity (BFC to FFC)`, `Lateral Trunk Flexion (BFC to FFC)`, 
          `Trunk Rotation (Y) @ BR`, `Trunk Flexion (BR to EFT)`, `Front Knee Extension Velocity @ BR`,
          `Stride Length (% Height) @ FFC`, `Trunk Flexion (MAW to BR)`, `Trunk Rotation (Y) (BR to EFT)`,  
          `Shoulder Abduction @ MAW`, `Shoulder H Abd Velocity (max)`)


write.csv(df_lme4, file = "Aim 3 (mixed model) Variables.csv", row.names = FALSE, na = "")

