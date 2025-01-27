
# Load the file to be time-normalised
file_path <- file.choose()

library(readr)
data <- read.csv(file_path, check.names = FALSE)
file_name <- tools::file_path_sans_ext(basename(file_path))


library(dplyr)
data <- data %>%
  select(`Back Knee Flexion`, `Front Knee Flexion`, `Back Knee Extension Velocity`,
         `Front Knee Extension Velocity`, `Pelvis Rotation (Y)`, `Pelvis Rotation Velocity`, 
         `Pelvis-Trunk Separation`, `Trunk Flexion`, `Trunk Flexion Velocity`, `Lateral Trunk Flexion`,
         `Trunk Rotation (Y)`, `Trunk Rotation Velocity`, `Shoulder Abduction`, `Shoulder H Abduction`,
         `Shoulder H Abduction Velocity`, `Shoulder Rotation`, `Shoulder Rotation Velocity`, `Elbow Flexion`,
         `Elbow Extension Velocity`, `Hand COM (X)`, `Hand COM (Y)`, `Hand COM (Z)`, `Hand COM Velocity (X)`,
         `COM (X)`, `COM (Y)`, `COM (Z)`,`COM Velocity (X)`, `COM Velocity (Z)`)


# Define the function for time normalising
time_normalise <- function(data) {
  # Get the number of rows in the original data
  n <- nrow(data)
  
  # Original time vector based on the number of rows
  original_time <- seq(0, n - 1)
  
  # Define the target time vector for 101 points
  target_time <- seq(0, n - 1, length.out = 101)  # 101 evenly spaced points
  
  # Initialize a data frame to store the interpolated data
  interpolated_data <- data.frame(matrix(nrow = 101, ncol = ncol(data)))
  colnames(interpolated_data) <- colnames(data)  # Keep original column names
  
  # Perform interpolation for each column of the data
  for (col in 1:ncol(data)) {
    current_col_data <- data[[col]]  # Access the column directly
    
    # Perform interpolation, ensuring we handle NAs if any exist
    interpolated_values <- approx(original_time, current_col_data, xout = target_time, na.rm = TRUE)$y
    
    # Fill the interpolated data
    interpolated_data[, col] <- interpolated_values
  }
  
  # Add the Duration column (0 to 100)
  interpolated_data <- cbind(interpolated_data, Duration = seq(0, 100, length.out = 101))
  
  return(interpolated_data)
}


# Perform time normalisation
normalised_data <- time_normalise(data)

normalised_data$`File Name` <- file_name
normalised_data <- normalised_data %>%
  select(`File Name`, Duration, everything())


# Save 'normalised_data' as a CSV file in the folder "Time Normalised Data"

# Specify the folder to save the data to
output_folder <- "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/PhD Thesis/2. Chapters/Chapter 4 - Aim 1/Data/Files for Waveform Plots/Time Normalised Data"

# Create the folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

write.csv(normalised_data, file = file.path(output_folder, paste0(file_name, " .csv")), row.names = FALSE, na = "")

