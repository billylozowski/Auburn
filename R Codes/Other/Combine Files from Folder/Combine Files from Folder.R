# This code combines multiple .csv files from a folder into one.

# Let the user choose the folder
folder_path <- choose.dir()

# Get a list of all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each CSV file, read it, and combine it into the 'combined_data' data frame
for (csv_file in csv_files) {
  # Read the CSV file
  current_data <- read.csv(csv_file, header = TRUE,check.names = FALSE)
  
  # Combine the data frames
  combined_data <- rbind(combined_data, current_data)
}


# Save the combined data as a new CSV file
write.csv(combined_data, file = "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/PhD Thesis/Data/Pilot/KOT - Combined Data/Pilot.csv", row.names = FALSE)

