# This code combines multiple .csv files from a folder into one.

rm(list = ls())

# Let the user choose the folder - Load from the Event Data folder
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

# Specify the folder to save the data to
output_folder <- "C:/Users/billy/OneDrive/Documents/Academics/2. Auburn/Sports Medicine & Movement Lab/Misc/Caroline Keller/Fellowship/Data"
# Create the folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

# Make sure to specify the correct file name!!!
write.csv(combined_data, file = file.path(output_folder, "CK Event Indices (combined).csv"), row.names = FALSE, na = "")
