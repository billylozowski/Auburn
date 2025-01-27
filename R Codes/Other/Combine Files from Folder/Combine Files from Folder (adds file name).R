# This code reads all .csv files from a specified folder, and combines them into one data frame.
# In addition, it adds the file's name to the first column


library(dplyr)

# Let the user choose the folder
folder_path <- choose.dir()

# Get a list of all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
Pilot_data <- data.frame()

# Loop through each CSV file, read it, and combine it into the 'combined_data' data frame
for (csv_file in csv_files) {
  # Read the CSV file
  current_data <- read.csv(csv_file, header = TRUE, check.names = FALSE) %>%
    mutate(FileName = tools::file_path_sans_ext(basename(csv_file)))
  
  # Combine the data frames
  Pilot_data <- rbind(Pilot_data, current_data)
}

# Remove columns containing "- SD" and removes "- Mean" from the remaining columns
Pilot_data <- Pilot_data %>%
  select(-ends_with("- SD")) %>%
  rename_all(~ gsub("- Mean$", "", .)) %>%
  
# Re-orders the data frame so FileName is the first column
  select(FileName, everything())

# Print the combined data
print(Pilot_data)

# Save 'Pilot_data' as a CSV file
write.csv(Pilot_data, "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/PhD Thesis/Data/Pilot/KOT - Combined Data/Pilot Data.csv", row.names = FALSE)

rm(current_data, csv_file, csv_files, folder_path)
