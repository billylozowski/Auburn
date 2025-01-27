# This code splits

# Let the user select the file to import
file_path <- file.choose()

# Read the data
library(readr)
data <- read.csv(file_path, check.names = FALSE)

# # Remove the "File Name" column
# library(dplyr)
# data <- data %>%
#   select(-File Name)

# Group by 'Event' and calculate mean for each numeric column
Means <- data %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

rownames(Means) <- "mean"

# Group by 'Event' and calculate standard deviation for each numeric column
SD <- data %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE))

rownames(SD) <- "sd"

# Combine Means and SD data frames
summary_data <- rbind(Means, SD) 


# # Define order of "Event" - don't need this for the Aim 3!
# custom_order <- c("BFC", "FFC", "MAW", "BR", "EFT", "Min", "Max")
# 
# # Convert the "Event" column to a factor with custom levels
# summary_data$Event <- factor(summary_data$Event, levels = custom_order)
# 
# # Order the data frame by the "Event" column
# summary_data <- summary_data[order(summary_data$Event), ]

# Specify the folder to save the data to
output_folder <- "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/PhD Thesis/Data/Pilot Study/KOT - Combined Data/Combined Data (pilot)"

# Create the folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

write.csv(summary_data, file = file.path(output_folder, "Means & SDs.csv"), row.names = TRUE)

rm(list = ls())# Save 'summary_data' as a CSV file
