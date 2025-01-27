# This code splits

# Let the user select the file to import
file_path <- file.choose()

# Read the data
library(readr)
data <- read.csv(file_path, check.names = FALSE)

# Remove the "File Name" column
library(dplyr)
data <- data %>%
  select(-FileName)

# Group by 'Event' and calculate mean for each numeric column
Means <- data %>%
  group_by(Event) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE, .names = "{.col}"))

# Group by 'Event' and calculate standard deviation for each numeric column
SD <- data %>%
  group_by(Event) %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE, .names = "{.col}SD"))

# Combine Means and SD data frames
summary_data <- cbind(Means, SD) 

# Remove 2nd "Event" column
summary_data <- summary_data %>%
  select(-36) 

# Define order of "Event"
custom_order <- c("BFC", "FFC", "AA", "BR", "FT", "Min", "Max")

# Convert the "Event" column to a factor with custom levels
summary_data$Event <- factor(summary_data$Event, levels = custom_order)

# Order the data frame by the "Event" column
summary_data <- summary_data[order(summary_data$Event), ]

# Save 'summary_data' as a CSV file
write.csv(summary_data, "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/PhD Thesis/Data/Pilot/KOT - Combined Data/Means and SDs.csv", row.names = FALSE)

rm(data, Means, SD, custom_order, file_path)

