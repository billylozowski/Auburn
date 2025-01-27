# This code splits

# Let the user select the file to import
file_path <- file.choose()

# Read the data
library(readxl)
data <- read_excel(file_path)

# Remove the "File Name" column
library(dplyr)
data <- data %>%
  select(-`File Name`)

# Group by 'Event' and calculate mean for each numeric column
Means <- data %>%
  group_by(Event) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE, .names = "{.col} - Mean"))

# Group by 'Event' and calculate standard deviation for each numeric column
SD <- data %>%
  group_by(Event) %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE, .names = "{.col} - SD"))

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

rm(data, Means, SD, custom_order)

