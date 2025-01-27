
# This script calculates the AIM score for individual throws based on Rapsodo data

# Distance from back edge of the force plates is 16.73m (minus COM (X) location)
# Target height is 0.71m (accounting for the 0.34m height of the platform)

rm(list = ls())

# Specify target location
Target_Side <- 0 # target side in metres
Target_Height <- 0.71 # target height in metres


# Let the user select the file to import - "Master File.csv"
file_path <- file.choose()

library(readr)
data <- read_csv(file_path, col_names = TRUE)


library(dplyr)
data <- data[complete.cases(data), ]

# # Extract the base file name without extension
# base_filename <- tools::file_path_sans_ext(basename(file_path))

# # Load the combined file with kinematics and Rapsodo data - "Master File"
# library(readxl)
# data <- read_excel(file_path, col_names = TRUE)


# data$Velocity <- as.numeric(data$Velocity)        
# data$`Strike Zone Side` <- as.numeric(data$`Strike Zone Side`)    
# data$`Strike Zone Height` <- as.numeric(data$`Strike Zone Height`)  
# # data$`Release Extension (ft)` <- as.numeric(data$`Release Extension (ft)`)                   


# Calculate throw length - may only use 16.73 so I don't have to export all trials immediately!
throw_length <- 16.73 - data$`COM (X) @ BR` # uses COM location relative to target instead of BR location

# data$Velocity <- data$Velocity * 0.27777778 # 0.44704 (mph to m/s) --- 0.27777778 (kph to m/s)
# data$`Strike Zone Side` <- data$`Strike Zone Side` * 0.0254 # * 0.0254 # inches to m
# data$`Strike Zone Height` <- data$`Strike Zone Height` * 0.0254 # * 0.0254 # inches to m
data$`Length (m)` <- throw_length #- (complete_data$`Release Extension (ft)` * 0.3048) # ft to m
# complete_data$`% MTS` <- complete_data$Velocity / MTS


# # Add column with player code
# complete_data$Player <-  base_filename


# Offset throw location based on target location
data$`Offset Location Side (m)` <- (data$`Strike Zone Side` - Target_Side)/100
data$`Offset Location Height (m)` <- (data$`Strike Zone Height` - Target_Height)/100


# Filter out any cases where Location Height (m) is below the level of the ground (< 0m)
data <- data %>%
  filter(`Offset Location Height (m)` >= 0)


# Triangulate displacement from target (m)
data$`Displacement (m)` <- sqrt(data$`Offset Location Height (m)`^2 + data$`Offset Location Side (m)`^2)


# Calculate AIM score - velocity*length / 10^(displacement^(4/3))
data$AIM <- ((data$Velocity * data$`Length (m)`) / (10 ^ (data$`Displacement (m)`^(4/3))))


# Remove variables created for AIM that are no longer needed
data <- data %>%
  select(-`Offset Location Height (m)`, -`Offset Location Side (m)`, -`Strike Zone Height`, -`Strike Zone Side`,) %>%
  select(`File Name (kinematics)`, No, Velocity, `Length (m)`, `Displacement (m)`, AIM, everything())


# Round columns to desired decimal places
data$Velocity <- sprintf("%.1f", data$Velocity)
data$`Length (m)` <- sprintf("%.1f", data$`Length (m)`)
data$`Displacement (m)` <- sprintf("%.2f", data$`Displacement (m)`)
data$`AIM` <- sprintf("%.1f", data$`AIM`)


# Specify the folder to save the data to
output_folder <- "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/PhD Thesis/Data/Pilot Study/KOT - Processed Data/Complete Data"

# Create the folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

# Write file to .csv
write.csv(data, file.path(output_folder, "Complete Data.csv"), row.names = FALSE)

# Clean Console
rm(list = ls())
