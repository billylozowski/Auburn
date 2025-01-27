
# This script calculates the AIM score for individual throws based on Rapsodo
# strike zone locations. Distance is 60ft from back edge of the FPs (18.288m)


# Let the user select the file to import
file_path <- file.choose()

library(tools)

# Extract the base filename without extension
base_filename <- tools::file_path_sans_ext(basename(file_path))


# Load Rapsodo .csv File - need to actually select the excel file
library(readr)
data <- read_csv(file_path, col_names = TRUE, skip = 3) 


# Select variables for AIM
library(tidyverse)
data <- data %>%
  select(No, Velocity, `Strike Zone Side`, `Strike Zone Height`) #, `Release Extension (ft)`)

data$Velocity <- as.numeric(data$Velocity)        
data$`Strike Zone Side` <- as.numeric(data$`Strike Zone Side`)    
data$`Strike Zone Height` <- as.numeric(data$`Strike Zone Height`)  
# data$`Release Extension (ft)` <- as.numeric(data$`Release Extension (ft)`)                   

complete_data <- data[complete.cases(data), ]


# Convert into SI units
throw_length <- 18.288

complete_data$Velocity <- complete_data$Velocity * 0.27777778 # 0.44704 (mph to m/s) --- 0.27777778 (kph to m/s)
complete_data$`Strike Zone Side` <- complete_data$`Strike Zone Side` / 100 # * 0.0254 # inches to m
complete_data$`Strike Zone Height` <- complete_data$`Strike Zone Height` / 100 # * 0.0254 # inches to m
complete_data$`Length (m)` <- throw_length #- (complete_data$`Release Extension (ft)` * 0.3048) # ft to m


# Specify target location
Aim_Side <- 0 # specify target side in metres
Aim_Height <- 0.71 # specify target height in metres (accounting for 0.34m platform height)


# Add column with player code
complete_data$Player <-  base_filename


# Clean Console
rm(data)  


# Offset throw location based on target location
complete_data$`Location Side (m)` <- complete_data$`Strike Zone Side` - Aim_Side
complete_data$`Location Height (m)` <- complete_data$`Strike Zone Height` - Aim_Height


# May want a line of code here to remove/filter out any cases where Height (adj.) is below the level of the ground (< -0.71m)


# Triangulate displacement from target (m)
complete_data$`Displacement (m)` <- sqrt(complete_data$`Location Height (m)`^2 + complete_data$`Location Side (m)`^2)


# Calculate AIM score - velocity*length / 100^displacement
complete_data$AIM <- ((complete_data$Velocity * complete_data$`Length (m)`) / (100 ^ complete_data$`Displacement (m)`))


# Create a final data frame with Velocity, Distance, Displacement, and AIM Score
final_data <- complete_data %>%
  select(Player, No, Velocity, `Length (m)`, `Displacement (m)`, `AIM`)


# Round columns to desired decimal places
final_data$Velocity <- sprintf("%.2f", final_data$Velocity)
final_data$`Length (m)` <- sprintf("%.2f", final_data$`Length (m)`)
final_data$`Displacement (m)` <- sprintf("%.2f", final_data$`Displacement (m)`)
final_data$`AIM` <- sprintf("%.2f", final_data$`AIM`)


# Write file to .csv
save_directory <- tools::file_path_as_absolute(dirname(file_path))
write.csv(final_data, file.path(save_directory, paste0(base_filename, " - Complete.csv")), row.names = FALSE)


# Clean Console
rm(base_filename, file_path, save_directory, throw_length, Aim_Height, Aim_Side, complete_data)
