
# This script:
# 1. Plots an ellipse around the participant's data
# 2. Saves the figure in the original folder
# 3. Calculates the AIM score for individual throws based on Rapsodo data
#   - offsets height and side values from the target centre

# Distance is 16.73m from back edge of the force plates
# Target height is 0.71m (accounting for the 0.34m height of the platform)


# Let the user select the file to import
file_path <- file.choose()

library(tools)
library(dplyr)

# Extract the base file name without extension
base_filename <- tools::file_path_sans_ext(basename(file_path))


# # Load Participant's Raw Rapsodo .xlsx File
# library(readxl)
# data <- read_excel(file_path, col_names = TRUE, skip = 3) %>%
#   select(No, Velocity, `Strike Zone Side`, `Strike Zone Height`, `Release Extension (ft)`)

# Load Rapsodo .csv File
library(readr)
data <- read_csv(file_path, col_names = TRUE, skip = 3)

# Select variables for AIM
library(tidyverse)
data <- data %>%
   select(No, Velocity, `Strike Zone Side`, `Strike Zone Height`)

# Extract the max velocity throw
max_velocity <- data %>%
  filter(No == "21") %>%
  select(No, Velocity)

max_velocity$Velocity <- as.numeric(as.character(max_velocity$Velocity))

MTS <- max_velocity$Velocity * 0.44704

# Separate normal trials from MTS trial
data <- data %>%
  head(20)

data$Velocity <- as.numeric(data$Velocity)        
data$`Strike Zone Side` <- as.numeric(data$`Strike Zone Side`)    
data$`Strike Zone Height` <- as.numeric(data$`Strike Zone Height`)  
# data$`Release Extension (ft)` <- as.numeric(data$`Release Extension (ft)`)                   

complete_data <- data[complete.cases(data), ]


# Define throw length
throw_length <- 16.73

complete_data$Velocity <- complete_data$Velocity * 0.44704 # 0.44704 (mph to m/s) --- 0.27777778 (kph to m/s)
complete_data$`Strike Zone Side` <- complete_data$`Strike Zone Side` * 0.0254 # * 0.0254 # inches to m
complete_data$`Strike Zone Height` <- complete_data$`Strike Zone Height` * 0.0254 # * 0.0254 # inches to m
complete_data$`Length (m)` <- throw_length # - (complete_data$`Release Extension (ft)` * 0.3048) # ft to m
complete_data$`% MTS` <- complete_data$Velocity / MTS


# Specify target location
Aim_Side <- 0 # target side in metres
Aim_Height <- 0.71 # target height in metres


# Add column with player code
complete_data$Player <-  base_filename


# Clean Console
rm(data)  


# Offset throw location based on target location
complete_data$`Offset Location Side (m)` <- complete_data$`Strike Zone Side` - Aim_Side
complete_data$`Offset Location Height (m)` <- complete_data$`Strike Zone Height` - Aim_Height


# Filter out any cases where Location Height (m) is below the level of the ground (< 0m)
complete_data <- complete_data %>%
  filter(`Strike Zone Height` >= 0)


# PLOT EACH THROW WITH A CONFIDENCE ELLIPSE

ellipse_data <- complete_data %>%
  select(`Strike Zone Side`, `Strike Zone Height`)

# Function to calculate center point of confidence ellipse
calculateEllipseCentre <- function(ellipse_data) {
  # Calculate the center point using column means
  centre_point <- colMeans(ellipse_data)
  
  # Return the center point with the new column names
  return(data.frame(`Strike Zone Side` = centre_point["Strike Zone Side"],
                    `Strike Zone Height` = centre_point["Strike Zone Height"]))
}

# Calculate the centre of the participant's confidence ellipse
centre_point <- calculateEllipseCentre(ellipse_data)

colnames(centre_point) <- c("Horizontal Location (m)", "Vertical Location (m)")

target <- data.frame(Aim_Side, Aim_Height)

library(ggplot2)

# Plot the data with confidence ellipse and center
plot <- ggplot(ellipse_data, aes(x = `Strike Zone Side`, y = `Strike Zone Height`)) +
  stat_ellipse(level = 0.95, alpha = 0.3) +
  geom_point() +
  geom_point(data = centre_point, aes(x = `Horizontal Location (m)`, y = `Vertical Location (m)`, 
             color = "Centre of Dispersion"), size = 2, shape = 16) +
  geom_point(data = target, aes(x = Aim_Side, y = Aim_Height, color = "Target"), 
             size = 1, shape = 4, stroke = 2) +
  labs(title = paste("Dispersion for", base_filename),
       x = "Location Side (m)",
       y = "Location Height (m)",
       color = NULL) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(0, 2)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.position = c(0.2, 0.9),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.key = element_blank()) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 4))))


folder_path <- tools::file_path_sans_ext(dirname(file_path))
ggsave(paste0(folder_path, "/", base_filename, ".png"), plot = plot, width = 8.5, height = 8, dpi = 600, device = "png")


# Triangulate displacement from target (m)
complete_data$`Displacement (m)` <- sqrt(complete_data$`Offset Location Height (m)`^2 + complete_data$`Offset Location Side (m)`^2)


# Calculate AIM score - velocity^6 * length / 10^(displacement^(4/3))
complete_data$AIM <- (((complete_data$Velocity^6) * complete_data$`Length (m)`) / (10 ^ (complete_data$`Displacement (m)`^(4/3)))) / 10^8


# Create a final data frame with Velocity, Distance, Displacement, and AIM Score
final_data <- complete_data %>%
  select(Player, No, Velocity, `% MTS`, `Length (m)`, `Displacement (m)`, `AIM`)


# Round columns to desired decimal places
final_data$Velocity <- sprintf("%.1f", final_data$Velocity)
final_data$`Length (m)` <- sprintf("%.1f", final_data$`Length (m)`)
final_data$`Displacement (m)` <- sprintf("%.2f", final_data$`Displacement (m)`)
final_data$`AIM` <- sprintf("%.1f", final_data$`AIM`)


# Specify the folder to save the data to
output_folder <- "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/PhD Thesis/Data/Main Study/KOT - Rapsodo Files"

# Create the folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

# Write file to .csv
write.csv(final_data, file.path(output_folder, paste0(base_filename, " - Complete.csv")), row.names = FALSE)

# Clean Console
rm(list = ls())
             