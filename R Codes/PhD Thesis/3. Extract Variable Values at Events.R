# This code will extract variables of interest at specific events, as well as
# calculating temporal variables

# Load "Event Data (combined)" - all participants' trials and events

rm(list = ls())

# Load the desired file - "Event Data (combined).csv"
file_path <- file.choose()

data <- read.csv(file_path, check.names = FALSE) 

# Create event data frames

library(dplyr)

# BFC data
BFC_data <- data %>%
  filter(Event =="BFC") %>%
  select(`File Name`, `Time (s)`, `Back Knee Flexion`, `Back Knee Extension Velocity`, `COM (X)`, `COM (Z)`,
         `COM Velocity (X)`, `COM Velocity (Z)`, `Pelvis Rotation (Y)`, `Trunk Rotation (Y)`, `Trunk Flexion`, 
         `Shoulder Abduction`, `Shoulder H Abduction`, `Elbow Flexion`) %>%
  rename_with(~paste0(.x, " @ BFC"), .names = "{.col}")

# FFC data
FFC_data <- data %>% 
  filter(Event == "FFC") %>% 
  select(`File Name`, `Time (s)`, `Stride Length (% Height)`, `Stride Angle`,`Back Knee Extension Velocity`, `Front Knee Flexion`, 
         `Pelvis Rotation (Y)`, `Trunk Rotation (Y)`, `Trunk Flexion`, `Shoulder Abduction`, `Shoulder H Abduction`, 
         `Elbow Flexion`, `COM (X)`, `COM (Z)`, `COM Velocity (X)`, `COM Velocity (Z)`) %>%
         # "Shoulder Abduction SD", "Shoulder H Abduction SD", "Trunk Flexion SD") 
  rename_with(~paste0(.x, " @ FFC"), .names = "{.col}")

# MAW data
MAW_data <- data %>% 
  filter(Event == "MAW") %>% 
  select(`File Name`, `Time (s)`, `Front Knee Flexion`, `Pelvis Rotation (Y)`, `Trunk Rotation (Y)`,
         `Shoulder Abduction`, `Shoulder H Abduction`, `Elbow Flexion`) %>%
         # "Shoulder H Abduction SD", "Shoulder Rotation SD") 
  rename_with(~paste0(.x, " @ MAW"), .names = "{.col}")

# BR data
BR_data <- data %>% 
  filter(Event == "BR") %>% 
  select(`File Name`, `Time (s)`, Height, `Front Knee Flexion`, `Front Knee Extension Velocity`, `Pelvis Rotation (Y)`, 
         `Trunk Flexion`, `Trunk Rotation (Y)`, `Lateral Trunk Flexion`, `Shoulder Abduction`, `Shoulder H Abduction`, 
         `Elbow Flexion`, `COM (X)`, `COM (Z)`, `COM Velocity (X)`, `COM Velocity (Z)`, `Hand COM (X)`, `Hand COM (Z)`) %>%
  rename_with(~paste0(.x, " @ BR"), .names = "{.col}")

# # EFT data
# EFT_data <- data %>%
#   filter(Event == "EFT") %>%
#   select(`File Name`, `Time (s)`, "Knee Flexion", "Shoulder Abduction", "Shoulder H Abduction",
#          "Shoulder Rotation", "Elbow Flexion") %>%
#   rename_with(~paste0(.x, " @ EFT"), .names = "{.col}")

# Max data (need to calculate mins and maxes first)
Max_data <- data %>%
  filter(Event == "Max") %>%
  select(`Front Knee Extension Velocity`, `Pelvis Rotation Velocity`, `Trunk Rotation Velocity`,
        `Trunk Flexion Velocity`, `Trunk Flexion`, `Shoulder Rotation Velocity`, `Elbow Flexion`) %>%
  rename_with(~paste0(.x, " (max)"), .names = "{.col}")

# Min data
Min_data <- data %>%
  filter(Event == "Min") %>%
  select(`Shoulder Rotation`, `Elbow Extension Velocity`) %>%
  rename_with(~paste0(.x, " (max)"), .names = "{.col}")

# # Temporal data
# temporal_data <- data.frame() # create an empty data frame called 'temporal_data'
# 
# # Phase Durations (also have this in the Event Marking Script)
# temporal_data$`Stride Duration (s)` <- FFC_data$Time - BFC_data$Time # Stride
# temporal_data$`Transition Duration (s)` <- MAW_data$Time - FFC_data$Time # Transition
# temporal_data$`Acceleration Duration (s)` <- BR_data$Time - MAW_data$Time # Acceleration
# temporal_data$`Follow-Through Duration (s)` <- EFT_data$Time - BR_data$Time # Follow-Through
# temporal_data$`Contact Time (s)` <- BR_data$Time # Contact Time
# temporal_data$`Total Duration (s)` <- EFT_data$Time - BFC_data$Time # Total Throw Duration

# Combine Event data into one data frame (Variables of Interest [VOI])
VOI_data <- cbind(BFC_data, FFC_data, MAW_data, BR_data, Max_data, Min_data) %>% # EFT_data, temporal_data) %>%
  select(-`File Name @ FFC`, -`File Name @ MAW`, -`File Name @ BR`) #, -`File Name (max)`)

# Add COM displacements
VOI_data$`COM Displacement (X) - stride` <- FFC_data$`COM (X)` - BFC_data$`COM (X)` # COM Displacement (X) over the stride phase
VOI_data$`COM Displacement (X) - action` <- BR_data$`COM (X)` - FFC_data$`COM (X)` # COM Displacement (X) over the action phase
VOI_data$`COM Displacement (X) - total` <- BR_data$`COM (X)` - BFC_data$`COM (X)` # COM Displacement (X) over the whole throw (BFC-BR)
VOI_data$`COM Displacement (Z) - stride` <- FFC_data$`COM (Z)` - BFC_data$`COM (Z)` # COM Displacement (Z) over the stride phase
VOI_data$`COM Displacement (Z) - action` <- BR_data$`COM (Z)` - FFC_data$`COM (Z)` # COM Displacement (Z) over the action phase
VOI_data$`COM Displacement (Z) - total` <- BR_data$`COM (Z)` - BFC_data$`COM (Z)` # COM Displacement (Z) over the whole throw (BFC-BR)

# Add Δ COM Velocities
VOI_data$`Δ COM Velocity (X) - stride` <- FFC_data$`COM Velocity (X)` - BFC_data$`COM Velocity (X)` # Δ COM Velocity (X) over the stride phase
VOI_data$`Δ COM Velocity (X) - action` <- BR_data$`COM Velocity (X)` - FFC_data$`COM Velocity (X)` # Δ COM Velocity (X) over the action phase
VOI_data$`Δ COM Velocity (X) - total` <- BR_data$`COM Velocity (X)` - BFC_data$`COM Velocity (X)` # Δ COM Velocity (X) over the whole throw (BFC-BR)
VOI_data$`Δ COM Velocity (Z) - stride` <- FFC_data$`COM Velocity (Z)` - BFC_data$`COM Velocity (Z)` # Δ COM Velocity (Z) over the stride phase
VOI_data$`Δ COM Velocity (Z) - action` <- BR_data$`COM Velocity (Z)` - FFC_data$`COM Velocity (Z)` # Δ COM Velocity (Z) over the action phase
VOI_data$`Δ COM Velocity (Z) - total` <- BR_data$`COM Velocity (Z)` - BFC_data$`COM Velocity (Z)` # Δ COM Velocity (Z) over the whole throw (BFC-BR)

# Add release location variables
VOI_data$`Stretch (%height)` <- (BR_data$`Hand COM (X)` - BR_data$`COM (X)`) / BR_data$Height # hand position (X) relative to COM (X) @ BR
VOI_data$`Span (%height)` <- (BR_data$`Hand COM (Z)` - BR_data$`COM (Z)`) / BR_data$Height # hand position (Z) relative to COM (Z) @ BR

# Remove " @ BFC" from the "File Name @ FFC" column
#VOI_data$`File Name @ BFC` <- sub(" @ BFC$", "", VOI_data$`File Name @ BFC`)

colnames(VOI_data)[colnames(VOI_data) == "File Name @ BFC"] <- "File Name"

# Save 'VOI_data' as a CSV file
write.csv(VOI_data, "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/PhD Thesis/3. Data/Main Study/KOT - Processed Data/Variables of Interest (all participants).csv", 
          row.names = FALSE, fileEncoding = "UTF-8", na = "")

rm(data, BFC_data, FFC_data, MAW_data, BR_data, Min_data, Max_data, file_path)
