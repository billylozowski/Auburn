
# This script:
# Imports data
# Converts all data into the desired units
# Calculates ball trajectory, before plotting it

# Import data from one Excel file
# This code only imports that data which is needed
library(readxl)
library(dplyr)

# Read the Excel file
data <- read_excel("Auburn/SMML/Manuscripts/Kinematic Sequencing + Timing (performance)/Baseball/Data/Trackman Data (AU Spring 2023)/3-23-23 Georgia @ AU.xlsx")

# Filter rows with "fastball" in the "TaggedPitchType" column
filtered_data <- data %>%
  filter(TaggedPitchType == "Fastball")

# CALCULATING BALL TRAJECTORY

# Conversion factors
feet_to_meters <- 0.3048

# Convert variables to appropriate units
Velocity_mps <- filtered_data$RelSpeed * feet_to_meters
Release_Angle_deg <- filtered_data$VertRelAngle
Horizontal_Release_Angle_deg <- filtered_data$HorzRelAngle
Release_Distance_m <- (60.5 - filtered_data$Extension) * feet_to_meters
Release_Height_m <- filtered_data$RelHeight
Release_Side_m <- filtered_data$RelSide

converted_data <- data.frame(Velocity_mps, Release_Angle_deg, 
                             Horizontal_Release_Angle_deg, Release_Height_m, 
                             Release_Side_m, Release_Distance_m)

# Gravity constant (m/s^2)
gravity <- 9.81

# Calculate initial velocity components
initial_velocity_x <- Velocity_mps * cos(Release_Angle_deg) * cos(Horizontal_Release_Angle_deg)
initial_velocity_y <- Velocity_mps * sin(Release_Angle_deg)
initial_velocity_z <- Velocity_mps * cos(Release_Angle_deg) * sin(Horizontal_Release_Angle_deg)

# Calculate time of flight
time_of_flight <- (initial_velocity_y + sqrt(initial_velocity_y^2 + 2 * gravity * Release_Height_m)) / gravity

# Number of time intervals
num_intervals <- 100

# Calculate time step
time_step <- time_of_flight / num_intervals

# Initialize vectors for position coordinates
horizontal_distance <- numeric(num_intervals)
vertical_distance <- numeric(num_intervals)
lateral_distance <- numeric(num_intervals)

# Calculate position coordinates at each time interval
for (i in 1:num_intervals) {
  t <- i * time_step
  
  horizontal_distance[i] <- initial_velocity_x * t
  vertical_distance[i] <- initial_velocity_y * t - 0.5 * gravity * t^2 + Release_Height_m
  lateral_distance[i] <- Release_Side_m * t
}

# Convert vectors to matrices
lateral_distance_matrix <- matrix(lateral_distance, ncol = 1)  # Assuming lateral_distance is a vector
vertical_distance_matrix <- matrix(vertical_distance, ncol = 1)  # Assuming vertical_distance is a vector

# Determine the number of throws based on the dimensions of the matrices
number_of_throws <- ncol(lateral_distance_matrix)

# Set xlim range
xlim_range <- max(abs(range(lateral_distance_matrix, na.rm = TRUE)))

# Create a new plot for each throw
for (i in 1:number_of_throws) {
  # Create a new plot window
  plot(0, type = "n", xlab = "Lateral Distance (m)", ylab = "Vertical Distance (m)", main = paste0("Ball Trajectory - Throw ", i), xlim = c(-xlim_range, xlim_range), ylim = c(0, max(vertical_distance_matrix, na.rm = TRUE)))
  
  # Plot the trajectory for the current throw
  lines(lateral_distance_matrix[, i], vertical_distance_matrix[, i])
}
