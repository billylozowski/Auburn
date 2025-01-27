
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

# Conversion factors
feet_to_meters <- 0.3048

# Convert variables to appropriate units
velocity <- filtered_data$RelSpeed * 0.3048
release_angle <- filtered_data$VertRelAngle
horizontal_angle <- filtered_data$HorzRelAngle
release_distance <- (60.5 - filtered_data$Extension) * 0.3048
release_height <- filtered_data$RelHeight * 0.3048
Rel_Side_m <- filtered_data$RelSide * 0.3048
Timecode <- filtered_data$Time

# Combine vales into a data frame
converted_data <- data.frame(Timecode, velocity, release_angle, 
                             horizontal_angle, release_height, 
                             Rel_Side_m, release_distance)


# CALCULATE BALL TRAJECTORIES

library(ggplot2)

# Constants
gravity <- 9.81  # Acceleration due to gravity in m/s^2
time_interval <- 0.01  # Time interval in seconds for calculation

# Function to calculate the trajectory
calculate_trajectory <- function(velocity, release_angle, horizontal_angle, release_height, release_distance) {
  # Convert angles from degrees to radians
  release_angle_rad <- release_angle * pi / 180
  horizontal_angle_rad <- horizontal_angle * pi / 180
  
  # Calculate time of flight
  time_of_flight <- (2 * velocity * sin(release_angle_rad)) / gravity
  
  # Calculate the number of time steps
  num_steps <- ceiling(time_of_flight / time_interval)
  
  # Create a time vector with the specified time interval
  time <- seq(0, time_of_flight, length.out = num_steps)
  
  # Calculate the x, y, and z coordinates for each time step
  x <- release_distance + velocity * cos(release_angle_rad) * cos(horizontal_angle_rad) * time
  y <- release_height + velocity * sin(release_angle_rad) * time - 0.5 * gravity * time^2
  z <- velocity * cos(release_angle_rad) * sin(horizontal_angle_rad) * time
  
  # Return the trajectory as a data frame
  data.frame(x, y, z)
}

# Retrieve data from existing variables
velocity <- converted_data$velocity
release_angle <- converted_data$release_angle
horizontal_angle <- converted_data$horizontal_angle
release_distance <- converted_data$release_distance
release_height <- converted_data$release_height

# Calculate trajectory for each pitch
trajectories <- calculate_trajectory(velocity, release_angle, horizontal_angle, release_height, release_distance)

# Plot the displacement in the Y and Z axes
ggplot(trajectories, aes(x = y, y = z)) +
  geom_line() +
  xlab("Displacement in Y Axis (m)") +
  ylab("Displacement in Z Axis (m)") +
  xlim(-20, 20)
