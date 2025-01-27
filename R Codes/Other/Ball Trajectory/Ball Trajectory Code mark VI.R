
library(readxl)
library(dplyr)

# Read the Excel file
data <- read_excel("Auburn/SMML/Manuscripts/Kinematic Sequencing + Timing (performance)/Baseball/Data/Trackman Data (AU Spring 2023)/3-23-23 Georgia @ AU.xlsx")

# Filter rows with "fastball" in the "TaggedPitchType" column
fastball_data <- data %>%
  filter(TaggedPitchType == "Fastball")


# Extract variables from existing object
velocity <- fastball_data$RelSpeed
release_angle <- fastball_data$VertRelAngle
horizontal_angle <- fastball_data$HorzRelAngle
release_height <- fastball_data$RelHeight
release_distance <- fastball_data$Extension
release_side <- fastball_data$RelSide
timecode <- fastball_data$Time

# Create new object release_parameters (converted to appropriate units)
release_parameters <- data.frame(
  Timecode = timecode,
  Velocity = velocity * 0.44704,
  ReleaseAngle = release_angle,
  HorizontalAngle = horizontal_angle,
  ReleaseHeight = release_height * 0.3048,
  ReleaseSide = release_side * 0.3048,
  ReleaseDistance = (60.5 - release_distance) * 0.3048
)


# CALCULATING BALL TRAJECTORY

calculate_trajectory <- function(initial_velocity, vertical_angle, horizontal_angle, height, g, num_points) {
  # Convert angles from degrees to radians
  vertical_angle_rad <- vertical_angle * pi / 180
  horizontal_angle_rad <- horizontal_angle * pi / 180
  
  # Calculate vertical and horizontal components of initial velocity
  v_y <- initial_velocity * sin(vertical_angle_rad)
  v_x <- initial_velocity * cos(vertical_angle_rad) * cos(horizontal_angle_rad)
  
  # Calculate time of flight
  t_flight <- 2 * v_y / g
  
  # Calculate time increment for each point
  dt <- t_flight / num_points
  
  # Preallocate vectors to store positions
  x_points <- numeric(num_points)
  y_points <- numeric(num_points)
  
  # Calculate positions for each time point
  for (i in 1:num_points) {
    t <- (i - 1) * dt
    
    # Calculate horizontal and vertical positions
    x_points[i] <- v_x * t
    y_points[i] <- height + v_y * t - 0.5 * g * t^2
  }
  
  # Return trajectory as a data frame
  trajectory <- data.frame(x = x_points, y = y_points)
  return(trajectory)
}

# Example data for multiple trials (pitches)
data <- data.frame(
  initial_velocity = c(40, 39, 39),  # Initial velocity in units/s for each trial
  vertical_angle = c(-4, -1, 2),  # Vertical projection angle in degrees for each trial
  horizontal_angle = c(-3, 0, 1),  # Horizontal projection angle in degrees for each trial
  height = c(1.80, 1.80, 1.79)  # Launch height in units for each trial
)

g <- 9.81  # Gravitational acceleration in units/s^2
num_points <- 100  # Number of points to calculate along the trajectory

# Create the first plot: Distance vs. Height (Perpendicular view)
plot_distance_height <- function(data) {
  plot(0, 0, xlim = c(-100, 20), ylim = c(0, 3), xlab = "Distance", ylab = "Height")
  
  # Iterate through each trial and calculate the trajectory
  for (i in 1:nrow(data)) {
    initial_velocity <- data$initial_velocity[i]
    vertical_angle <- data$vertical_angle[i]
    horizontal_angle <- data$horizontal_angle[i]
    height <- data$height[i]
    
    trajectory <- calculate_trajectory(initial_velocity, vertical_angle, horizontal_angle, height, g, num_points)
    
    # Plot the trajectory
    lines(trajectory$x, trajectory$y)
  }
  
  # Add a legend for each trajectory
  #legend("topright", legend = 1:nrow(data), title = "Throw", col = 1:nrow(data), lty = 1)
}

# Create the second plot: Lateral Displacement vs. Height (View from behind)
plot_lateral_displacement_height <- function(data) {
  plot(0, 0, xlim = c(-1, 1), ylim = c(0, 3), xlab = "Lateral Displacement", ylab = "Height")
  
  # Iterate through each trial and calculate the trajectory
  for (i in 1:nrow(data)) {
    initial_velocity <- data$initial_velocity[i]
    vertical_angle <- data$vertical_angle[i]
    horizontal_angle <- data$horizontal_angle[i]
    height <- data$height[i]
    
    trajectory <- calculate_trajectory(initial_velocity, vertical_angle, horizontal_angle, height, g, num_points)
    
    # Plot the trajectory
    lines(trajectory$x - mean(trajectory$x), trajectory$y)
  }
  
  # Add a legend for each trajectory
  #legend("topright", legend = 1:nrow(data), title = "Throw", col = 1:nrow(data), lty = 1)
}

# Call the plot functions to generate the plots
plot_distance_height(data)
plot_lateral_displacement_height(data)

    



#######################################################################################
# ABOVE THIS DIVIDE, THE CODE WORKS FINE. THE DATA IS IMPORTED, FILTERED, AND CONVERTED
# INTO THE APPROPRIATE UNITS. IT ALSO CREATES THE 'calculate_trajectories' FUNCTION.
#######################################################################################

# Calculate trajectory for each pitch
trajectories <- calculate_trajectory(release_parameters$Velocity, release_parameters$ReleaseAngle, release_parameters$HorizontalAngle, release_parameters$ReleaseHeight, release_parameters$ReleaseDistance, num_steps)


                            #############################
                            # THIS CODE NOW WORKS FINE. # 
                            #############################


# Combine the trajectories into a single data frame
combined_trajectory <- dplyr::bind_rows(trajectories, .id = "PitchID")

# Plot the trajectory
ggplot(combined_trajectory, aes(x = X, y = Y, group = PitchID)) +
  geom_line() +
  labs(x = "Displacement in X (m)", y = "Displacement in Y (m)") +
  #coord_fixed(ratio = 1) +
  xlim(-20, 20)


# Plot the trajectory with Y on the X-axis and Z on the Y-axis, grouped by PitchID
ggplot(combined_trajectory, aes(x = Time, y = Z, group = PitchID)) +
  geom_line() +
  labs(x = "Displacement in Y (m)", y = "Displacement in Z (m)") +
  coord_fixed(ratio = 1) +
  xlim(-1, 1)

