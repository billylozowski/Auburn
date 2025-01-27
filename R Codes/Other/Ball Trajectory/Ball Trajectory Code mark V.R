
library(readxl)
library(dplyr)

# Read the Excel file
data <- read_excel("Auburn/SMML/Manuscripts/Kinematic Sequencing + Timing (performance)/Baseball/Data/Trackman Data (AU Spring 2023)/3-23-23 Georgia @ AU.xlsx")

# Filter rows with "fastball" in the "TaggedPitchType" column
fastball_data <- data %>%
  filter(TaggedPitchType == "Fastball")



# CALCULATING BALL TRAJECTORY

calculate_trajectory <- function(velocity, release_angle, horizontal_angle, release_height, release_distance, num_steps = 100) {
  g <- 9.81  # Acceleration due to gravity in m/s^2
  
  trajectories <- data.frame(Time = numeric(0), X = numeric(0), Y = numeric(0), Z = numeric(0), Pitch_ID = numeric(0))
  
  for (i in seq_along(velocity)) {
    u <- velocity[i]
    theta <- release_angle[i]
    phi <- horizontal_angle[i]
    h <- release_height[i]
    d <- release_distance[i]
    
    t <- seq(0, (2 * u * sin(theta * pi / 180)) / g, length.out = num_steps)
    x <- d * cos(phi * pi / 180) + h * tan(theta * pi / 180)
    y <- u * sin(theta * pi / 180) * t - 0.5 * g * t^2
    z <- d * sin(phi * pi / 180) + h - g * t^2 / 2
    
    # Find the index when the distance is reached
    index <- which(x >= d)[1]
    
    if (is.na(index)) {
      # If target distance is not reached, use the last time step
      index <- num_steps
    }
    
    trajectory <- data.frame(Time = t[index],
                             X = x[index],
                             Y = y[index],
                             Z = z[index],
                             Pitch_ID = i)
    
    trajectories <- rbind(trajectories, trajectory)
  }
  
  return(trajectories)
}

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

num_steps <- 100

#######################################################################################
# ABOVE THIS DIVIDE, THE CODE DOESN'T WORKS. THE DATA IS IMPORTED, FILTERED, AND CONVERTED
# INTO THE APPROPRIATE UNITS. IT DOES NOT CREATE THE 'calculate_trajectories' FUNCTION.
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

