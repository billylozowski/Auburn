
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
  
  trajectories <- lapply(1:length(velocity), function(i) {
    v <- velocity[i]
    ra <- release_angle[i]
    ha <- horizontal_angle[i]
    rh <- release_height[i]
    rd <- release_distance[i]
    
    time_of_flight <- (2 * v * sin(ra * pi / 180)) / g
    time_intervals <- seq(0, time_of_flight, length.out = num_steps)
    
    x <- rd * cos(ha * pi / 180)
    y <- rh
    z <- rd * sin(ha * pi / 180)
    
    x_displacement <- x * cos(ra * pi / 180) * time_intervals
    y_displacement <- y + v * sin(ra * pi / 180) * time_intervals - 0.5 * g * time_intervals^2
    z_displacement <- z * cos(ra * pi / 180) * time_intervals
    
    trajectory <- data.frame(Time = time_intervals,
                             X = x_displacement,
                             Y = y_displacement,
                             Z = z_displacement)
    return(trajectory)
  })
  
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

