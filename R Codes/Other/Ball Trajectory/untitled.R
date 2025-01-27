
# This script determines the location of the ball at the target based off the 
# initial release parameters using projectile motion equations.

# I want to have the option to select a file with a GUI (lines 8-14 currently)


# IMPORT DATA FROM FILE

# Let the user select the file to import
file_path <- file.choose()


library(readxl)
# Import the data from the selected file
data <- read.excel(file_path)  # Modify the function based on your file format and structure

###########################################################################################





# DEFINE RELEASE PARAMETERS AND CALCULATE THROW DISTANCE FROM TARGET ()

# Release Parameters

horizontal_angle <- c(-3, 1, -1)
vertical_angle <- c(1, -3, -2)
distance <- c(16.48, 16.71, 16.16)
height <- c(1.82, 1.84, 1.83)
velocity <- c(36.1, 34.9, 35.5)
side <- c(0.05, 0.13, 0.24)

# Merge release parameters into a data frame

data <- data.frame(velocity, vertical_angle, horizontal_angle, distance, side, height)

A <- data$distance
H <- A/cos(data$horizontal_angle*pi/180) # converts horizontal_angle into radians

# Determine lateral displacement (O)

O <- sin(data$horizontal_angle*pi/180) # converts horizontal_angle into radians
O <- O + data$side # offsets lateral displacement by accounting for release side distance

# Merge A, H, and O into a data frame

trig_numbers <- data.frame(A,H,O)

throw_distance <- trig_numbers$H
data$throw_distance <- throw_distance

####################################################################################
# THE ABOVE CODE GETS ME THE TOTAL DISTANCE TO TARGET AND THE LATERAL DISPLACEMENT #
#                          OF THE BALL WHEN IT REACHES IT                          #
####################################################################################





# Constants
g <- -9.81  # Acceleration due to gravity (m/s^2)

# Function to calculate height at a specific distance
calculate_height <- function(velocity, vertical_angle, distance) {
  # Convert angle to radians
  theta <- vertical_angle * pi / 180
  
  # Calculate time of flight
  T <- distance / (velocity * cos(theta))
  
  # Calculate height
  height <- velocity * sin(theta) * T - 0.5 * g * T^2
  
  return(height)
}

# Let the user select the file to import
file_path <- file.choose()

# Import the data from the selected file
data <- read.csv(file_path)  # Modify the function based on your file format and structure

# Loop through each row of trig_numbers
results <- NULL
for (i in 1:nrow(trig_numbers)) {
  # Extract the values from the data frames
  velocity <- data$velocity[i]
  vertical_angle <- data$vertical_angle[i]
  distance <- trig_numbers$H[i]
  variable_O <- trig_numbers$O[i]
  
  # Calculate height at the given distance
  height <- calculate_height(velocity, vertical_angle, distance)
  
  # Store the results in a list
  result <- c(trial_number = i, velocity = velocity, vertical_angle = vertical_angle, distance = distance, height = height, lateral_disp = variable_O)
  results <- rbind(results, result)
}

# Convert the list of results to a data frame
result_df <- as.data.frame(results)

# Remove the row with the name "results"
result_df <- result_df[result_df$velocity != "results", ]

# Print the resulting data frame
print(result_df)
