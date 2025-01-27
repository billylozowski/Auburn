
# This script determines the location of the ball at the target based off the 
# initial release parameters using projectile motion equations.

# I want to have the option to select a file with a GUI (lines 8-14 currently)


# IMPORT DATA FROM FILE

# Let the user select the file to import
  file_path <- file.choose()

# Import the data from the selected file
  library(readxl)
  data <- read_excel(file_path)  # Modify the function based on your file format and structure

# Filter rows with "fastball" in the "TaggedPitchType" column  
  library(tidyverse)
  data <- data %>%
    filter(TaggedPitchType == "Fastball")

# Extract variables of interest
  
  # Trackman
  library(tidyverse)
  ball_data <- data %>%
    select(PitchNo, Time, PitcherId, TaggedPitchType, RelSpeed, VertRelAngle, HorzRelAngle, RelHeight, RelSide, Extension)
  
  plate_data <- data %>%
    select(PlateLocHeight, PlateLocSide)
  
    # Rapsodo
  # library(tidyverse)
  # ball_data <- data %>%
  #   select(No, HitID, `ExitVelocity (kph)`, StrikeZoneX, StrikeZoneY, LaunchAngle, ExitDirection)

# Remove rows with all NA values
  ball_data <- ball_data[complete.cases(ball_data), ]
  
  
###########################################################################################


# SET THROW DISTANCE, CONVERT UNITS, AND ESTABLISH RELEASE DISTANCE FROM TARGET
  
# Throw length (start point to target)  
  throw_length <- 18.288 # (60 ft in metres)
  # throw_length <- 18.44 # (60.5 ft in metres)

# Conversion units (use as necessary)  
  ft_to_m <- 0.3048
  in_to_m <- 0.0254
  mph_to_mps <- 0.44704
  # kph_to_mps <- 0.27777778
  
  
# DEFINE RELEASE PARAMETERS AND CALCULATE THROW DISTANCE FROM TARGET ()

# Release Parameters
  # Eventually, this will be data from an excel worksheet

  pitcher_id <- ball_data$PitcherId
  horizontal_angle <- ball_data$HorzRelAngle
  vertical_angle <- ball_data$VertRelAngle
  distance <- throw_length - (ball_data$Extension * ft_to_m) # throw_length - extension
  height <- ball_data$RelHeight * ft_to_m
  velocity <- ball_data$RelSpeed * mph_to_mps
  side <- ball_data$RelSide * mph_to_mps

# Merge release parameters into a data frame

  converted_data <- data.frame(pitcher_id, velocity, vertical_angle, horizontal_angle, distance, side, height)

  A <- converted_data$distance
  H <- A/cos(converted_data$horizontal_angle * pi/180) # converts horizontal_angle into radians

# Determine lateral displacement (O)
  O <- sin(converted_data$horizontal_angle * pi/180) * H # converts horizontal_angle into radians
  Offset_O <- O + converted_data$side # offsets lateral displacement by accounting for release side distance

# Merge A, H, and O into a data frame

  trig_data <- data.frame(A,H,O,Offset_O)

  throw_distance <- trig_data$H
  data$throw_distance <- throw_distance # don't think I need this line
  
  
################################################################################
  

# CALCULATE BALL HEIGHT AT TARGET (m)

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

# Loop through each row of trig_numbers
  results <- NULL
  for (i in 1:nrow(trig_data)) {

# Extract the values from the data frames
  velocity <- converted_data$velocity[i]
  vertical_angle <- converted_data$vertical_angle[i]
  distance <- trig_data$H[i]
  variable_O <- trig_data$Offset_O[i]
  
# Calculate height at the given distance
  height <- calculate_height(velocity, vertical_angle, distance)
  
# Store the results in a list
  result <- c(trial_number = i, velocity = velocity, vertical_angle = vertical_angle, distance = distance, height = height, lateral_disp = variable_O)
  results <- rbind(results, result)
}

# Convert the list of results to a data frame
  results_df <- as.data.frame(results)

# Remove the row with the name "results"
  results_df <- results_df[results_df$velocity != "results", ]

# Print the resulting data frame
  print(results_df)
 
  
################################################################################  
  
  
# REMOVE THROWS THAT BOUNCED BEFORE THEY REACHED THE TARGET
  
# Extract only trials where the ball didn't bounce before reaching the target
  no_bounce <- results_df[results_df$height > 0, ]
  
# Print the filtered data frame
  print(no_bounce) 
  

################################################################################
  

# TRIANGULATING THROW LOCATION FROM AIM POINT
  
# Set aim point
  target_height <- 30 * in_to_m # change this value as required
  target_side <- 0 # change this value as required (only when pitching to off-centre quadrants)
  target_point <- data.frame(target_height, target_side)
  
# Create a scatter plot
  library(ggplot2)
  
  ggplot(no_bounce, aes(x = lateral_disp, y = height, color = trial_number)) +
    geom_point() +
    geom_point(data = target_point, aes(x = target_side, y = target_height), shape = 4, color = "red", size = 4, stroke = 2) + #fill = "red"
    scale_x_continuous(limits = c(-1.5, 1.5)) +
    scale_y_continuous(limits = c(0, 2.5)) +
    labs(x = "Lateral Displacement (m)", y = "Vertical Displacement (m)", color = "Trial Number")
    
# Triangulate each throw from 'target_point' using vertical and horizontal displacements

# Offset each 'height' and 'lateral_disp' value by the target location
  no_bounce$offset_height <- no_bounce$height - target_point$target_height
  # no_bounce$offset_LD <- no_bounce_lateral_disp - target_point$target_side 
    # un-comment this when the target_side value is not '0'
  
# Calculate displacement for each row
  displacement <- sqrt(no_bounce$offset_height^2 + no_bounce$lateral_disp^2) 
    # may want to multiply this by 100 to avoid 0.00 values in the throwing score
  
# Create a new data frame with calculated displacement
  triangulated_result <- data.frame(no_bounce, displacement)
  
# Print the result
  print(triangulated_result)
  
  
################################################################################
  
  
# CALCULATE THE THROW'S SCORE (VD/S).
  # where V = velocity, D = throw distance, and S = displacement from target_point
  
  triangulated_result$throw_score <- ((triangulated_result$velocity * triangulated_result$distance) / triangulated_result$displacement) / 100 # /100 is a scaling factor!

# RENAME COLUMN HEADERS
  colnames(triangulated_result) <- c("Trial", "Velocity (m/s)", "Release Angle", "Throw Distance", 
                                   "Vertical Displacement (m)", "Lateral Displacement (m)", "Height From Target (m)",
                                   "Displacement (m)", "Throw Score")

# Rename Output
  Final_Output <- triangulated_result

# Print Output
  print(Final_Output) # this can be commented out if necessary
  
# Write the Final_Output to a CSV file (rename the file as necessary)
   write.csv(Final_Output, file = "Georgia data.csv", row.names = TRUE)
   
# Write the Final_Output to a TXT file (rename the file as necessary)
   # write.table(Final_Output, file = "Georgia data.txt", sep = "\t", row.names = TRUE)

   
##################################################################################
# THE CODE BELOW NEEDS SOME WORK, BUT WILL ESSENTIALLY PLOT CONFIDENCE INTERVALS #  
##################################################################################
  
  
# PLOT A CONFIDENCE ELLIPSE AROUND THE DATA
  
  library(ggplot2)
  ggplot(data = Final_Output, aes(x = `Lateral Displacement (m)`, y = `Vertical Displacement (m)`)) +
    geom_point() +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(limits = c(-1, 2.5)) +
    stat_ellipse(level = 0.95, type = "norm", color = "red", fill = NA)
  
# Create a matrix from the 'Lateral Displacement (m)' and 'Vertical Displacement (m)' variables
  data_matrix <- cbind(Final_Output$`Lateral Displacement (m)`, Final_Output$`Vertical Displacement (m)`)
  
# Perform principal component analysis
  pca <- prcomp(data_matrix, scale = TRUE)
  
# Extract the eigenvectors and eigenvalues
  eigenvectors <- pca$rotation
  eigenvalues <- pca$sdev^2
  
# Calculate the major and minor radii
  major_radius <- sqrt(eigenvalues[1])
  minor_radius <- sqrt(eigenvalues[2])
  
# Compute the inclination of the major axis (in degrees)
  major_inclination <- atan(eigenvectors[2, 1] / eigenvectors[1, 1]) * (180 / pi)
  
# Print the major and minor radii and the major inclination
  cat("Major Radius:", major_radius, "\n")
  cat("Minor Radius:", minor_radius, "\n")
  cat("Major Inclination:", major_inclination, "degrees\n")
  
  
###################################################################################
# THE CODE ABOVE PLOTS ONLY THE CONFIDENCE ELLIPSE, NOT THE RADII. THE CODE BELOW #
#               PLOTS C.E. AND RADII, BUT THE RADII ARE NOT CORRECT!              #
###################################################################################
  
  
  library(ggplot2)
  
# Calculate the major and minor radii
  major_radius <- sqrt(pca$sdev[1])
  minor_radius <- sqrt(pca$sdev[2])
  
# Compute the inclination of the major axis (in degrees)
  major_inclination <- atan(eigenvectors[2, 1] / eigenvectors[1, 1]) * (180 / pi)
  
# Calculate the center of the confidence ellipse
  centre_point <- data.frame(x = colMeans(data_matrix)[1], y = colMeans(data_matrix)[2])
  
# Plot the data points, confidence ellipse, and radii
  ggplot(data = Final_Output, aes(x = `Lateral Displacement (m)`, y = `Vertical Displacement (m)`)) +
    geom_point() +
    stat_ellipse(level = 0.95, geom = "polygon", fill = "transparent", color = "red") +
    geom_segment(x = 0, y = 0, xend = major_radius * cos(major_inclination * (180 / pi)), 
                 yend = major_radius * sin(major_inclination * (180 / pi)), color = "blue") +
    geom_segment(x = 0, y = 0, xend = minor_radius * cos((major_inclination + 90) * (180 / pi)), 
                 yend = minor_radius * sin((major_inclination + 90) * (180 / pi)), color = "green") +
    geom_point(data = centre_point, aes(x, y), shape = 4, color = "purple", size = 3, stroke = 2) 
    # coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 2.5)) 
  
# Ensures equal aspect ratio
  # coord_equal()    