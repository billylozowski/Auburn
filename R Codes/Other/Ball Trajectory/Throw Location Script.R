# A script for calculating ball trajectory, x & y location at target, dispersion 
# ellipse, and triangulated distance from intended target.
# This script will also score each throw based on its velocity, distance thrown 
# from, and displacement from intended target (Vk / d)

install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")

library(readxl)
rapsodo_throwing_test <- read_excel("Auburn/SMML/Manuscripts/Positional Throwing/rapsodo throwing test.xlsx", 
                                    sheet = "metric", range = "A5:M20", na = "0")

## Allow user to specify folder, then load all Excel files within it
# library(readxl)
# folder_path <- choose.dir()

## Read all Excel files in the selected folder
# files <- list.files(folder_path, pattern = "*.xlsx", full.names = TRUE)
# data <- lapply(files, read_excel)

# Re-organise data to preferred column order
library("tidyverse")
RTT <- rapsodo_throwing_test %>%
select(No, HitID, `ExitVelocity (kph)`, StrikeZoneX, StrikeZoneY, LaunchAngle, ExitDirection, `Rapsodo Distance`)

# Remove rows with all NA values
RTT_filtered <- RTT[complete.cases(RTT), ]
RTT_filtered

# Convert ball velocity from kph to m/s, then remove kph column
library(dplyr)
RTT_filtered$BallVelocity_mps <- RTT_filtered$`ExitVelocity (kph)`* 0.27777778
RTT_filtered$`ExitVelocity (kph)` <- NULL

# Rename Strike Zone locations to reflect release height and side, then remove Strike Zone columns
# Rename Exit Direction variable to LaunchDirection
library(dplyr)
RTT_filtered$ReleaseSide_m <- RTT_filtered$StrikeZoneX/100
RTT_filtered$ReleaseHeight_m <- RTT_filtered$StrikeZoneY/100
RTT_filtered$LaunchDirection <- RTT_filtered$ExitDirection
RTT_filtered$StrikeZoneX <- NULL
RTT_filtered$StrikeZoneY <- NULL
RTT_filtered$ExitDirection <- NULL

# Compute the mean & standard deviation for exit velocity
mean_exit_velocity <- mean(RTT_filtered$BallVelocity_mps)  
SD_exit_velocity <- sd(RTT_filtered$BallVelocity_mps)

# Plot ball velocity (points)
library(ggplot2)
ggplot(data = RTT_filtered, aes(x = No, y = BallVelocity_mps)) +
  geom_point() +
  geom_hline(yintercept = mean_exit_velocity, color = "red", linetype = "dashed") +
  geom_text(aes(label = sprintf("Mean: %.1f", mean_exit_velocity)),
            x = 6.0, y = mean_exit_velocity, vjust = -1.0, color = "red", fontface = "bold") +
  geom_hline(yintercept = mean_exit_velocity + 2*SD_exit_velocity, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = mean_exit_velocity - 2*SD_exit_velocity, linetype = "dashed", color = "blue") +
  labs(x = "Trial Number", y = "Ball Velocity (m/s))")

## Plot ball velocity (bars with mean and mean label)
# library(ggplot2)
# ggplot(data = RTT_filtered, aes(x = No, y = `ExitVelocity (kph)`)) +
  # geom_bar(stat = "identity") + 
  # geom_hline(yintercept = mean_exit_velocity, color = "red", linetype = "dashed") +
  # geom_text(aes(label = sprintf("Mean: %.1f", mean_exit_velocity)),
            # x = 6.0, y = mean_exit_velocity, vjust = -1.0, color = "red", fontface = "bold") +
  # geom_hline(yintercept = mean_exit_velocity + 2*SD_exit_velocity, linetype = "dashed", color = "blue") +
  # geom_hline(yintercept = mean_exit_velocity - 2*SD_exit_velocity, linetype = "dashed", color = "blue") +
  # labs(x = "Trial Number", y = "Exit Velocity (kph)")

## Remove outliers with the Z-score method
  ## Do this for each variable of interest
# z_scores <- scale(your_data$variable) 
# threshold <- 3  # Adjust the threshold as needed
# your_data <- your_data[abs(z_scores) <= threshold, ]

## Remove outliers with the Z-score method
  # Do this for each variable of interest
# Q1 <- quantile(your_data$variable, 0.25)
# Q3 <- quantile(your_data$variable, 0.75)
# IQR <- Q3 - Q1
# lower_limit <- Q1 - 1.5 * IQR
# upper_limit <- Q3 + 1.5 * IQR
# your_data <- your_data[your_data$variable >= lower_limit & your_data$variable <= upper_limit, ]

# Plot release point with 95% ellipse
library(ggplot2)
ggplot(data = RTT_filtered, aes(x = ReleaseSide_m, y = ReleaseHeight_m)) +
  geom_point() +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(0, 2.5)) +
  stat_ellipse(level = 0.95, type = "norm", color = "red", fill = NA)

# Create a matrix from the ReleaseSide_m and ReleaseHeight_m variables
data_matrix <- cbind(RTT_filtered$ReleaseSide_m, RTT_filtered$ReleaseHeight_m)

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

library(ggplot2)

# Calculate the major and minor radii
major_radius <- sqrt(pca$sdev[1])
minor_radius <- sqrt(pca$sdev[2])

# Compute the inclination of the major axis (in degrees)
major_inclination <- atan(eigenvectors[2, 1] / eigenvectors[1, 1]) * (180 / pi)

# Calculate the center of the confidence ellipse
centre_point <- data.frame(x = colMeans(data_matrix)[1], y = colMeans(data_matrix)[2])

# Plot the data points, confidence ellipse, and radii
ggplot(RTT_filtered, aes(x = ReleaseSide_m, y = ReleaseHeight_m)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", fill = "transparent", color = "red") +
  geom_segment(x = 0, y = 0, xend = major_radius * cos(major_inclination * (180 / pi)), 
               yend = major_radius * sin(major_inclination * (180 / pi)), color = "blue") +
  geom_segment(x = 0, y = 0, xend = minor_radius * cos((major_inclination + 90) * (180 / pi)), 
               yend = minor_radius * sin((major_inclination + 90) * (180 / pi)), color = "green") +
  geom_point(data = centre_point, aes(x, y), shape = 4, color = "purple", size = 3, stroke = 2) +
   coord_cartesian(xlim = c(-1, 1), ylim = c(0, 2.5)) 

# Ensures equal aspect ratio
    coord_equal()  