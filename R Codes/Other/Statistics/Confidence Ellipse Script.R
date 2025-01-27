
# Once data have been imported, this script:
# Calculates major/minor radii, plus the inclination of the confidence ellipse.
# Calculates the confidence ellipse using the 'stat_ellipse()' function
# Plots the confidence ellipse, plus the data

library(readxl)
library(dplyr)

# Read the Excel file
data <- read_excel("Auburn/SMML/Manuscripts/Kinematic Sequencing + Timing (performance)/Baseball/Data/Trackman Data (AU Spring 2023)/3-23-23 Georgia @ AU.xlsx")

# Filter rows with "fastball" in the "TaggedPitchType" column
filtered_data <- data %>%
  filter(TaggedPitchType == "Fastball")

# Conversion factors
ft_to_m <- 0.3048

filtered_data$Plate_Height_m <- filtered_data$PlateLocHeight * ft_to_m
filtered_data$Plate_Side_m <- filtered_data$PlateLocSide * ft_to_m

# Create a matrix from the Plate_Side_m and Plate_Height_m variables
data_matrix <- cbind(filtered_data$Plate_Side_m, filtered_data$Plate_Height_m)

# V1 = Side, V2 = Height

# Perform principal component analysis
pca <- prcomp(data_matrix, scale = TRUE)

# Extract the eigenvectors and eigenvalues
eigenvectors <- pca$rotation
eigenvalues <- pca$sdev^2

library(ggplot2)

# Calculate the major and minor radii
major_radius <- sqrt(pca$sdev[1])
minor_radius <- sqrt(pca$sdev[2])

# Compute the inclination of the major axis (in degrees)
major_inclination <- atan(eigenvectors[2, 1] / eigenvectors[1, 1]) * (180 / pi)

# Calculate the center of the confidence ellipse
centre_point <- data.frame(x = colMeans(data_matrix)[1], y = colMeans(data_matrix)[2])

# Plot the data points, confidence ellipse, and radii
ggplot(filtered_data, aes(x = Plate_Side_m, y = Plate_Height_m)) +
  geom_point() +
  stat_ellipse(level = 0.95, geom = "polygon", fill = "transparent", color = "red") +
  geom_segment(x = 0, y = 0, xend = major_radius * cos(major_inclination * (180 / pi)), 
               yend = major_radius * sin(major_inclination * (180 / pi)), color = "blue") +
  geom_segment(x = 0, y = 0, xend = minor_radius * cos((major_inclination + 90) * (180 / pi)), 
               yend = minor_radius * sin((major_inclination + 90) * (180 / pi)), color = "green") +
  geom_point(data = centre_point, aes(x, y), shape = 4, color = "purple", size = 3, stroke = 2) +
  labs(x = "Plate Side (m)", y = "Plate Height (m))") +
  coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(0, 2))

# Ensures equal aspect ratio 
    # coord_equal()  

# the above code does not plot the major or minor radius withing the ellipse!


# Calculate major/minor radii, plus the inclination of the confidence ellipse.
# Calculate the confidence ellpise using the 'ellipse()' function
# Plot the confidence ellipse, plus the data

library(ggplot2)
library(ellipse)

# Calculate the major and minor radii
major_radius <- sqrt(pca$sdev[1])
minor_radius <- sqrt(pca$sdev[2])

# Compute the inclination of the major axis (in degrees)
major_inclination <- atan(eigenvectors[2, 1] / eigenvectors[1, 1]) * (180 / pi)

# Generate data points for the ellipse
ellipse_points <- ellipse::ellipse(major_radius, minor_radius, angle = major_inclination, centre = colMeans(data_matrix))

# Calculate points for major and minor radii
major_radius_points <- data.frame(
  x = c(0, major_radius * cos(major_inclination * pi / 180)),
  y = c(0, major_radius * sin(major_inclination * pi / 180))
)

minor_radius_points <- data.frame(
  x = c(0, minor_radius * cos((major_inclination + 90) * pi / 180)),
  y = c(0, minor_radius * sin((major_inclination + 90) * pi / 180))
)

# Plot the data points, confidence ellipse, major and minor radii
ggplot(RTT_filtered, aes(x = ReleaseSide_m, y = ReleaseHeight_m)) +
  geom_point() +
  geom_path(data = as.data.frame(ellipse_points), aes(x, y), color = "red") +
  geom_line(data = major_radius_points, aes(x, y), color = "blue") +
  geom_line(data = minor_radius_points, aes(x, y), color = "green") +
  coord_cartesian(xlim = c(-1, 1), ylim = c(0, 2.5)) +
  coord_equal()  # Ensures equal aspect ratio

# the above code doesn't work as R doesn't recognise the ellipse() function
