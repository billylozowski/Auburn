# Load necessary library
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Parameters
n <- 25  # Number of throws

# Generate distance values between 7.5 and 60
distance <- runif(n, min = 7.5, max = 60)

# Add small noise to ensure distance values are not duplicated
distance <- jitter(distance, factor = 0.5)

# Generate velocity values: roughly related to distance but with controlled variability
velocity <- 26 + (distance - min(distance)) * ((36 - 26) / (max(distance) - min(distance))) +
  rnorm(n, mean = 0, sd = 1)  # Reduced variability for more realistic speeds

# Initialize accuracy vector
accuracy <- numeric(n)

# Set accuracy based on distance intervals
# For 7.5 <= distance < 15, accuracy between 0 and 0.75
accuracy[distance >= 7.5 & distance < 15] <- runif(sum(distance >= 7.5 & distance < 10), min = 0, max = 1)

# For 15 <= distance < 35, accuracy between 0.1 and 1.5
accuracy[distance >= 15 & distance < 35] <- runif(sum(distance >= 10 & distance < 35), min = 0.1, max = 1.5)

# For distance >= 35, accuracy between 0.25 and 2.5
accuracy[distance >= 35] <- runif(sum(distance >= 35), min = 0.25, max = 2.5)

# Add small noise to ensure accuracy values are not duplicated
accuracy <- jitter(accuracy, factor = 0.03)

# Ensure no duplicates in accuracy values
while(any(duplicated(accuracy))) {
  dupes <- duplicated(accuracy) | duplicated(accuracy, fromLast = TRUE)
  accuracy[dupes] <- accuracy[dupes] + runif(sum(dupes), min = -0.01, max = 0.01)
}

# Add small random noise to velocity to avoid exact duplicates
velocity <- jitter(velocity, factor = 0.01)

# Combine into a data frame
ball_data <- data.frame(
  Velocity = round(velocity, 1),
  Distance = round(distance, 1),
  Accuracy = round(accuracy, 2)
)

colnames(ball_data) <- c("Velocity (m/s)", "Distance (m)", "Accuracy (m)")

# View the data
head(ball_data)

# Check if any duplicate values remain in the accuracy column
any(duplicated(ball_data$Accuracy))

# Print the dataset
print(ball_data)

