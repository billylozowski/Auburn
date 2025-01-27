
# This code extracts peak linear velocities for hip, shoulder, elbow, and hand 
# for use in a Fibonacci Sequence analysis

# Load the desired .txt file
file_path <- file.choose()

# Read the data
data <- read.delim(file_path, header = TRUE, skip = 8, check.names = FALSE)
file_name <- tools::file_path_sans_ext(basename(file_path))

# Remove column "8" as it's empty
library(dplyr)
data <- data %>%
  select(! 8)

################################################################################

# Front Foot Contact (FFC)
FFC_condition <- data$`Event - FFC` > 20  # define FFC condition

# Find the index of the first occurrence
FFC_index <- which(FFC_condition)[1]

# Ball Release (BR)
BR_index <- which.max(data$Hand) + 1

# End of Follow-Through
subset_data <- data[BR_index:min(BR_index + 150, nrow(data)), ] # Subset the data from BR to BR +150 (or until the end of the data)

# Find the index of the minimum 'Event - EFT' value in the subset
EFT_index <- which.min(subset_data$`Event - EFT`)

# Adjust the index to reflect the original data frame
EFT_index <- BR_index + EFT_index - 1

data <- data[FFC_index:EFT_index, ]

################################################################################

# Find peak linear velocities for each segment
peaks <- data.frame("Hip" = max(data$Hip, na.rm = TRUE),
                    "Shoulder" = max(data$Shoulder, na.rm = TRUE),
                    "Elbow" = max(data$Elbow, na.rm = TRUE),
                    "Hand" = max(data$Hand, na.rm = TRUE))

# Index peak linear velocity values
peaks$`Hip index` <-which.max(data$Hip)
peaks$`Shoulder index` <-which.max(data$Shoulder)
peaks$`Elbow index` <-which.max(data$Elbow)
peaks$`Hand index` <-which.max(data$Hand)

# Add a File Name column
peaks$`File Name` <- file_name
peaks$Participant <- substr(peaks$`File Name`, 1, 7)
peaks$Distance <- substr(peaks$`File Name`, 21, 22)
peaks$Trial <- substr(peaks$`File Name`, 25, 25)

# Define Fibonacci ratio
Fibonacci <- (1+sqrt(5))/2

# Add predicted linear velocities based on the Fibonacci ratio
peaks$`Predicted Shoulder (Fib)` <- peaks$Hip * Fibonacci
peaks$`Predicted Elbow (Fib)` <- peaks$`Predicted Shoulder (Fib)` + (peaks$Hip * Fibonacci)
peaks$`Predicted Hand (Fib)` <- peaks$`Predicted Elbow - (Fib)` + peaks$`Predicted Shoulder (Fib)`

# Add predicted linear velocities based on the Hip velocity * 2
peaks$`Predicted Shoulder (hip2)` <- peaks$Hip * 2
peaks$`Predicted Elbow (hip2)` <- peaks$`Predicted Shoulder (hip2)` + (peaks$Hip * 2)
peaks$`Predicted Hand (hip2)` <- peaks$`Predicted Elbow (hip2)` + peaks$`Predicted Shoulder (hip2)`

# Rearrange peaks data frame
peaks <- peaks %>%
  select(`File Name`, Participant, Distance, Trial, `Hip index`, Hip,
         `Shoulder index`, Shoulder, `Predicted Shoulder (Fib)`, `Predicted Shoulder (hip2)`,
         `Elbow index`, Elbow, `Predicted Elbow - (Fib)`, `Predicted Elbow (hip2)`,
         `Hand index`, Hand, `Predicted Hand - (Fib)`, `Predicted Hand (hip2)`)

# Specify the folder path for saving the data
output_folder <- "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/Misc/Fibonacci/Predicted Velocities"

# Create the folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

# Save plot_data as CSV to the specified folder and file name
write.csv(peaks, file = file.path(output_folder, paste0(file_name, ".csv")), row.names = FALSE)