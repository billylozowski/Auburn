
# This code calculates the correlation coefficients for 4 segments in the Fibonacci Sequence analysis

# Let the user choose the folder - Load from the Event Data folder
folder_path <- choose.dir()

# Get a list of all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each CSV file, read it, and combine it into the 'combined_data' data frame
for (csv_file in csv_files) {
  # Read the CSV file
  current_data <- read.csv(csv_file, header = TRUE,check.names = FALSE)
  
  # Combine the data frames
  combined_data <- rbind(combined_data, current_data)
}

combined_data <- combined_data %>%
  mutate(Distance = substr(Distance, 1, 2) %>% as.numeric())

# Mean values
mean60 <- combined_data %>%
  filter(Distance == "60") %>%
  group_by(Participant) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) 

mean84 <- combined_data %>%
  filter(Distance == "84") %>%
  group_by(Participant) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) 

# Specify the folder to save the data to
output_folder <- "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/Misc/Fibonacci"

# Create the folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

# Save correlation stats
write.csv(combined_data, file = file.path(output_folder, "Combined Data.csv"), row.names = FALSE)
write.csv(mean60, file = file.path(output_folder, "Mean 60.csv"), row.names = FALSE)
write.csv(mean84, file = file.path(output_folder, "Mean 84.csv"), row.names = FALSE)

# Correlation coefficients 60
stats60 <- data.frame("Shoulder (Fib)" = cor(mean60$Shoulder, mean60$`Predicted Shoulder (Fib)`),
                    "Elbow (Fib)" = cor(mean60$Elbow, mean60$`Predicted Elbow - (Fib)`),
                    "Hand (Fib)" = cor(mean60$Hand, mean60$`Predicted Hand - (Fib)`),
                    "Shoulder (hip2)" = cor(mean60$Shoulder, mean60$`Predicted Shoulder (hip2)`),
                    "Elbow (hip2)" = cor(mean60$Elbow, mean60$`Predicted Elbow (hip2)`),
                    "Hand (hip2)" = cor(mean60$Hand, mean60$`Predicted Hand (hip2)`)
                    )

# Correlation coefficients 84
stats84 <- data.frame("Shoulder (Fib)" = cor(mean84$Shoulder, mean84$`Predicted Shoulder (Fib)`),
                    "Elbow (Fib)" = cor(mean84$Elbow, mean84$`Predicted Elbow - (Fib)`),
                    "Hand (Fib)" = cor(mean84$Hand, mean84$`Predicted Hand - (Fib)`),
                    "Shoulder (hip2)" = cor(mean84$Shoulder, mean84$`Predicted Shoulder (hip2)`),
                    "Elbow (hip2)" = cor(mean84$Elbow, mean84$`Predicted Elbow (hip2)`),
                    "Hand (hip2)" = cor(mean84$Hand, mean84$`Predicted Hand (hip2)`)
                    )

# Save correlation stats
write.csv(stats60, file = file.path(output_folder, "Measured vs. Predicted Agreement (60 correlation).csv"), row.names = FALSE)
write.csv(stats84, file = file.path(output_folder, "Measured vs. Predicted Agreement (84 correlation).csv"), row.names = FALSE)


# Load necessary library
library(rmcorr)

# Calculate the agreement between predicted and observed values

# Perform repeated measures correlation
shoulder_result <- rmcorr(participant = Participant, 
                     measure1 = Shoulder, 
                     measure2 = `Predicted Shoulder (Fib)`, 
                     dataset = combined_data)

elbow_result <- rmcorr(participant = Participant, 
                          measure1 = Elbow, 
                          measure2 = `Predicted Elbow - (Fib)`, 
                          dataset = combined_data)

hand_result <- rmcorr(participant = Participant, 
                          measure1 = Hand, 
                          measure2 = `Predicted Hand - (Fib)`, 
                          dataset = combined_data)

shoulder_result2 <- rmcorr(participant = Participant, 
                          measure1 = Shoulder, 
                          measure2 = `Predicted Shoulder (hip2)`, 
                          dataset = combined_data)

elbow_result2 <- rmcorr(participant = Participant, 
                       measure1 = Elbow, 
                       measure2 = `Predicted Elbow (hip2)`, 
                       dataset = combined_data)

hand_result2 <- rmcorr(participant = Participant, 
                      measure1 = Hand, 
                      measure2 = `Predicted Hand (hip2)`, 
                      dataset = combined_data)


# Add r coefficient to Agreement
Agreement <- setNames(
  data.frame(
    shoulder_result$r,
    elbow_result$r,
    hand_result$r,
    shoulder_result2$r,
    elbow_result2$r,
    hand_result2$r
    
  ),
  c("Shoulder Agreement (Fib)", "Elbow Agreement (Fib)", "Hand Agreement (Fib)",
    "Shoulder Agreement (hip2)", "Elbow Agreement (hip2)", "Hand Agreement (hip2)")
)

# Make sure to specify the correct file name!!!
write.csv(Agreement, file = file.path(output_folder, "Measured vs. Predicted Agreement (rm correlation).csv"), row.names = FALSE)

rm(list = ls())
