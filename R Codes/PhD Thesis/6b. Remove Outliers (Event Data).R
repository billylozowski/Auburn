library(readxl)
data <- read_excel("Auburn/Sports Medicine & Movement Lab/PhD Thesis/3. Data/Main Study/KOT - Processed Data/Event Data (combined).xlsx")
# additional <- read_csv("Auburn/Sports Medicine & Movement Lab/PhD Thesis/3. Data/Main Study/KOT - Processed Data/Additional Data (combined).csv")

library(dplyr)
BFC <- data %>%
  filter(Event == "BFC")

FFC <- data %>%
  filter(Event == "FFC")

MAW <- data %>%
  filter(Event == "MAW")

BR <- data %>%
  filter(Event == "BR")

EFT <- data %>%
  filter(Event == "EFT")

Max <- data %>%
  filter(Event == "Max")

Min <- data %>%
  filter(Event == "Min")

# Loop over each numeric column in a data frame

remove_outliers <- function(data) {
  data[] <- lapply(data, function(column) {
    if (is.numeric(column)) {
      # Calculate the mean and standard deviation
      mean_col <- mean(column, na.rm = TRUE)
      sd_col <- sd(column, na.rm = TRUE)
      
      # Set thresholds for outliers
      lower_limit <- mean_col - 3 * sd_col
      upper_limit <- mean_col + 3 * sd_col
      
      # Replace outliers with NA (or you can choose to filter them out)
      column[column < lower_limit | column > upper_limit] <- NA
    }
    return(column)
  })
  return(data)
}

# Apply the function to your data frame
BFC_clean <- remove_outliers(BFC)
FFC_clean <- remove_outliers(FFC)
MAW_clean <- remove_outliers(MAW)
BR_clean <- remove_outliers(BR)
EFT_clean <- remove_outliers(EFT)
Min_clean <- remove_outliers(Min)
Max_clean <- remove_outliers(Max)
# additional_cleaned <- remove_outliers(additional)

# Combine all cleaned Event Data
cleaned <- rbind(BFC_clean, FFC_clean, MAW_clean, BR_clean, EFT_clean, Min_clean, Max_clean)

cleaned <- cleaned %>%
  arrange(`File Name`) %>%
  select(`File Name`, Event, `Time (s)`, `Duration (%)`, Height, `Stride Length (% Height)`, `Stride Angle`,`Front Foot Angle`,
         `Back Knee Flexion`, `Back Knee Extension Velocity`, `Front Knee Flexion`, `Front Knee Extension Velocity`, 
         `Pelvis Rotation (Y)`, `Pelvis Rotation Velocity`, `Pelvis-Trunk Separation`,  `Trunk Rotation (Y)`,
         `Trunk Rotation Velocity`, `Trunk Flexion`, `Trunk Flexion Velocity`, `Lateral Trunk Flexion`, 
         `Shoulder Abduction`, `Shoulder H Abduction`, `Shoulder H Abduction Velocity`, `Shoulder Rotation`,
         `Shoulder Rotation Velocity`,`Elbow Flexion`, `Elbow Extension Velocity`, `Hand COM (X)`, `Hand COM (Y)`,
         `Hand COM (Z)`,`COM (X)`, `COM Velocity (X)`, `COM (Y)`, `COM (Z)`, `COM Velocity (Z)`)

write.csv(cleaned, "Cleaned Event Data (combined).csv", row.names = FALSE, na = "")
# write.csv(additonal_cleaned, "Cleaned Additional Data (combined).csv", row.names = FALSE, na = "")
