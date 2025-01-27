
# This script summarises data by variable and event (mean, min, max)

# Let the user select the file to import
file_path <- file.choose()

# Load Packages
library(readxl)
library(dplyr)
library(tidyr)

# Read the data, and remove unnecessary columns
data <- read_excel(file_path, sheet = "Pitching - at events", col_names = TRUE) %>%
  select(-Article, `Stride orientation`) %>%
  mutate(across(2:30, as.numeric))

# Convert to a long format
long_data <- pivot_longer(data, cols = 2:30)

# Specify Event Order
event_order <- c("FFC", "MER", "BR", "MIR", "MAX")

# Summarise Data
summary_data <- long_data %>%
  group_by(name, Event) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE)
  ) %>%
  filter(!is.nan(Mean)) %>% # remove row where Mean == NaN
  arrange(factor(Event, levels = event_order))

summary_data <- summary_data %>%
  mutate(across(2:4, as.numeric))

# Round columns to desired decimal places
summary_data$Mean <- sprintf("%.1f", summary_data$Mean)
summary_data$Min <- sprintf("%.1f", summary_data$Min)
summary_data$Max <- sprintf("%.1f", summary_data$Max)

# Write 'summary_data' to a CSV file
write.csv(summary_data, "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/
          PhD Thesis/Final Thesis/Aims/Aim 1/Aim 1 Pitching - at events (summary).csv", row.names = FALSE)
