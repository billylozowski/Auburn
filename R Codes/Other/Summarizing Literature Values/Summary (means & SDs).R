# This script summarises data by variable and event (mean, min, max)

# Let the user select the file to import
file_path <- file.choose()

# Load Packages
library(readxl)
library(dplyr)
library(tidyr)

# Read the data, and remove unnecessary columns
data <- read_excel(file_path, sheet = "Master File", col_names = TRUE) 
    # mutate(across(2:30, as.numeric))

result <- data %>%
  group_by(Participant = data[, 2], Ball = data[, 5]) %>%
  summarize(across(names(data)[7:37], ~mean(., na.rm = TRUE), .names = "{.col} (mean)"),
            across(names(data)[7:37], ~sd(., na.rm = TRUE), .names = "{.col} (sd)"))

result <- ungroup(result)

# Write 'result' to a CSV file
write.csv(result, file = "CK Scaled Ball Master File (summary).csv", row.names = FALSE)
