
# Let the user select the file to import
file_path <- file.choose()

# Load Rapsodo .csv File - need to actually select the excel file
library(readr)
data <- read_csv(file_path, col_names = TRUE) 

library(dplyr)
complete_data <- data %>%
  select(10:18, 25:66)

complete_data <- complete_data[complete.cases(complete_data), ]

write.csv(complete_data, file = "C:/Users/billy/OneDrive/Documents/Auburn/Classes/7. Spring 2024/ERMA 8340 - An Practical Guide to Structural Equation Modelling/HBM (complete).csv", row.names = FALSE)
