library(readxl)
data <- read_excel("Auburn/Sports Medicine & Movement Lab/PhD Thesis/3. Data/Main Study/KOT - Processed Data/Master File.xlsx")

# Assuming your dataset is a data frame called data
# Loop over each numeric column in data

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
cleaned <- remove_outliers(data)


write.csv(cleaned, "Cleaned Master File.csv", row.names = FALSE, na = "")

