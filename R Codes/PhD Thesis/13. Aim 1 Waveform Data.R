# Load the file with combined, time-normalised data
library(readr)
data <- read_csv("Academics/2. Auburn/Sports Medicine & Movement Lab/PhD Thesis/2. Chapters/Chapter 4 - Aim 1/Data/Files for Waveform Plots/Time Normalised Data (combined).csv")

library(tidyr)
library(dplyr)
library(ggplot2)

# Rename Selected Variables
colnames(data)[colnames(data) == "Back Knee Flexion"] <- "Back Knee Fle-Ext"
colnames(data)[colnames(data) == "Front Knee Flexion"] <- "Front Knee Fle-Ext"
colnames(data)[colnames(data) == "Back Knee Extension Velocity"] <- "Back Knee Fle-Ext Velocity"
colnames(data)[colnames(data) == "Front Knee Extension Velocity"] <- "Front Knee Fle-Ext Velocity"
colnames(data)[colnames(data) == "Pelvis Rotation (Y)"] <- "Pelvis Rotation"
colnames(data)[colnames(data) == "Trunk Flexion"] <- "Trunk Fle-Ext"
colnames(data)[colnames(data) == "Trunk Flexion Velocity"] <- "Trunk Fle-Ext Velocity"
colnames(data)[colnames(data) == "Trunk Rotation (Y)"] <- "Trunk Rotation"
colnames(data)[colnames(data) == "Shoulder H Abduction"] <- "Shoulder H Abd-Add"
colnames(data)[colnames(data) == "Shoulder H Abduction Velocity"] <- "Shoulder H Abd-Add Velocity"
colnames(data)[colnames(data) == "Elbow Flexion"] <- "Elbow Fle-Ext"
colnames(data)[colnames(data) == "Elbow Extension Velocity"] <- "Elbow Fle-Ext Velocity"


# Loop through each variable (excluding the file name and duration columns)
for (i in 3:ncol(data)) {
  
  # Create a new data frame with the file name, duration, and the current column (variable)
  new_df <- data[, c(1, 2, i)]  # File name (column 1), Duration (column 2), and the variable (column i)
  
  # Assign a name to the new data frame based on the column name
  variable_name <- colnames(data)[i]
  
  # Rename the third column to "Value" for simplicity in plotting
  colnames(new_df)[3] <- "Value"
  
  # Generate the plot for the current variable
  p <- ggplot(new_df, aes(x = Duration, y = Value, group = `File Name`)) +  # Use `File Name` to group participants
    geom_line(color = "black", linewidth = 0.2) +  # Plot lines with black color and width 0.2
    labs(title = variable_name,  
         x = "Duration %", 
         y = NULL) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Save the plot as a PNG file
  ggsave(filename = paste0(variable_name, " (all).png"), plot = p, dpi = 600, height = 7, width = 9)
}


# Initialize an empty data frame for the Waveform Data
waveform_data <- data.frame(Duration = seq(0, 100))


# Loop through the columns starting from the second one (since the first column is the file name)
for (i in 2:ncol(data)) {
  # Create a new data frame with the file name and the current column (variable)
  new_df <- data[, c(1, i)]
  
  # Rename the columns for clarity
  colnames(new_df) <- c("File_Name", "Value")
  
  # Add an index column to track the time series (assuming 101 rows per participant)
  new_df$Index <- rep(1:101, times = length(unique(new_df$File_Name)))

  
  # Pivot the data to wide format
  wide_df <- pivot_wider(new_df, names_from = File_Name, values_from = Value, id_cols = Index)
  
  # Remove the Index column after pivoting
  wide_df <- select(wide_df, -Index)
  
  # Calculate row means across all participants (columns)
  mean_column_name <- paste("Mean", colnames(data)[i])
  wide_df[[mean_column_name]] <- rowMeans(wide_df, na.rm = TRUE)
  
  # Calculate row standard deviation across all participants (columns)
  sd_column_name <- paste("SD", colnames(data)[i])
  wide_df[[sd_column_name]] <- apply(wide_df, 1, sd, na.rm = TRUE)
  
  # Add the Mean and SD columns to the Waveform Data data frame
  waveform_data[[mean_column_name]] <- wide_df[[mean_column_name]]
  waveform_data[[sd_column_name]] <- wide_df[[sd_column_name]]
  
  # Assign the wide data frame to a variable in the global environment
  assign(paste0("wide_", colnames(data)[i]), wide_df)
}

waveform_data <- waveform_data %>%
  select(-`Mean Duration`, -`SD Duration`)


# Specify the folder to save the data to
output_folder <- "C:/Users/billy/OneDrive/Documents/Academics/2. Auburn/Sports Medicine & Movement Lab/PhD Thesis/2. Chapters/Chapter 4 - Aim 1/Data/Files for Waveform Plots"

# Create the folder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

# Save the data frame to a CSV file in the "Files for Waveform Plots" folder
write.csv(waveform_data, file = file.path(output_folder, "Time Normalised Waveform Data (Mean & SD).csv"), row.names = FALSE)


# Plot mean & sd waveforms for each variable

# Get the number of columns in the data frame
num_columns <- ncol(waveform_data)

# Loop through the even columns (mean values) and their corresponding odd columns (SD values)
for (i in seq(2, num_columns, by = 2)) {
  # Extract the variable name from the column names
  variable_name <- colnames(waveform_data)[i]
  sd_variable_name <- colnames(waveform_data)[i + 1]  # SD column is the next one
  
  # Create a plot
  plot_data <- waveform_data[, c(1, i, i + 1)]  # Keep Duration, Mean, and SD columns
  colnames(plot_data) <- c("Duration", "Mean", "SD")  # Rename for clarity
  
  # Calculate upper and lower bounds for SD
  plot_data$Upper <- plot_data$Mean + plot_data$SD
  plot_data$Lower <- plot_data$Mean - plot_data$SD
  
  # Generate the plot
  p <- ggplot(plot_data, aes(x = Duration)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.5, fill = "pink") +  # Plot the SD bounds
    geom_line(aes(y = Mean), color = "black", linewidth = 1) +  # Plot the Mean
    labs(title = NULL,  # Remove the title
         x = "Throw %",
         y = NULL) +
    theme_classic() +  # Use classic theme for a clean white background
    theme(axis.title.y = element_text(margin = margin(r = 10)),  # Add some space to the right of y-axis
          axis.title.x = element_text(margin = margin(t = 10), size = 12)) +  # Add some space above x-axis
#   geom_vline(xintercept = 0, colour = "black", linetype = "dashed") +
    geom_vline(xintercept = 45, colour = "black", linetype = "dashed") +
    geom_vline(xintercept = 44, colour = "black", linetype = "dashed") +
    geom_vline(xintercept = 68, colour = "black", linetype = "dashed") +
#   geom_vline(xintercept = 100, colour = "black", linetype = "dashed") +
    
    # Add labels for the vlines, slightly adjusting positions for clarity
#   annotate("text", x = 0, y = max(plot_data$Upper) + 0.05, label = "BFC", hjust = -0.2, vjust = -0.5, size = 4) +
    annotate("text", x = 44, y = max(plot_data$Upper) + 0.05, label = "MAW", hjust = 1.2, vjust = 1) +
    annotate("text", x = 45, y = max(plot_data$Upper) + 0.05, label = "FFC", hjust = -0.2, vjust = 1) +
    annotate("text", x = 68, y = max(plot_data$Upper) + 0.05, label = "BR", hjust = 1.2, vjust = 1) +
#   annotate("text", x = 100, y = max(plot_data$Upper) + 0.05, label = "EFT", hjust = 1.2, vjust = -0.5, size = 4) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 101))  # Remove padding at the ends of x-axis
    
  
  # Save the plot as a PNG file
  ggsave(filename = paste(variable_name, ".png", sep = ""), plot = p, dpi = 600, height = 4, width = 6)
}

