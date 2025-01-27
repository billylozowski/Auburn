
# This code will import time normalised data for each trial, then plots select variables before saving them

rm(list = ls())

# Let the user select the file to import
file_path <- file.choose()

# Get a list of all CSV files in the folder
plot_data <- read.csv(file_path, check.names = FALSE) 


library(ggplot2)

# Function to plot specified variable
plot_variable <- function(data, variable) {
  # Create a factor for File Name to ensure correct order in scale
  data$`File Name` <- factor(data$`File Name`, levels = unique(data$`File Name`))
  
  # Define custom colors for File Name
  custom_colors <- colorRampPalette(c("deepskyblue", "darkblue"))(length(levels(data$`File Name`)))
  
  p <- ggplot(data, aes(x = `Duration (%)`, y = .data[[variable]], group = `File Name`, 
                        color = `File Name`)) +
    geom_line(linewidth = 1) +
    ylab(variable) +  # Set y-axis label
    scale_color_manual(values = custom_colors) +  # Use custom colors
    theme_classic() +
    theme(legend.position = "none")  # Turn off the legend
  
  return(p)
}


# Specify the variables to plot
variables <- c("Front Knee Flexion", 
               "Front Knee Extension Velocity",
               "Pelvis Rotation (Y)",
               "Pelvis Rotation Velocity",
               "Trunk Rotation (Y)",
               "Trunk Rotation Velocity",
               "Shoulder Abduction",
               "Shoulder H Abduction",
               "Shoulder Rotation",
               "Shoulder Rotation Velocity",
               "Elbow Flexion",
               "Elbow Extension Velocity",
               "COM Velocity (X)",
               "COM Velocity (Z)"
)

# Create plots for each variable and display them
for (variable in variables) {
  plot <- plot_variable(plot_data, variable)
  print(plot)
}

# Will need the codes to save each figure to the original folder location here!

