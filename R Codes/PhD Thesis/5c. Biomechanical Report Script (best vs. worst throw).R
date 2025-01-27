
# This code will plot a participant's best and worst throw according to AIM.

# Load the "~ Plot Data" .csv for their best throw
best_file_path <- file.choose()
best <- read.csv(best_file_path, check.names = FALSE)

# Load the "~ Plot Data" .csv for their worst throw
worst_file_path <- file.choose()
worst <- read.csv(worst_file_path, check.names = FALSE)

# Add a new column to distinguish between the best and worst throws
best$Type <- 'Best'
worst$Type <- 'Worst'

# Combine Throws into a single data frame
throws_to_plot <- rbind(best, worst)


library(ggplot2)

# Function to plot variable from combined data frame
plot_variable <- function(data, variable) {
  # Create a factor for Type to ensure correct order in scale
  data$Type <- factor(data$Type, levels = c('Best', 'Worst'))
  
  # Define custom colors for Type
  custom_colours <- c("blue", "darkorange")
  
  p <- ggplot(data, aes(x = `Duration (%)`, y = .data[[variable]], group = `Type`, 
                        color = `Type`)) +
    geom_line(linewidth = 1) +
    ylab(variable) +  # Set y-axis label
    scale_color_manual(values = custom_colours) +  # Use custom colors
    theme_classic() +
    geom_vline(xintercept = 100, color = "red", linetype = "dashed", linewidth = 0.75) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank())
  
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

# Directory for saving plots (using the directory from best_file_path)
output_folder <- file.path(dirname(best_file_path), "Best vs. Worst Plots")  # Creating "Report Plots" subfolder

# Create the subfolder if it doesn't exist
dir.create(output_folder, showWarnings = FALSE)

# Loop over each variable, create and save the plot
for (variable in variables) {
  plot <- plot_variable(throws_to_plot, variable)
  ggsave(file.path(output_folder, paste0(variable, ".png")), plot, width = 10, height = 6, dpi = 600)
  print(plot)
}

rm(list = ls())
