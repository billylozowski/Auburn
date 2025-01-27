# Plot "Cleaned Event Data" as jitter plots with boxplots overlaid
# Calculate the IQRs for each variable (upper and lower bounds)
# Summarise data (means +/- sd) - not actually being used for Aim 1

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

###################### LOAD AND CLEAN 'CLEANED EVENT DATA' #####################

# Load "Cleaned Event Data (combined)"
file_path <- file.choose()
df_events <- read_csv(file_path)

# Convert selected columns to numeric
df_events[, 3:ncol(df_events)] <- lapply(df_events[, 3:ncol(df_events)], as.numeric)

# Create a new column called Player
df_events$Player <- substr(df_events$`File Name`, 1,8)

# Reorder df_events
df_events <- df_events %>%
  select(`File Name`, Player, everything()) %>%
  select(-5) # remove Duration %

# Convert 'Stride Length (% Height)' to non-decimal percentage
df_events$`Stride Length (% Height)` <- df_events$`Stride Length (% Height)` * 100

# Rename Selected Variables
colnames(df_events)[colnames(df_events) == "Stride Angle"] <- "Stride Angle (°)"
colnames(df_events)[colnames(df_events) == "Front Foot Angle"] <- "Front Foot Angle (°)"
colnames(df_events)[colnames(df_events) == "Back Knee Flexion"] <- "Back Knee Flex Ext"
colnames(df_events)[colnames(df_events) == "Back Knee Extension Velocity"] <- "Back Knee Flex Ext Velocity"
colnames(df_events)[colnames(df_events) == "Front Knee Flexion"] <- "Front Knee Flex Ext"
colnames(df_events)[colnames(df_events) == "Front Knee Extension Velocity"] <- "Front Knee Flex Ext Velocity"
colnames(df_events)[colnames(df_events) == "Pelvis Rotation (Y)"] <- "Pelvis Rotation"
colnames(df_events)[colnames(df_events) == "Trunk Rotation (Y)"] <- "Trunk Rotation"
colnames(df_events)[colnames(df_events) == "Trunk Flexion"] <- "Trunk Flex Ext"
colnames(df_events)[colnames(df_events) == "Trunk Flexion Velocity"] <- "Trunk Flex Ext Velocity"
colnames(df_events)[colnames(df_events) == "Trunk Flexion"] <- "Trunk Flex Ext"
colnames(df_events)[colnames(df_events) == "Shoulder Rotation"] <- "Shoulder IR ER"
colnames(df_events)[colnames(df_events) == "Shoulder Abduction"] <- "Shoulder Abd Add"
colnames(df_events)[colnames(df_events) == "Shoulder H Abduction"] <- "Shoulder H Abd Add"
colnames(df_events)[colnames(df_events) == "Shoulder H Abduction Velocity"] <- "Shoulder H Abd Add Velocity"
colnames(df_events)[colnames(df_events) == "Elbow Flexion"] <- "Elbow Flex Ext"
colnames(df_events)[colnames(df_events) == "Elbow Extension Velocity"] <- "Elbow Flex Ext Velocity"
colnames(df_events)[colnames(df_events) == "COM Velocity (X)"] <- "COMx"
colnames(df_events)[colnames(df_events) == "COM Velocity (Z)"] <- "COMz"


####################### LOAD AND CLEAN 'ADDITIONAL DATA' #######################

# Load "Additional Data (combined)"
file_path <- file.choose()
df_additional <- read_csv(file_path)

# Convert selected columns to numeric
df_additional[, 3:ncol(df_additional)] <- lapply(df_additional[, 3:ncol(df_additional)], as.numeric)

# Create a new column called Player
df_additional$Player <- substr(df_additional$`File Name`, 1,8)

# Reorder df_events
df_additional <- df_additional %>%
  select(`File Name`, Player, everything())

# Rename phases columns to remove "(ms)" from the end
colnames(df_additional)[colnames(df_additional) == "Stride (ms)"] <- "Stride"
colnames(df_additional)[colnames(df_additional) == "Transition (ms)"] <- "Transition"
colnames(df_additional)[colnames(df_additional) == "Acceleration (ms)"] <- "Acceleration"
colnames(df_additional)[colnames(df_additional) == "Follow-Through (ms)"] <- "Follow-Through"
colnames(df_additional)[colnames(df_additional) == "Total (ms)"] <- "Total"
colnames(df_additional)[colnames(df_additional) == "Arm Path (°)"] <- "Arm Path"


########### Separate Event and Additional Data into new data frames ############

# STRIDE
df_stride <- df_events %>%
  filter(Event == "FFC") %>%
  select(1,2,6:8)  # select specified variables

# BACK-FOOT CONTACT
# Proximal Joint Angles
df_BFC_prox <- df_events %>%
  filter(Event == "BFC") %>%
  select(1,2,9,13,16,15,18,20) # select specified variables

# Distal Joint Angles
df_BFC_dist <- df_events %>%
  filter(Event == "BFC") %>%
  select(1,2,21,22,24,26) # select specified variables

# Proximal Joint Velocities
df_BFC_vel <- df_events %>%
  filter(Event == "BFC") %>%
  select(1,2,10,14,17,19,23) # select specified variables

# COM Velocities
df_BFC_COM <- df_events %>%
  filter(Event == "BFC") %>%
  select(1,2,32,35) # select specified variables

# FRONT-FOOT CONTACT
# Proximal Joint Angles
df_FFC_prox <- df_events %>%
  filter(Event == "FFC") %>%
  select(1,2,9,11,13,16,15,18,20) # select specified variables

# Distal Joint Angles
df_FFC_dist <- df_events %>%
  filter(Event == "FFC") %>%
  select(1,2,21,22,24,26) # select specified variables

# Proximal Joint Velocities
df_FFC_vel_prox <- df_events %>%
  filter(Event == "FFC") %>%
  select(1,2,10,12,14,17,19) # select specified variables

# Distal Joint Velocities
df_FFC_vel_dist <- df_events %>%
  filter(Event == "FFC") %>%
  select(1,2,23,25,27) # select specified variables

# COM Velocities
df_FFC_COM <- df_events %>%
  filter(Event == "FFC") %>%
  select(1,2,32,35) # select specified variables

# MAXIMAL ARM WITHDRAWAL
# Proximal Joint Angles
df_MAW_prox <- df_events %>%
  filter(Event == "MAW") %>%
  select(1,2,11,13,16,15,18,20) # select specified variables

# Distal Joint Angles
df_MAW_dist <- df_events %>%
  filter(Event == "MAW") %>%
  select(1,2,21,22,24,26) # select specified variables

# Proximal Joint Velocities
df_MAW_vel_prox <- df_events %>%
  filter(Event == "MAW") %>%
  select(1,2,12,14,17,19) # select specified variables

# Distal Joint Velocities
df_MAW_vel_dist <- df_events %>%
  filter(Event == "MAW") %>%
  select(1,2,23,25,27) # select specified variables

# COM Velocities
df_MAW_COM <- df_events %>%
  filter(Event == "MAW") %>%
  select(1,2,32,35) # select specified variables

# BALL RELEASE
# Proximal Joint Angles
df_BR_prox <- df_events %>%
  filter(Event == "BR") %>%
  select(1,2,11,13,16,15,18,20) # select specified variables

# Distal Joint Angles
df_BR_dist <- df_events %>%
  filter(Event == "BR") %>%
  select(1,2,21,22,24,26) # select specified variables

# Proximal Joint Velocities
df_BR_vel_prox <- df_events %>%
  filter(Event == "BR") %>%
  select(1,2,12,14,17,19) # select specified variables

# Distal Joint Velocities
df_BR_vel_dist <- df_events %>%
  filter(Event == "BR") %>%
  select(1,2,23,25,27) # select specified variables

# COM Velocities
df_BR_COM <- df_events %>%
  filter(Event == "BR") %>%
  select(1,2,32,35) # select specified variables

# END OF FOLLOW-THROUGH
# Proximal Joint Angles
df_EFT_prox <- df_events %>%
  filter(Event == "EFT") %>%
  select(1,2,11,13,16,15,18,20) # select specified variables

# Distal Joint Angles
df_EFT_dist <- df_events %>%
  filter(Event == "EFT") %>%
  select(1,2,21,24,26) # select specified variables

# COM Velocities
df_EFT_COM <- df_events %>%
  filter(Event == "EFT") %>%
  select(1,2,32,35) # select specified variables

# MAX JOINT KINEMATICS (angles)
# Max angles
df_Max <- df_events %>%
  filter(Event == "Max") %>%
  select(1,2,26) # select specified variables

# Min angles
df_Min <- df_events %>%
  filter(Event == "Min") %>%
  select(1,2,15,24) # select specified variables

# MAX JOINT KINEMATICS (velocities)
df_Max <- cbind(df_Min,df_Max[, 3])

# Max Velocities (proximal)
df_Max_vel_prox <- df_events %>%
  filter(Event == "Max") %>%
  select(1,2,10,12,14,17,19) # select specified variables

# Max  Velocities (distal)
df_Max_vel_dist <- df_events %>%
  filter(Event == "Max") %>%
  select(1,2,23,25) # select specified variables

df_Min_vel_dist <- df_events %>%
  filter(Event == "Min") %>%
  select(1,2,27) # select specified variables

df_Max_vel_dist <- cbind(df_Max_vel_dist,df_Min_vel_dist[, 3]) 

# PHASES
df_phases <- df_additional %>%
  select(1,2,6:10) # select specified variables

# ARM 
df_arm <- df_additional %>%
  select(1,2,4) # select specified variables


#################### Pivot all data frames to longer format ####################

pivot_to_long <- function(df) {
  # Get the original data frame name as a string
  df_name <- deparse(substitute(df))
  
  # Create the new name with '_long' appended
  new_name <- paste0(df_name, "_long")
  
  # Get column names from the third column to the last, to use as levels
  variable_levels <- names(df)[3:ncol(df)]
  
  # Pivot the data frame to long format
  long_df <- df %>%
    pivot_longer(cols = 3:ncol(df), names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(variable, levels = variable_levels))  # Preserve original column order
  
  # Assign the pivoted data frame to a new variable with '_long' added to the original name
  assign(new_name, long_df, envir = .GlobalEnv)
}

# Apply the pivot_to_longer function to all data frames
pivot_to_long(df_stride)
pivot_to_long(df_BFC_prox)
pivot_to_long(df_BFC_dist)
pivot_to_long(df_BFC_vel)
pivot_to_long(df_BFC_COM)
pivot_to_long(df_FFC_prox)
pivot_to_long(df_FFC_dist)
pivot_to_long(df_FFC_vel_prox)
pivot_to_long(df_FFC_vel_dist)
pivot_to_long(df_FFC_COM)
pivot_to_long(df_MAW_prox)
pivot_to_long(df_MAW_dist)
pivot_to_long(df_MAW_vel_prox)
pivot_to_long(df_MAW_vel_dist)
pivot_to_long(df_MAW_COM)
pivot_to_long(df_BR_prox)
pivot_to_long(df_BR_dist)
pivot_to_long(df_BR_vel_prox)
pivot_to_long(df_BR_vel_dist)
pivot_to_long(df_BR_COM)
pivot_to_long(df_EFT_prox)
pivot_to_long(df_EFT_dist)
pivot_to_long(df_EFT_COM)
pivot_to_long(df_Max)
pivot_to_long(df_Max_vel_prox)
pivot_to_long(df_Max_vel_dist)
pivot_to_long(df_phases)
pivot_to_long(df_arm)


############################# Generate Jitter Plots ############################

# Generate plots for each data frame
library(ggplot2)

plot_jitter <- function(data, y_label, filename = NULL, width = width, height = height, dpi = 600) {

# Get the exact number of unique players  
num_players <- length(unique(data$Player))
  
# Define plot parameters
p <- ggplot(data) +
  geom_jitter(aes(x = variable, y = value, group = Player, color = Player),
              shape = 16, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
              size = 1.5, alpha = 0.4) +  # jittered points by Player
  geom_boxplot(aes(x = variable, y = value), fill = NA, width = 0.2, alpha = 0.5, na.rm = TRUE, outlier.shape = NA) +  
  theme_classic() +
  labs(x = NULL,  # remove x-axis label
       y = y_label) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
 # coord_cartesian(ylim = c(y_min, y_max)) # set only the y-axis max limit
 # coord_flip()
  #scale_shape_manual(values = 1:num_players) # apply unique shapes

# Save the plot if a filename is provided
if (!is.null(filename)) {
  ggsave(filename, plot = p, width = width, height = height, dpi = dpi)
  }
}

plot_jitter(df_BFC_COM_long, y_label = expression("Velocity (" * "m.s"^-1 * ")"), y_min = -1, y_max = 3.5, "Centre of Mass at BFC.png", width = 4, height = 4)
plot_jitter(df_FFC_COM_long, y_label = expression("Velocity (" * "m.s"^-1 * ")"), y_min = -1, y_max = 3.5, "Centre of Mass at FFC.png", width = 4, height = 4)
plot_jitter(df_MAW_COM_long, y_label = expression("Velocity (" * "m.s"^-1 * ")"), y_min = -1, y_max = 3.5, "Centre of Mass at MAW.png", width = 4, height = 4)
plot_jitter(df_BR_COM_long, y_label = expression("Velocity (" * "m.s"^-1 * ")"), y_min = -1, y_max = 3.5, "Centre of Mass at BR.png", width = 4, height = 4)
plot_jitter(df_EFT_COM_long, y_label = expression("Velocity (" * "m.s"^-1 * ")"), y_min = -1, y_max = 3.5, "Centre of Mass at EFT.png", width = 4, height = 4)

plot_jitter(df_BFC_prox_long, y_label = expression("Angle (°)"), "Proximal Joint Angles at BFC.png", width = 5.5, height = 4)
plot_jitter(df_BFC_dist_long, y_label = expression(""), "Distal Joint Angles at BFC.png", width = 4, height = 4)
plot_jitter(df_FFC_prox_long, y_label = expression("Angle (°)"), "Proximal Joint Angles at FFC.png", width = 6, height = 4)
plot_jitter(df_FFC_dist_long, y_label = expression(""), "Distal Joint Angles at FFC.png", width = 4, height = 4)
plot_jitter(df_MAW_prox_long, y_label = expression("Angle (°)"), "Proximal Joint Angles at MAW.png", width = 6, height = 4, dpi = 600)
plot_jitter(df_MAW_dist_long, y_label = expression(""), "Distal Joint Angles at MAW.png", width = 4, height = 4, dpi = 600)
plot_jitter(df_BR_prox_long, y_label = expression("Angle (°)"), "Proximal Joint Angles at BR.png", width = 6, height = 4, dpi = 600)
plot_jitter(df_BR_dist_long, y_label = expression(""), "Distal Joint Angles at BR.png", width = 4, height = 4, dpi = 600)
plot_jitter(df_EFT_prox_long, y_label = expression("Angle (°)"), "Proximal Joint Angles at EFT.png", width = 5.5, height = 4, dpi = 600)
plot_jitter(df_EFT_dist_long, y_label = expression(""), "Distal Joint Angles at EFT.png", width = 4, height = 4, dpi = 600)

plot_jitter(df_BFC_vel_long, y_label = expression("Velocity (°/s)"), "Joint Velocities at BFC.png", width = 5.5, height = 4)
plot_jitter(df_FFC_vel_prox_long, y_label = expression("Velocity (°/s)"), "Proximal Joint Velocities at FFC.png", width = 5.5, height = 4)
plot_jitter(df_FFC_vel_dist_long, y_label = expression(""), "Distal Joint Velocities at FFC.png", width = 4, height = 4)
plot_jitter(df_MAW_vel_prox_long, y_label = expression("Velocity (°/s)"), "Proximal Joint Velocities at MAW.png", width = 5.5, height = 4)
plot_jitter(df_MAW_vel_dist_long, y_label = expression(""), "Distal Joint Velocities at MAW.png", width = 4, height = 4)
plot_jitter(df_BR_vel_prox_long, y_label = expression("Velocity (°/s)"), "Proximal Joint Velocities at BR.png", width = 5, height = 4)
plot_jitter(df_BR_vel_dist_long, y_label = expression(""), "Distal Joint Velocities at BR.png", width = 4, height = 4)

plot_jitter(df_Max_long, y_label = expression("Angle (°)"), "Max Joint Angles.png", width = 4, height = 4, dpi = 600)
plot_jitter(df_Max_vel_prox_long, y_label = expression("Velocity (°/s)"), "Max Proximal Joint Velocities.png", width = 6, height = 4, dpi = 600)
plot_jitter(df_Max_vel_dist_long, y_label = expression(""), "Max Distal Joint Velocities.png", width = 3.5, height = 4, dpi = 600)
plot_jitter(df_stride_long, y_label = expression("Stride Characteristics"), "Stride Characteristics.png", width = 4, height = 4, dpi = 600)
plot_jitter(df_arm_long, y_label = expression("Angle (°)"), "Arm Path.png", width = 2, height = 4, dpi = 600)
plot_jitter(df_phases_long, y_label = expression("Duration (ms)"), "Phase Durations.png", width = 6, height = 4, dpi = 600)


##################### Calculate IQRs for Cleaned Event Data ####################

# Define the range of variables to iterate over
variable_range <- 6:ncol(df_events)

# Create an empty data frame to store Q1 and Q3 values
iqr_table <- data.frame(Event = character(), Variable = character(), Q1 = numeric(), Q3 = numeric(), stringsAsFactors = FALSE)

# Get unique events from column #3
events <- unique(df_events[[3]])

# Iterate through each variable and calculate Q1 and Q3 for each event
for (col_index in variable_range) {
  # Get the column name
  col_name <- names(df_events)[col_index]
  
  for (event in events) {
    # Filter the data for the current event
    event_data <- df_events[df_events[[3]] == event, ]
    
    # Calculate Q1 and Q3 for the current variable and event
    Q1 <- quantile(event_data[[col_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(event_data[[col_name]], 0.75, na.rm = TRUE)
    
    # Add Q1 and Q3 to the table
    iqr_table <- rbind(iqr_table, data.frame(Event = event, Variable = col_name, Q1 = Q1, Q3 = Q3))
  }
}

# Print the IQR table to view the results
rownames(iqr_table) <- NULL
print(iqr_table)

IQR <- as.data.frame(iqr_table) # convert table to a data frame

write.csv(iqr_table, "Quartile Values (cleaned event data).csv", row.names = FALSE)


####################### Calculate IQRs for Additional Data #####################

# Define the range of variables to iterate over
variable_range <- 4:10

# Create an empty data frame to store Q1 and Q3 values
iqr_table1 <- data.frame(Event = character(), Variable = character(), Q1 = numeric(), Q3 = numeric(), stringsAsFactors = FALSE)

# Iterate through each variable and calculate Q1 and Q3
for (col_index in variable_range) {
  # Get the column name
  col_name <- names(df_additional)[col_index]
  
  # Calculate Q1 and Q3 for the current variable
  Q1 <- quantile(df_additional[[col_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df_additional[[col_name]], 0.75, na.rm = TRUE)
  
  # Add Q1 and Q3 to the table
  iqr_table1 <- rbind(iqr_table1, data.frame(Variable = col_name, Q1 = Q1, Q3 = Q3))
}

# Print the IQR table to view the results
rownames(iqr_table1) <- NULL
print(iqr_table1)

IQR1 <- as.data.frame(iqr_table1) # convert table to a data frame

write.csv(iqr_table1, "Quartile Values (additional data).csv", row.names = FALSE)


############################## SUMMARISE LONG DATA #############################

# Function to summarize long-format data frames
summarise_data <- function(data, save = FALSE, filename = NULL) {
  # Ensure the data has the required columns 'variable' and 'value'
  if (!all(c("variable", "value") %in% names(data))) {
    stop("Data frame must contain 'variable' and 'value' columns.")
  }
  
  # Summarize the data
  summary_data <- data %>%
    group_by(variable) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE),
              .groups = 'drop')  # Calculate SD instead of SE
  
  # Save the summary data as a CSV file if specified
  if (save) {
    if (is.null(filename)) {
      stop("Please provide a filename to save the summary data.")
    }
    write.csv(summary_data, file = filename, row.names = FALSE)
  }
  
  return(summary_data)
}

# Call the summary_data function for specified data frames
summarise_data(df_stride_long, save = TRUE, filename = "Stride Summary.csv")
summarise_data(df_BFC_long, save = TRUE, filename = "BFC Summary.csv")
summarise_data(df_BFC_vel_long, save = TRUE, filename = "BFC Velocity Summary.csv")
summarise_data(df_BFC_COM_long, save = TRUE, filename = "BFC COM Summary.csv")
summarise_data(df_FFC_long, save = TRUE, filename = "FFC Summary.csv")
summarise_data(df_FFC_vel_long, save = TRUE, filename = "FFC Velocity Summary.csv")
summarise_data(df_FFC_COM_long, save = TRUE, filename = "FFC COM Summary.csv")
summarise_data(df_MAW_long, save = TRUE, filename = "MAW Summary.csv")
summarise_data(df_MAW_vel_long, save = TRUE, filename = "MAW Velocity Summary.csv")
summarise_data(df_MAW_COM_long, save = TRUE, filename = "MAW COM Summary.csv")
summarise_data(df_BR_long, save = TRUE, filename = "BR Summary.csv")
summarise_data(df_BR_vel_long, save = TRUE, filename = "BR Velocity Summary.csv")
summarise_data(df_BR_COM_long, save = TRUE, filename = "BR COM Summary.csv")
summarise_data(df_EFT_long, save = TRUE, filename = "EFT Summary.csv")
summarise_data(df_EFT_COM_long, save = TRUE, filename = "EFT COM Summary.csv")
summarise_data(df_arm_long, save = TRUE, filename = "Arm Path Summary.csv")
summarise_data(df_Max_long, save = TRUE, filename = "Max Summary.csv")
summarise_data(df_Max_vel_long, save = TRUE, filename = "Max Velocity Summary.csv")
summarise_data(df_phases_long, save = TRUE, filename = "Phase Summary.csv")
