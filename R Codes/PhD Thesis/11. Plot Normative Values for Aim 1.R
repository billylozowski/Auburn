# Calculate the 90th Percentile for each variable (upper and lower bounds for 90% of the data)
# Plot this data as Cleveland Dot Plots

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# Load "Cleaned Event Data (combined)
file_path <- file.choose()
df <- read_csv(file_path)
df <- df %>%
  mutate(`Stride Length (% Height)` = `Stride Length (% Height)` *100)

# Define the range of variables you want to iterate over (e.g., all columns except Event)
variable_range <- 6:ncol(df)

# Create an empty data frame to store Q1 and Q3 values
iqr_table <- data.frame(Event = character(), Variable = character(), Q1 = numeric(), Q3 = numeric(), stringsAsFactors = FALSE)

# Get unique events from the Event column
events <- unique(df[[2]])

# Iterate through each variable and create boxplots for each event
for (col_index in variable_range) {
  # Get the column name
  col_name <- names(df)[col_index]
  
  for (event in events) {
    # Filter the data for the current event
    event_data <- df[df[[2]] == event, ]
    
    # Calculate Q1 and Q3 for the current variable and event
    Q1 <- quantile(event_data[[col_name]], 0.05, na.rm = TRUE)
    Q3 <- quantile(event_data[[col_name]], 0.95, na.rm = TRUE)
    
    # Add Q1 and Q3 to the table
    iqr_table <- rbind(iqr_table, data.frame(Event = event, Variable = col_name, Q1 = Q1, Q3 = Q3))
  }
}

# Print the IQR table to view the results
rownames(iqr_table) <- NULL
print(iqr_table)

IQR <- as.data.frame(iqr_table) # convert table to a data frame

write.csv(iqr_table, "Quartile Values for Dot Plots (90th percentile).csv")

###############################CLEVELAND DOT PLOTS###############################

# BACK-FOOT CONTACT
df_BFC <- IQR %>%
  filter(Event == "BFC") %>%
  select(-1) %>% # remove the Events column
  slice(-1:-3)

df_BFC_vel <- df_BFC %>%
  slice(2,6,9,11,15) %>% # separate joint velocities
  mutate(Variable = factor(Variable, levels = c("Back Knee Extension Velocity", "Pelvis Rotation Velocity", 
                                                "Trunk Rotation Velocity", "Trunk Flexion Velocity", 
                                                "Shoulder H Abduction Velocity")))
df_BFC_COM <- df_BFC %>%
  slice(24,27) %>% # separate COM variables
  mutate(Variable = factor(Variable, levels = c("COM Velocity (X)", "COM Velocity (Z)")))

df_BFC <- df_BFC %>%
  slice(1,5,7,8,10,12:14,16,18) %>% # separate velocity & COM variables
  mutate(Variable = factor(Variable, levels = c("Back Knee Flexion", "Pelvis Rotation (Y)", "Trunk Rotation (Y)", 
                                                "Pelvis-Trunk Separation", "Trunk Flexion", "Lateral Trunk Flexion",
                                                "Shoulder Abduction", "Shoulder H Abduction", "Shoulder Rotation",
                                                "Elbow Flexion")))

# FRONT-FOOT CONTACT
df_FFC <- IQR %>%
  filter(Event == "FFC") %>%
  select(-1) # remove the Events column

df_FFC_stride <- df_FFC %>%
  slice(1:3) %>% # separate stride parameters
  mutate(Variable = factor(Variable, levels = c("Stride Length (% Height)", "Stride Angle", "Front Foot Angle")))

df_FFC_vel <- df_FFC %>%
  slice(5,7,9,12,14,18,20,22) %>% # separate joint velocities
  mutate(Variable = factor(Variable, levels = c("Back Knee Extension Velocity","Front Knee Extension Velocity", 
                                                "Pelvis Rotation Velocity", "Trunk Rotation Velocity", 
                                                "Trunk Flexion Velocity", "Shoulder H Abduction Velocity", 
                                                "Shoulder Rotation Velocity", "Elbow Extension Velocity")))
df_FFC_COM <- df_FFC %>%
  slice(27,30) %>% # separate COM variables
  mutate(Variable = factor(Variable, levels = c("COM Velocity (X)", "COM Velocity (Z)")))

df_FFC <- df_FFC %>%
  slice(4,6,8,10,11,13,15:17,19,21) %>% # separate velocity & COM variables
  mutate(Variable = factor(Variable, levels = c("Back Knee Flexion", "Front Knee Flexion", "Pelvis Rotation (Y)",
                                                "Trunk Rotation (Y)", "Pelvis-Trunk Separation", "Trunk Flexion", 
                                                "Lateral Trunk Flexion", "Shoulder Abduction", "Shoulder H Abduction",
                                                "Shoulder Rotation", "Elbow Flexion")))

# MAXIMAL ARM WITHDRAWAL
df_MAW <- IQR %>%
  filter(Event == "MAW") %>%
  select(-1) # remove the Events column

df_MAW_vel <- df_MAW %>%
  slice(7,9,12,14,18,20,22) %>% # separate joint velocities
  mutate(Variable = factor(Variable, levels = c("Front Knee Extension Velocity", "Pelvis Rotation Velocity", 
                                                "Trunk Rotation Velocity", "Trunk Flexion Velocity", 
                                                "Shoulder H Abduction Velocity", "Shoulder Rotation Velocity", 
                                                "Elbow Extension Velocity")))
df_MAW_COM <- df_MAW %>%
  slice(27,30) %>% # separate COM variables
  mutate(Variable = factor(Variable, levels = c("COM Velocity (X)", "COM Velocity (Z)")))

df_MAW_hand <- df_MAW %>%
  slice(23:25) %>% # separate hand variables
  mutate(Variable = factor(Variable, levels = c("Hand COM (X)", "Hand COM (Y)", "Hand COM (Z)")))

df_MAW <- df_MAW %>%
  slice(6,8,10,11,13,15:17,19,21) %>% # separate velocity & COM variables
  mutate(Variable = factor(Variable, levels = c("Front Knee Flexion", "Pelvis Rotation (Y)", "Trunk Rotation (Y)",
                                                "Pelvis-Trunk Separation", "Trunk Flexion", "Lateral Trunk Flexion",
                                                "Shoulder Abduction", "Shoulder H Abduction", "Shoulder Rotation", 
                                                "Elbow Flexion")))

# BALL RELEASE
df_BR <- IQR %>%
  filter(Event == "BR") %>%
  select(-1) # remove the Events column

df_BR_vel <- df_BR %>%
  slice(7,9,12,14,18,20,22) %>% # separate joint velocities
  mutate(Variable = factor(Variable, levels = c("Front Knee Extension Velocity", "Pelvis Rotation Velocity",
                                                "Trunk Rotation Velocity", "Trunk Flexion Velocity", 
                                                "Shoulder H Abduction Velocity", "Shoulder Rotation Velocity", 
                                                "Elbow Extension Velocity")))
df_BR_COM <- df_BR %>%
  slice(27,30) %>% # separate COM variables
  mutate(Variable = factor(Variable, levels = c("COM Velocity (X)", "COM Velocity (Z)")))

df_BR_hand <- df_BR %>%
  slice(23:25) %>% # separate hand variables
  mutate(Variable = factor(Variable, levels = c("Hand COM (X)", "Hand COM (Y)", "Hand COM (Z)")))

df_BR <- df_BR %>%
  slice(6,8,10,11,13,15:17,19,21) %>% # separate velocity & COM variables
  mutate(Variable = factor(Variable, levels = c("Front Knee Flexion", "Pelvis Rotation (Y)","Trunk Rotation (Y)", 
                                                "Pelvis-Trunk Separation", "Trunk Flexion", "Lateral Trunk Flexion",
                                                "Shoulder Abduction", "Shoulder H Abduction", "Shoulder Rotation",
                                                "Elbow Flexion")))

# END OF FOLLOW-THROUGH
df_EFT <- IQR %>%
  filter(Event == "EFT") %>%
  select(-1) # remove the Events column

df_EFT_COM <- df_EFT %>%
  slice(27,30) %>% # separate COM variables
  mutate(Variable = factor(Variable, levels = c("COM Velocity (X)", "COM Velocity (Z)")))

df_EFT <- df_EFT %>%
  slice(6,8,10,11,13,15,16,19,21) %>% # separate velocity & COM variables
  mutate(Variable = factor(Variable, levels = c("Front Knee Flexion", "Pelvis Rotation (Y)","Trunk Rotation (Y)", 
                                                "Pelvis-Trunk Separation", "Trunk Flexion", "Lateral Trunk Flexion",
                                                "Shoulder Abduction", "Shoulder Rotation", "Elbow Flexion")))

# MAX VALUES (rotation velocities, flexion/extension/rotation angles)
df_Max <- IQR %>%
  filter(Event == "Max") %>%
  select(-1) %>%
  slice(13,21) # select joint angles where max values are of interest

df_Min <- IQR %>%
  filter(Event == "Min") %>%
  select(-1) %>%
  slice(10,15,19) # select joint angles where min values are of interest

df_Max_vel <- IQR %>%
  filter(Event == "Max") %>%
  select(-1) %>%
  slice(5,7,9,12,14,18,20) # select joint velocities where max values are of interest

df_Min_vel <- IQR %>%
  filter(Event == "Min") %>%
  select(-1) %>%
  slice(22) # select joint velocities where min values are of interest

df_Max_COM <- IQR %>%
  filter(Event == "Max") %>%
  select(-1) %>%
  slice(27) %>%
  mutate(Variable = factor(Variable, levels = c("COM Velocity (X)")))

df_Max_vel <- rbind(df_Max_vel,df_Min_vel) %>% # combine min/max velocity values
  mutate(Variable = factor(Variable, levels = c("Back Knee Extension Velocity", "Front Knee Extension Velocity",
                                                "Pelvis Rotation Velocity", "Trunk Rotation Velocity",
                                                "Trunk Flexion Velocity", "Shoulder H Abduction Velocity",
                                                "Shoulder Rotation Velocity", "Elbow Extension Velocity")))

df_Max <- rbind(df_Max,df_Min) %>% # combine min/max values
  slice(3,1,4,5,2) %>%
  mutate(Variable = factor(Variable, levels = c("Pelvis-Trunk Separation", "Trunk Flexion", "Lateral Trunk Flexion", 
                                                "Shoulder Rotation", "Elbow Flexion")))

########################## Create a plotting function ##########################

plot_symmetric_ylim <- function(data, title, filename = NULL, width = 6, height = 4, dpi = 600) {
  # Calculate the maximum absolute y-value across min and max columns
  max_y <- max(abs(data$Q1), abs(data$Q3))
  
  # Generate the plot with symmetric y-axis limits
  p <- ggplot(data) +
    geom_segment(aes(x = Variable, xend = Variable, y = Q1, yend = Q3), color = "grey") +
    geom_point(aes(x = Variable, y = Q1), color = "#0C2340", size = 3, alpha = 0.75) +
    geom_point(aes(x = Variable, y = Q3), color = "#E87722", size = 3, alpha = 0.75) +
    coord_flip() +
    ylim(-max_y, max_y) +  # Set equal limits around 0
    geom_hline(yintercept = 0, color = "black", alpha = 0.75, linetype = 2) +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text( hjust = 0.5, size = 16)) +
    labs(title = title, x = NULL, y = NULL)
  
  # Save the plot if a filename is provided
  if (!is.null(filename)) {
    ggsave(filename, plot = p, width = width, height = height)  # Use specified dimensions
  }
}

# Use the function for any data frame
plot_symmetric_ylim(df_BFC, "Joint Angles at BFC", "Joint Angles at BFC.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_BFC_vel, "Joint Velocities at BFC", "Joint Velocities at BFC.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_BFC_COM, "Centre of Mass Velocity at BFC", "Centre of Mass at BFC.png", width = 6, height = 2, dpi = 600)
plot_symmetric_ylim(df_FFC, "Joint Angles at FFC", "Joint Angles at FFC.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_FFC_vel, "Joint Velocities at FFC", "Joint Velocities at FFC.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_FFC_COM, "Centre of Mass Velocity at FFC", "Centre of Mass at FFC.png", width = 6, height = 2, dpi = 600)
plot_symmetric_ylim(df_MAW, "Joint Angles at MAW", "Joint Angles at MAW.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_MAW_vel, "Joint Velocities at MAW", "Joint Velocities at MAW.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_MAW_COM, "Centre of Mass Velocity at MAW", "Centre of Mass at MAW.png", width = 6, height = 2, dpi = 600)
plot_symmetric_ylim(df_MAW_hand, "Hand Centre of Mass Position at MAW", "Hand Centre of Mass at MAW.png", width = 6, height = 2, dpi = 600)
plot_symmetric_ylim(df_BR, "Joint Angles at BR", "Joint Angles at BR.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_BR_vel, "Joint Velocities at BR", "Joint Velocities at BR.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_BR_COM, "Centre of Mass Velocity at BR", "Centre of Mass at BR.png", width = 6, height = 2, dpi = 600)
plot_symmetric_ylim(df_BR_hand, "Hand Centre of Mass Position at BR", "Hand Centre of Mass at BR.png", width = 6, height = 2, dpi = 600)
plot_symmetric_ylim(df_EFT, "Joint Angles at EFT", "Joint Angles at EFT.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_EFT_COM, "Centre of Mass Velocity at EFT", "Centre of Mass at EFT.png", width = 6, height = 2, dpi = 600)
plot_symmetric_ylim(df_Max, "Max Joint Angles", "Max Joint Angles.png", width = 8, height = 3, dpi = 600)
plot_symmetric_ylim(df_Max_vel, "Max Joint Velocities", "Max Joint Velocities.png", width = 8, height = 4, dpi = 600)

# Plot and save COM Velocity (X)
max_y <- max(abs(df_Max_COM$Q1), abs(df_Max_COM$Q3))

COM <- ggplot(df_Max_COM) +
  geom_segment(aes(x = Variable, xend = Variable, y = Q1, yend = Q3), color = "grey") +
  geom_point(aes(x = Variable, y = Q1), color = "#0C2340", size = 3, alpha = 0.75) +
  geom_point(aes(x = Variable, y = Q3), color = "#E87722", size = 3, alpha = 0.75) +
  coord_flip() +
  ylim(-max_y, max_y) +  # Set equal limits around 0
  geom_hline(yintercept = 0, color = "black", alpha = 0.75, linetype = 2) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text( hjust = 0.5, size = 16),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Max COM Velocity (X)", x = NULL, y = "m/s")

# Save the COM Velocity plot
ggsave(COM, file = "Max COM Velocity (X).png", width = 6, height = 1.5, dpi = 600)
