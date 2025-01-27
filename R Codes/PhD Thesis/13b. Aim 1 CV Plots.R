# This code will plot Coefficient of Variation data and boxplots/Cleveland dot plots for Aim 1

# Load the Coefficient of Variation Data from Aim 1 Data folder
file_path <- file.choose()

library(readr)
data <- read.csv(file_path)
colnames(data)[colnames(data) == "X"] <- "Variable"

# Split the data into each event
library(dplyr)
library(ggplot2)
library(tidyverse)

BFC <- data %>%
  select(1:2) %>% # select event column
  slice(-1:-4, -13, -15,-28,-29,-31,-32) %>% # remove Duration % row
  arrange(across(2)) %>% # sort from smallest to largest
  mutate (BFC = BFC*100) # convert to whole %

BFC <- BFC %>%
  mutate(Variable = factor(Variable, levels = rev(Variable[order(BFC)])))

BFC_plot <- ggplot(BFC, aes(x=Variable, y=BFC)) +
  geom_segment(aes(x=Variable, xend=Variable, y=0, yend=BFC), color = "grey", linetype = 6) +
  geom_point(size = 3, color = "#E87722") +
  coord_flip() +
  theme_light() +
  labs(title = "Back-Foot Contact",
       x = NULL,
       y = NULL) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 7),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 9),  # y-axis title text size
        plot.title = element_text(hjust = 0.3)) +
  ylab("Coefficient of Variation (%)")

# Save the plot as a PNG file
ggsave(filename = "Coefficient - BFC.png", plot = BFC_plot, dpi = 600, height = 4, width = 6)
  

FFC <- data %>%
  select(1,3) %>% # select event column
  slice(-1:-3,-29,-31,-32) %>% # remove Duration % row
  arrange(across(2)) %>% # sort from smallest to largest
  mutate(FFC = FFC*100) # convert to whole %

FFC <- FFC %>%
  mutate(Variable = factor(Variable, levels = rev(Variable[order(FFC)])))

FFC_plot <- ggplot(FFC, aes(x=Variable, y=FFC)) +
  geom_segment(aes(x=Variable, xend=Variable, y=0, yend=FFC), color = "grey", linetype = 6) +
  geom_point(size = 3, color = "#E87722") +
  coord_flip() +
  theme_light() +
  labs(title = "Front-Foot Contact",
       x = NULL,
       y = NULL) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 7),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 9),  # y-axis title text size
        plot.title = element_text(hjust = 0.3)) +
  ylab("Coefficient of Variation (%)")

# Save the plot as a PNG file
ggsave(filename = "Coefficient - FFC.png", plot = FFC_plot, dpi = 600, height = 4, width = 6)


MAW <- data %>%
  select(1,4) %>% # select event column
  slice(-1:-6,-29,-31,-32) %>% # remove Duration % row
  arrange(across(2)) %>% # sort from smallest to largest
  mutate(MAW = MAW*100) # convert to whole %

MAW <- MAW %>%
  mutate(Variable = factor(Variable, levels = rev(Variable[order(MAW)])))

MAW_plot <- ggplot(MAW, aes(x=Variable, y=MAW)) +
  geom_segment(aes(x=Variable, xend=Variable, y=0, yend=MAW), color = "grey", linetype = 6) +
  geom_point(size = 3, color = "#E87722") +
  coord_flip() +
  theme_light() +
  labs(title = "Maximal Arm Withdrawal",
       x = NULL,
       y = NULL) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 7),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 9),  # y-axis title text size
        plot.title = element_text(hjust = 0.3)) +
  ylab("Coefficient of Variation (%)")

# Save the plot as a PNG file
ggsave(filename = "Coefficient - MAW.png", plot = MAW_plot, dpi = 600, height = 4, width = 6)


BR <- data %>%
  select(1,5) %>% # select event column
  slice(-1:-8,-12,-14,-29,-31,-32) %>% # remove Duration % row
  arrange(across(2)) %>% # sort from smallest to largest
  mutate(BR = BR*100) # convert to whole %

BR <- BR %>%
  mutate(Variable = factor(Variable, levels = rev(Variable[order(BR)])))

BR_plot <- ggplot(BR, aes(x=Variable, y=BR)) +
  geom_segment(aes(x=Variable, xend=Variable, y=0, yend=BR), color = "grey", linetype = 6) +
  geom_point(size = 3, color = "#E87722") +
  coord_flip() +
  theme_light() +
  labs(title = "Ball Release",
       x = NULL,
       y = NULL) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 7),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 9),  # y-axis title text size
        plot.title = element_text(hjust = 0.3)) +
  ylab("Coefficient of Variation (%)")

# Save the plot as a PNG file
ggsave(filename = "Coefficient - BR.png", plot = BR_plot, dpi = 600, height = 4, width = 6)


EFT <- data %>%
  select(1,6) %>% # select event column
  slice(-1:-8,-21,-23,-29,-31:-33) %>% # remove Duration % row
  arrange(across(2)) %>% # sort from smallest to largest
  mutate(EFT = EFT*100) # convert to whole %

EFT <- EFT %>%
  mutate(Variable = factor(Variable, levels = rev(Variable[order(EFT)])))

EFT_plot <- ggplot(EFT, aes(x=Variable, y=EFT)) +
  geom_segment(aes(x=Variable, xend=Variable, y=0, yend=EFT), color = "grey", linetype = 6) +
  geom_point(size = 3, color = "#E87722") +
  coord_flip() +
  theme_light() +
  labs(title = "End of Follow-Through",
       x = NULL,
       y = NULL) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 7),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 9),  # y-axis title text size
        plot.title = element_text(hjust = 0.3)) +
  ylab("Coefficient of Variation (%)")

# Save the plot as a PNG file
ggsave(filename = "Coefficient - EFT.png", plot = EFT_plot, dpi = 600, height = 4, width = 6)


# Load the "Cleaned Event Data (combined)" file
file_path <- file.choose()
df <- read_csv(file_path)
df <- df %>%
  mutate(`Stride Length (% Height)` = `Stride Length (% Height)` *100)

# Specify the variables to be plotted below
variable_range <- 4:30

BFC <- df %>%
  filter(Event =="BFC")

BFC_min <- BFC %>%
  summarise(across(where(is.numeric), min, na.rm = TRUE))
BFC_max <- BFC %>%
  summarise(across(where(is.numeric), max, na.rm = TRUE))
BFC_range <- rbind(BFC_min, BFC_max)
row.names(BFC_range) <- c("min", "max")

write.csv(BFC_range, file = "Variable Ranges at BFC.csv", row.names = TRUE)

FFC <- df %>%
  filter(Event =="FFC")

FFC_min <- FFC %>%
  summarise(across(where(is.numeric), min, na.rm = TRUE))
FFC_max <- FFC %>%
  summarise(across(where(is.numeric), max, na.rm = TRUE))
FFC_range <- rbind(FFC_min, FFC_max)
row.names(FFC_range) <- c("min", "max")

write.csv(FFC_range, file = "Variable Ranges at FFC.csv", row.names = TRUE)

MAW <- df %>%
  filter(Event =="MAW")

MAW_min <- MAW %>%
  summarise(across(where(is.numeric), min, na.rm = TRUE))
MAW_max <- MAW %>%
  summarise(across(where(is.numeric), max, na.rm = TRUE))
MAW_range <- rbind(MAW_min, MAW_max)
row.names(MAW_range) <- c("min", "max")

write.csv(MAW_range, file = "Variable Ranges at MAW.csv", row.names = TRUE)

BR <- df %>%
  filter(Event =="BR")

BR_min <- BR %>%
  summarise(across(where(is.numeric), min, na.rm = TRUE))
BR_max <- BR %>%
  summarise(across(where(is.numeric), max, na.rm = TRUE))
BR_range <- rbind(BR_min, BR_max)
row.names(BR_range) <- c("min", "max")

write.csv(BR_range, file = "Variable Ranges at BR.csv", row.names = TRUE)

EFT <- df %>%
  filter(Event =="EFT")

EFT_min <- EFT %>%
  summarise(across(where(is.numeric), min, na.rm = TRUE))
EFT_max <- EFT %>%
  summarise(across(where(is.numeric), max, na.rm = TRUE))
EFT_range <- rbind(EFT_min, EFT_max)
row.names(EFT_range) <- c("min", "max")

write.csv(EFT_range, file = "Variable Ranges at EFT.csv", row.names = TRUE)

Min <- df %>%
  filter(Event =="Min")

Min_min <- Min %>%
  summarise(across(where(is.numeric), min, na.rm = TRUE))
Min_max <- Min %>%
  summarise(across(where(is.numeric), max, na.rm = TRUE))
Min_range <- rbind(Min_min, Min_max)
row.names(Min_range) <- c("min", "max")

write.csv(Min_range, file = "Variable Ranges (mins).csv", row.names = TRUE)

Max <- df %>%
  filter(Event =="Max")

Max_min <- Max %>%
  summarise(across(where(is.numeric), min, na.rm = TRUE))
Max_max <- Max %>%
  summarise(across(where(is.numeric), max, na.rm = TRUE))
Max_range <- rbind(Max_min, Max_max)
row.names(Max_range) <- c("min", "max")

write.csv(Max_range, file = "Variable Ranges (max).csv", row.names = TRUE)


####################################BOXPLOTS####################################

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Create an empty list to store the plots
boxplots <- list()

# Define the range of variables you want to iterate over (e.g., all columns except Event)
variable_range <- 4:ncol(df)  # Assuming Event is in column 2

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
    Q1 <- quantile(event_data[[col_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(event_data[[col_name]], 0.75, na.rm = TRUE)
    
    # Add Q1 and Q3 to the table
    iqr_table <- rbind(iqr_table, data.frame(Event = event, Variable = col_name, Q1 = Q1, Q3 = Q3))
    
    # Create the boxplot for the current variable and event
    plot <- ggplot(event_data, aes(x = "", y = .data[[col_name]])) +
      geom_boxplot(alpha = 1, width = 0.2, outlier.shape = NA, 
                   color = "#0C2340", fill = "#E87722", linewidth = 1) +
      labs(y = col_name, title = paste(col_name, "at", event)) +
      coord_flip() + 
      theme_minimal() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5),
            axis.title = element_blank())
    
    # Save the plot in the list using a unique name
    boxplots[[paste(col_name, event, sep = "_")]] <- plot
    
    # Create a file name based on variable and event
    file_name <- paste0(col_name, " at ", event, ".png")
    
    # Save the plot as a PNG file
    ggsave(filename = file_name, plot = plot, width = 4, height = 2, dpi = 600, device = "png")
    
  }
}

# Print the IQR table to view the results
print(iqr_table)

write.csv(iqr_table, "Quartile Values for Boxplots.csv", row.names = FALSE)

###############################CLEVELAND DOT PLOTS###############################

# BACK-FOOT CONTACT

df_BFC <- read_csv("Auburn/Sports Medicine & Movement Lab/PhD Thesis/2. Chapters/Chapter 4 - Aim 1/Data/Cleveland Dot Plot Data/Variable Ranges at BFC.csv")
df_BFC_vel <- df_BFC %>%
  slice(2,4,7,9,13,15) %>% # separate joint velocities
  mutate(Variable = factor(Variable, levels = c("Back Knee Extension Velocity", "Pelvis Rotation Velocity", 
                                                "Trunk Rotation Velocity", "Trunk Flexion Velocity", 
                                                "Shoulder H Abduction Velocity", "Shoulder Rotation Velocity")))

df_BFC_COM <- df_BFC %>%
  slice(16:17) %>% # separate COM variables
  mutate(Variable = factor(Variable, levels = c("COM Velocity (X)", "COM Velocity (Z)")))

df_BFC <- df_BFC %>%
  slice(-2,-4,-7,-9,-13,-15:-17) %>% # separate velocity & COM variables
  mutate(Variable = factor(Variable, levels = c("Back Knee Flexion", "Pelvis Rotation (Y)", "Trunk Rotation (Y)", 
                                                "Pelvis-Trunk Separation", "Trunk Flexion", "Lateral Trunk Flexion",
                                                "Shoulder Abduction", "Shoulder H Abduction", "Shoulder Rotation")))


# FRONT-FOOT CONTACT

df_FFC <- read_csv("Auburn/Sports Medicine & Movement Lab/PhD Thesis/2. Chapters/Chapter 4 - Aim 1/Data/Cleveland Dot Plot Data/Variable Ranges at FFC.csv")
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
  slice(23:24) %>% # separate COM variables
  mutate(Variable = factor(Variable, levels = c("COM Velocity (X)", "COM Velocity (Z)")))

df_FFC <- df_FFC %>%
  slice(-1:-3,-5,-7,-9,-12,-14,-18,-20,-22:-25) %>% # separate velocity & COM variables
  mutate(Variable = factor(Variable, levels = c("Back Knee Flexion", "Front Knee Flexion", "Pelvis Rotation (Y)",
                                                "Trunk Rotation (Y)", "Pelvis-Trunk Separation", "Trunk Flexion", 
                                                "Lateral Trunk Flexion", "Shoulder Abduction", "Shoulder H Abduction",
                                                "Shoulder Rotation", "Elbow Flexion")))

# MAXIMAL ARM WITHDRAWAL

df_MAW <- read_csv("Auburn/Sports Medicine & Movement Lab/PhD Thesis/2. Chapters/Chapter 4 - Aim 1/Data/Cleveland Dot Plot Data/Variable Ranges at MAW.csv")
df_MAW_vel <- df_MAW %>%
  slice(2,4,7,9,13,15,17) %>% # separate joint velocities
  mutate(Variable = factor(Variable, levels = c("Front Knee Extension Velocity", "Pelvis Rotation Velocity", 
                                                "Trunk Rotation Velocity", "Trunk Flexion Velocity", 
                                                "Shoulder H Abduction Velocity", "Shoulder Rotation Velocity", 
                                                "Elbow Extension Velocity")))

df_MAW_COM <- df_MAW %>%
  slice(21,22) %>% # separate COM variables
  mutate(Variable = factor(Variable, levels = c("COM Velocity (X)", "COM Velocity (Z)")))

df_MAW_hand <- df_MAW %>%
  slice(18:20) %>% # separate hand variables
  mutate(Variable = factor(Variable, levels = c("Hand COM (X)", "Hand COM (Y)", "Hand COM (Z)")))

df_MAW <- df_MAW %>%
  slice(-2,-4,-7,-9,-13,-15,-17:-22) %>% # separate velocity & COM variables
  mutate(Variable = factor(Variable, levels = c("Front Knee Flexion", "Pelvis Rotation (Y)", "Trunk Rotation (Y)",
                                                "Pelvis-Trunk Separation", "Trunk Flexion", "Lateral Trunk Flexion",
                                                "Shoulder Abduction", "Shoulder H Abduction", "Shoulder Rotation", 
                                                "Elbow Flexion")))

# BALL RELEASE

df_BR <- read_csv("Auburn/Sports Medicine & Movement Lab/PhD Thesis/2. Chapters/Chapter 4 - Aim 1/Data/Cleveland Dot Plot Data/Variable Ranges at BR.csv")
df_BR_vel <- df_BR %>%
  slice(2,4,7,9,13,15,17) %>% # separate joint velocities
  mutate(Variable = factor(Variable, levels = c("Front Knee Extension Velocity", "Pelvis Rotation Velocity",
                                                "Trunk Rotation Velocity", "Trunk Flexion Velocity", 
                                                "Shoulder H Abduction Velocity", "Shoulder Rotation Velocity", 
                                                "Elbow Extension Velocity")))

df_BR_COM <- df_BR %>%
  slice(21:22) %>% # separate COM variables
  mutate(Variable = factor(Variable, levels = c("COM (X)", "COM (Y)", "COM (Z)", 
                                                "COM Velocity (X)", "COM Velocity (Z)")))

df_BR_hand <- df_BR %>%
  slice(18:20) %>% # separate hand variables
  mutate(Variable = factor(Variable, levels = c("Hand COM (X)", "Hand COM (Y)", "Hand COM (Z)")))


df_BR <- df_BR %>%
  slice(-2,-4,-7,-9,-13,-15,-17:-22) %>% # separate velocity & COM variables
  mutate(Variable = factor(Variable, levels = c("Front Knee Flexion", "Pelvis Rotation (Y)","Trunk Rotation (Y)", 
                                                "Pelvis-Trunk Separation", "Trunk Flexion", "Lateral Trunk Flexion",
                                                "Shoulder Abduction", "Shoulder H Abduction", "Shoulder Rotation",
                                                "Elbow Flexion")))

# END OF FOLLOW-THROUGH

df_EFT <- read_csv("Auburn/Sports Medicine & Movement Lab/PhD Thesis/2. Chapters/Chapter 4 - Aim 1/Data/Cleveland Dot Plot Data/Variable Ranges at EFT.csv")

df_EFT_COM <- df_EFT %>%
  slice(10:11) %>% # separate COM variables
  mutate(Variable = factor(Variable, levels = c("COM Velocity (X)", "COM Velocity (Z)")))

df_EFT <- df_EFT %>%
  slice(-10:-11) %>% # separate velocity & COM variables
  mutate(Variable = factor(Variable, levels = c("Front Knee Flexion", "Pelvis Rotation (Y)","Trunk Rotation (Y)", 
                                                "Pelvis-Trunk Separation", "Trunk Flexion", "Lateral Trunk Flexion",
                                                "Shoulder Abduction", "Shoulder Rotation", "Elbow Flexion")))

# Create a plotting function

plot_symmetric_ylim <- function(data, title, filename = NULL, width = 6, height = 4, dpi = 600) {
  # Calculate the maximum absolute y-value across min and max columns
  max_y <- max(abs(data$min), abs(data$max))
  
  # Generate the plot with symmetric y-axis limits
  p <- ggplot(data) +
    geom_segment(aes(x = Variable, xend = Variable, y = min, yend = max), color = "grey") +
    geom_point(aes(x = Variable, y = min), color = "#0C2340", size = 3, alpha = 0.75) +
    geom_point(aes(x = Variable, y = max), color = "#E87722", size = 3, alpha = 0.75) +
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
plot_symmetric_ylim(df_MAW_hand, "Hand Centre of Mass Velocity at MAW", "Hand Centre of Mass at MAW.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_BR, "Joint Angles at BR", "Joint Angles at BR.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_BR_vel, "Joint Velocities at BR", "Joint Velocities at BR.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_BR_COM, "Centre of Mass Velocity at BR", "Centre of Mass at BR.png", width = 6, height = 2, dpi = 600)
plot_symmetric_ylim(df_BR_hand, "Hand Centre of Mass Velocity at BR", "Hand Centre of Mass at BR.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_EFT, "Joint Angles at EFT", "Joint Angles at EFT.png", width = 6, height = 4, dpi = 600)
plot_symmetric_ylim(df_EFT_COM, "Centre of Mass Velocity at EFT", "Centre of Mass at EFT.png", width = 6, height = 2, dpi = 600)
