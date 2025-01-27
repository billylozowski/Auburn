
# Load "Cleaned Event Data (combined)"
file_path <- file.choose()

library(readr)
data <- read_csv(file_path)

# Convert a range of columns to numeric
data[, 3:ncol(data)] <- lapply(data[, 3:ncol(data)], as.numeric)

# Select variables to plot
library(dplyr)
stride_data <- data %>%
  select(`File Name`, Event, `Stride Length (% Height)`, `Stride Angle`, `Front Foot Angle`) %>%
  filter(Event == "FFC")

# Convert 'Stride Length (% Height)' to non-decimal percentage
stride_data$`Stride Length (% Height)` <- stride_data$`Stride Length (% Height)` *100

# Create a new column called Player
stride_data$Player <- substr(stride_data$`File Name`, 1,8)

# Reorder stride_data
stride_data <- stride_data %>%
  select(`File Name`, Player, everything()) %>%
  select(-3)

# Rename the column to include the degrees symbol
colnames(stride_data)[colnames(stride_data) == "Stride Angle"] <- "Stride Angle (°)"
colnames(stride_data)[colnames(stride_data) == "Front Foot Angle"] <- "Front Foot Angle (°)"

################################################################################

# Load "Additional Data (combined)"
file_path <- file.choose()

library(readr)
additional <- read_csv(file_path)

# Convert a range of columns to numeric
additional[, 3:ncol(data)] <- lapply(data[, 3:ncol(data)], as.numeric)

# Select columns of interest
additional <- additional %>%
  select(1,3,5:9)

# Create a new column called Player
additional$Player <- substr(additional$`File Name`, 1,8)

# Reorder additional
additional <- additional %>%
  select(`File Name`, Player, everything())

# Create arm_path data frame
arm_path <- additional %>%
  select(1:3)

phase_data <- additional %>%
  select(1:2, 4:8)

# Rename the column to remove "(ms)" from the end
colnames(phase_data)[colnames(phase_data) == "Stride (ms)"] <- "Stride"
colnames(phase_data)[colnames(phase_data) == "Transition (ms)"] <- "Transition"
colnames(phase_data)[colnames(phase_data) == "Acceleration (ms)"] <- "Acceleration"
colnames(phase_data)[colnames(phase_data) == "Follow-Through (ms)"] <- "Follow-Through"
colnames(phase_data)[colnames(phase_data) == "Total (ms)"] <- "Total"
 
################################################################################ 

# Create data from wide to long formats
library(tidyr)

# stride_data
long_stride <- stride_data %>%
  pivot_longer(cols = 3:5, names_to = "variable", values_to = "value")

# phase_data
long_phase <- phase_data %>%
  pivot_longer(cols = 3:7, names_to = "variable", values_to = "value")

# arm_path
long_arm_path <- arm_path %>%
  pivot_longer(cols = 3, names_to = "variable", values_to = "value")

# Summarise long data

# stride_data
summary_data_stride <- long_stride %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE), 
            .groups = 'drop')  # Calculate SD instead of SE

# phase_data
summary_data_phase <- long_phase %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE), 
            .groups = 'drop')  # Calculate SD instead of SE

# arm_path
summary_data_arm_path <- long_arm_path %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE), 
            .groups = 'drop')  # Calculate SD instead of SE

# Define custom orders for each long data frame

# stride_data
long_stride$variable <- factor(long_stride$variable, levels = c("Stride Length (% Height)",
                                                                "Stride Angle (°)",
                                                                "Front Foot Angle (°)"))

# Define the custom order for the variable
long_phase$variable <- factor(long_phase$variable, levels = c("Stride", 
                                                              "Transition", 
                                                              "Acceleration",
                                                              "Follow-Through", 
                                                              "Total"))

################################################################################

# Generate plots for each data frame
library(ggplot2)

# stride_data
 stride <- ggplot() +
    geom_jitter(data = long_stride, aes(x = variable, y = value, group = Player, color = Player),
                shape = 16, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
                size = 1.5, alpha = 0.5) +  # Jittered points by Player
    geom_errorbar(data = summary_data_stride, aes(x = variable, ymin = mean - sd, ymax = mean + sd),
                  width = 0.2, color = "black", size = 1, position = position_dodge(0.5)) +  # Error bars with dodge
    geom_point(data = summary_data_stride, aes(x = variable, y = mean), color = "black", size = 2,
               position = position_dodge(0.5)) +  # Mean points with dodge
    theme_classic() +  # Clean theme
    labs(title = NULL,
         x = NULL,  # Remove x-axis label
         y = NULL) + # Remove y-axis label
    theme(legend.position = "none")
 
  ggsave(stride, filename = "Stride Characteristics (jittered by player).png", width = 4.25, height = 4, dpi = 600, device = "png")


# # Create jittered plot with standard deviation error bars (not jittered by player)
# stride <- ggplot() +
#   geom_jitter(data = long_stride, aes(x = variable, y = value, group = Player, color = Player), 
#               shape = 16, position = position_jitter(0.2), size = 1.5, alpha = 0.5) +  # Jittered points
#   geom_errorbar(data = summary_data_stride, aes(x = variable, ymin = mean - sd, ymax = mean + sd), 
#                 width = 0.2, color = "black", size = 1) +  # Standard deviation error bars
#   geom_point(data = summary_data_stride, aes(x = variable, y = mean), color = "black", size = 2) +  # Mean points
#   theme_classic() +  # Clean theme
#   labs(title = NULL,
#        x = NULL,  # Remove x-axis label
#        y = NULL) + # Remove y-axis label
#   theme(legend.position = "none")
# 
# ggsave(stride, filename = "Stride Characteristics.png", width = 4.25, height = 4, dpi = 600, device = "png")


# phase_data
 phase <- ggplot() +
   geom_jitter(data = long_phase, aes(x = variable, y = value, group = Player, color = Player),
               shape = 16, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
               size = 1.5, alpha = 0.5) +  # Jittered points by Player
   geom_errorbar(data = summary_data_phase, aes(x = variable, ymin = mean - sd, ymax = mean + sd),
                 width = 0.2, color = "black", size = 1, position = position_dodge(0.5)) +  # Error bars with dodge
   geom_point(data = summary_data_phase, aes(x = variable, y = mean), color = "black", size = 2,
              position = position_dodge(0.5)) +  # Mean points with dodge
   theme_classic() +  # Clean theme
   labs(title = NULL,
        x = NULL,  # Remove x-axis label
        y = "Duration (milleseconds)") + 
   theme(legend.position = "none")
 
 ggsave(phase, filename = "Phase Durations (jittered by player).png", width = 6, height = 4, dpi = 600, device = "png")


# # Create jittered plot with standard deviation error bars
# plot1 <- ggplot() +
#   geom_jitter(data = long_phase, aes(x = variable, y = value, group = Player, color = Player), 
#               shape = 16, position = position_jitter(0.2), size = 1.5, alpha = 0.5) +  # Jittered points
#   geom_errorbar(data = summary_data_phase, aes(x = variable, ymin = mean - sd, ymax = mean + sd), 
#                 width = 0.2, color = "black", size = 1) +  # Standard deviation error bars
#   geom_point(data = summary_data_phase, aes(x = variable, y = mean), color = "black", size = 2) +  # Red mean points
#   theme_classic() +  # Clean theme
#   labs(title = NULL,
#        x = NULL,  # Remove x-axis label
#        y = "Duration (milleseconds)") + # Remove y-axis label
#   theme(legend.position = "none")
# 
# ggsave(phase, filename = "Phase Durations (ms).png", width = 6, height = 4, dpi = 600, device = "png")


# arm_path
 armpath <- ggplot() +
   geom_jitter(data = long_arm_path, aes(x = variable, y = value, group = Player, color = Player),
               shape = 16, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
               size = 1.5, alpha = 0.5) +  # Jittered points by Player
   geom_errorbar(data = summary_data_arm_path, aes(x = variable, ymin = mean - sd, ymax = mean + sd),
                 width = 0.2, color = "black", size = 1, position = position_dodge(0.5)) +  # Error bars with dodge
   geom_point(data = summary_data_arm_path, aes(x = variable, y = mean), color = "black", size = 2,
              position = position_dodge(0.5)) +  # Mean points with dodge
   theme_classic() +  # Clean theme
   labs(title = NULL,
        x = NULL,  # Remove x-axis label
        y = NULL) + # Remove y-axis label
   theme(legend.position = "none")
 
 ggsave(armpath, filename = "Arm Path (jittered by player).png", width = 2, height = 4, dpi = 600, device = "png")
 