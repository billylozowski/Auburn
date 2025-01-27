
# DEFINE PARTICIPANT
participant_code <- "SB_HS_AT_01_10-09-23"

################################################################################

# LOAD RELEVANT PACKAGES
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tools)

################################################################################

# IMPORT DATA FROM FILE (master sheet) AND CLEAN

# Let the user select the file to import
file_path <- file.choose()

# # Extract the base file name (used for saving the data)
# library(tools)
# base_filename <- basename(file_path)

# Read the file and name columns
data <- read_excel(file_path, sheet = "Master", col_names = TRUE) %>%
  mutate(across(23:50, as.numeric))

# Remove column header suffixes ('...' + 'col number')
colnames_data <- colnames(data)
new_colnames <- sub("\\.\\.\\..*", "", colnames_data)
colnames(data) <- new_colnames

# Remove columns not containing required information
## For SB Throwing: cols 1:10, 23:38, 40:43
## For SB Hitting: cols 1:10, 23:38, 40:43
## For BB Throwing: cols: 1:10, 24:39, 41:44
## For BB Hitting: cols: 1:10, 22:37, 39:42
complete_data <- data[, c(1:10, 23:38, 40:43)]
complete_data <- complete_data[complete.cases(complete_data[, 11:26]), ]

# Re-order date to 'dd-mm-yyyy'
complete_data$`Date of Visit` <- format(as.Date(complete_data$`Date of Visit`), "%d-%m-%Y")

rm(data)

################################################################################

# DEFINE THE AGE GROUPS

complete_data <- complete_data %>%
  mutate(`Age Group` = case_when(
    `Age at Visit` < 14 ~ "Youth",
    `Age at Visit` >= 14 & `Age at Visit` <= 17 ~ "High School",
    `Age at Visit` >= 18 & `Age at Visit` <= 23 ~ "College",
    TRUE ~ "Other"
  )) %>%
  arrange(`Age Group`)

################################################################################

# CREATE A DATA FRAME WITH ONLY THE PARTICIPANT DATA IN IT

# Select only the participant's data
participant <- complete_data %>%
  filter(`Collection Code` == participant_code)
participant_age <- participant$`Age Group`

variable_range <- 11:30

################################################################################

# DETERMINE THE PEER GROUP BASED ON PARTICIPANT'S AGE
for (col_index in variable_range) {
  col_name <- names(complete_data)[col_index]
  if (participant$`Age Group` == "Youth") {
    peer_group <- complete_data %>%
      filter(`Age Group` == "Youth")
  } else if (participant$`Age Group` == "High School") {
    peer_group <- complete_data %>%
      filter(`Age Group` == "High School")
  } else if (participant$`Age Group` == "College") {
    peer_group <- complete_data %>%
      filter(`Age Group` == "College")
  } else {
    peer_group <- complete_data %>%
      filter(`Age Group` == "Other")
  }
}

rm(complete_data)

################################################################################

# SEPARATE ROM, TROM, AND STRENGTH VARIABLES INTO SEPARATE DATA FRAMES AND PLOT

# Participant Range of Motion (ROM)
participant_ROM <- participant %>%
  select(`R Hip IR ROM`, `R Hip ER ROM`, `L Hip IR ROM`, `L Hip ER ROM`, 
         `Dom SH IR ROM`, `Dom SH ER ROM`, `NDom SH IR ROM`, `NDom SH ER ROM`)
participant_ROM <- pivot_longer(participant_ROM, cols = everything()) # convert to long format for box plots
participant_ROM$name <- factor(participant_ROM$name,
                               levels = c("R Hip IR ROM", "R Hip ER ROM", 
                                          "L Hip IR ROM", "L Hip ER ROM", 
                                          "Dom SH IR ROM", "Dom SH ER ROM", 
                                          "NDom SH IR ROM", "NDom SH ER ROM"))

# Peer Group Range of Motion
peer_ROM <- peer_group %>%
  select(`R Hip IR ROM`, `R Hip ER ROM`, `L Hip IR ROM`, `L Hip ER ROM`, 
         `Dom SH IR ROM`, `Dom SH ER ROM`, `NDom SH IR ROM`, `NDom SH ER ROM`)
peer_ROM <- pivot_longer(peer_ROM, cols = everything()) # convert to long format for box plots
peer_ROM$name <- factor(peer_ROM$name,
                        levels = c("R Hip IR ROM", "R Hip ER ROM", 
                                   "L Hip IR ROM", "L Hip ER ROM", 
                                   "Dom SH IR ROM", "Dom SH ER ROM", 
                                   "NDom SH IR ROM", "NDom SH ER ROM")) 

# Define a custom colour palette
ROM_colours <- c("R Hip IR ROM" = "#FFE0B2", "R Hip ER ROM" = "#FFCC80",
                 "L Hip IR ROM" = "#FFB74D", "L Hip ER ROM" = "#FFA726",
                 "Dom SH IR ROM" = "#FF9800", "Dom SH ER ROM" = "#FB8C00",
                 "NDom SH IR ROM" = "#F57C00", "NDom SH ER ROM" = "#EF6C00")

# Plot Peer Group and Participant Data  
plot <- ggplot(peer_ROM, aes(x = value, y = name, fill = name)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  scale_fill_manual(values = ROM_colours) +
  xlab("Range of Motion (°)") + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), # remove grid lines
        plot.background = element_rect(fill = "white"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        legend.position = "none") # set background to white

ROM_plot <- plot + geom_point(data = participant_ROM, aes(x = value, y = name), color = "#133EB2", size = 3)

# Save the plot to designated folder
file_name <- paste("C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/Misc/ROM & ISO Normative Data/Participant/Range of Motion", ".png", sep="")
ggsave(filename = file_name, plot = ROM_plot, device = "png", width = 8, height = 6, units = "in", dpi = 300)

rm(participant_ROM, peer_ROM, ROM_colours, ROM_plot) 

# Participant Total Range of Motion Arc
participant_TROM <- participant %>%
  select(`R Hip TROM`, `L Hip TROM`, `Dom Shoulder TROM`, `NDom Shoulder TROM`)
participant_TROM <- pivot_longer(participant_TROM, cols = everything()) # convert to long format for box plots
participant_TROM$name <- factor(participant_TROM$name,
                                levels = c("R Hip TROM", "L Hip TROM",
                                           "Dom Shoulder TROM", "NDom Shoulder TROM"))

# Peer Group Total Range of Motion Arc
peer_TROM <- peer_group %>%
  select(`R Hip TROM`, `L Hip TROM`, `Dom Shoulder TROM`, `NDom Shoulder TROM`)
peer_TROM <- pivot_longer(peer_TROM, cols = everything())
peer_TROM$name <- factor(peer_TROM$name,
                         levels = c("R Hip TROM", "L Hip TROM",
                                    "Dom Shoulder TROM", "NDom Shoulder TROM"))

# Define a custom colour palette
TROM_colours <- c("R Hip TROM" = "gray91", "L Hip TROM" = "gray76",
                 "Dom Shoulder TROM" = "gray61", "NDom Shoulder TROM" = "gray46")

# Plot Peer Group and Participant Data 
plot <- ggplot(peer_TROM, aes(x = value, y = name, fill = name)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  scale_fill_manual(values = TROM_colours) +
  xlab("Total Range of Motion (°)") + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), # remove grid lines
        plot.background = element_rect(fill = "white"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        legend.position = "none") # set background to white

TROM_plot <- plot + geom_point(data = participant_TROM, aes(x = value, y = name), color = "red", size = 3)

# Save the plot to designated folder
file_name <- paste("C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/Misc/ROM & ISO Normative Data/Participant/Total Range of Motion", ".png", sep="")
ggsave(filename = file_name, plot = TROM_plot, device = "png", width = 8, height = 6, units = "in", dpi = 300)

rm(participant_TROM, peer_TROM, TROM_colours, TROM_plot)   

# Participant Strength  
participant_Strength <- participant %>%
  select(`R Hip IR Strength`, `R Hip ER Strength`, `L Hip IR Strength`, `L Hip ER Strength`,
         `Dom SH IR Strength`, `Dom SH ER Strength`, `NDom SH IR Strength`, `NDom SH ER Strength`)
participant_Strength <- pivot_longer(participant_Strength, cols = everything()) # convert to long format for box plots

# Peer Group Strength  
peer_Strength <- peer_group %>%
  select(`R Hip IR Strength`, `R Hip ER Strength`, `L Hip IR Strength`, `L Hip ER Strength`,
         `Dom SH IR Strength`, `Dom SH ER Strength`, `NDom SH IR Strength`, `NDom SH ER Strength`)
peer_Strength <- pivot_longer(peer_Strength, cols = everything()) # convert to long format for box plots
peer_Strength$name <- factor(peer_Strength$name,
                             levels = c("R Hip IR Strength", "R Hip ER Strength",
                                        "L Hip IR Strength", "L Hip ER Strength",
                                        "Dom SH IR Strength", "Dom SH ER Strength",
                                        "NDom SH IR Strength", "NDom SH ER Strength"))

# Define a custom colour palette
Strength_colours <- c("R Hip IR Strength" = "#B7CEEC", "R Hip ER Strength" = "#9DB9E4",
                      "L Hip IR Strength" = "#85A4DC", "L Hip ER Strength" = "#6C8FD3",
                      "Dom SH IR Strength" = "#557BCB", "Dom SH ER Strength" = "#3F67C3",
                      "NDom SH IR Strength" = "#2952BA", "NDom SH ER Strength" = "#133EB2")

# Plot Peer Group and Participant Data     
plot <- ggplot(peer_Strength, aes(x = value, y = name, fill = name)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  scale_fill_manual(values = Strength_colours) +
  xlab("Strength (Nm)") + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), # remove grid lines
        plot.background = element_rect(fill = "white"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        legend.position = "none") # set background to white

ISO_plot <- plot + geom_point(data = participant_Strength, aes(x = value, y = name), color = "#EF6C00", size = 3)

# Save the plot to designated folder
file_name <- paste("C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/Misc/ROM & ISO Normative Data/Participant/Strength", ".png", sep="")
ggsave(filename = file_name, plot = ISO_plot, device = "png", width = 8, height = 6, units = "in", dpi = 300)

rm(participant_Strength, peer_Strength, Strength_colours, ISO_plot) 

################################################################################

# SUMMARISE DATA AND CREATE A TABLE. SAVE TABLE TO .csv FILE


summary_tables <- list()

for (col_index in variable_range) {
  col_name <- names(peer_group)[col_index]
  summary_data <- peer_group %>%
    summarize(Q1 = quantile(.data[[col_name]], 0.25), Q3 = quantile(.data[[col_name]], 0.75)) %>%
    mutate(Variable = col_name, IQR = Q3 - Q1) %>%
    select(Variable, Q1, Q3, IQR)
  
  # Round columns to desired decimal places
  summary_data$Q1 <- sprintf("%.1f", summary_data$Q1)
  summary_data$Q3 <- sprintf("%.1f", summary_data$Q3)
  summary_data$IQR <- sprintf("%.1f", summary_data$IQR)
  
  summary_tables[[col_index - 10]] <- summary_data
}

summary_table <- bind_rows(summary_tables)

# Specify the directory and file path and write the summary table to a CSV file
file_path <- "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/Misc/ROM & ISO Normative Data/Participant/IQR Summary Table (peer group).csv"
write.csv(summary_table, file = file_path, row.names = FALSE)

rm(list = ls())
