
# IMPORT DATA FROM FILE (master sheet) and clean

# Let the user select the file to import
file_path <- file.choose()


# Extract the base file name (used for saving the data)
library(tools)
base_filename <- file_path_sans_ext(file_path)
base_filename <- basename(file_path)


# Read the file and name columns
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

data <- read_excel(file_path, sheet = "Master", col_names = TRUE) %>%
  mutate(across(23:50, as.numeric))

# Remove column header suffixes ('...' + 'col number')
# Get the current column names
colnames_data <- colnames(data)

# Use regular expression substitution to remove '...' and column number
new_colnames <- sub("\\.\\.\\..*", "", colnames_data)

# Set the new column names to the data frame
colnames(data) <- new_colnames


# Remove columns not containing required information
## For SB Throwing: cols 1:10, 23:38, 40:43
## For SB Hitting: cols 1:10, 23:38, 40:43
## For BB Throwing: cols: 1:10, 24:39, 41:44
## For BB Hitting: cols: 1:10, 22:37, 39:42
complete_data <- data[, c(1:10, 24:39, 41:44)] # keeps only anthro, ROM, and ISO data
complete_data <- complete_data[complete.cases(complete_data[, 11:26]), ] # removes rows where ROM/ISO values are missing
 
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
  
  
# Specify the variables to be plotted below
variable_range <- 11:30


################################################################################


 # Create an empty list to store the plots
 density_plots <- list()
 
 # Define the order of 'Age Group' levels
 age_group_order <- c("Youth", "High School", "College", "Other")
 
 # Set the 'Age Group' variable to a factor with the desired order
 complete_data$`Age Group` <- factor(complete_data$`Age Group`, levels = age_group_order)
 
 # Iterate through the columns
 for (col_index in variable_range) {
   # Get the column name
   col_name <- names(complete_data)[col_index]
   
   # Define the colors you want for each level
   age_group_colors <- c("Youth" = "lightskyblue", "High School" = "steelblue2", "College" = "deepskyblue2", "Other" = "royalblue4")
  
   # Create a summary data frame with Q1 and Q3 values for each Age Group
   summary_data <- complete_data %>%
     group_by(`Age Group`) %>%
     summarize(Q1 = quantile(.data[[col_name]], 0.25), Q3 = quantile(.data[[col_name]], 0.75))
   
   # Create the density plot for the current variable
   plot <- ggplot(complete_data, aes(x = .data[[col_name]], fill = `Age Group`)) +
     geom_density(alpha = 0.7) +
     facet_wrap(~ `Age Group`, ncol = 1) +  # Set ncol to 1 to stack plots vertically
     scale_fill_manual(values = age_group_colors) + 
     labs(x = paste(col_name, "(°)")) +
     theme_minimal() +
     theme(panel.grid.major = element_blank(),  # Remove major grid lines
           panel.grid.minor = element_blank()) +
     theme(plot.background = element_rect(fill = "white"),
           axis.ticks.y = element_blank(), 
           axis.text.y = element_blank(), # Remove y-axis text and ticks
           axis.title.y = element_blank()) + # remove y title
     geom_vline(data = summary_data, aes(xintercept = Q1), color = "black", linetype = "dashed", linewidth = 0.75) +  # Add Q1 as dashed lines
     geom_vline(data = summary_data, aes(xintercept = Q3), color = "black", linetype = "dashed", linewidth = 0.75)  # Add Q3 as dashed lines
   
   # Add the plot to the list
   density_plots[[col_index - 10]] <- plot
 }
 

 # Print or save the plots from the list
 for (i in 1:length(density_plots)) {
   print(density_plots[[i]])
   
   # Define the file name for each plot (you can customize the file name)
   file_name <- paste("C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/Misc/ROM & ISO Normative Data/Baseball Throwing/Density Plot ", i, ".png", sep="")
   # Use ggsave to save the plot as a PNG file (you can change the format and options)
   ggsave(filename = file_name, plot = density_plots[[i]], width = 9, height = 6, dpi = 300, device = "png")
 }
 
 
################################################################################
 
# Generate box plots
 
 # Iterate through the columns
 for (col_index in variable_range) {
   # Get the column name
   col_name <- names(complete_data)[col_index]
   
   # Define the colors you want for each level
   age_group_colors <- c("Youth" = "lightskyblue", "High School" = "steelblue2", "College" = "deepskyblue2", "Other" = "royalblue4")
   
   # Create a summary data frame with Q1 and Q3 values for each Age Group
   summary_data <- complete_data %>%
     group_by(`Age Group`) %>%
     summarize(Q1 = quantile(.data[[col_name]], 0.25), Q3 = quantile(.data[[col_name]], 0.75))
   
   # Create the density plot for the current variable
   plot <- ggplot(complete_data, aes(x = .data[[col_name]], fill = `Age Group`)) +
     geom_boxplot(alpha = 0.7, width = 0.4, position = position_dodge(width = 0.8)) +
     labs(x = paste(col_name)) +
     # facet_wrap(~ `Age Group`, ncol = 1) +  # Set ncol to 1 to stack plots vertically
     scale_fill_manual(values = age_group_colors) + 
     # scale_y_discrete(labels = unique(complete_data$`Age Group`)) +  # Set the labels on y-axis
     theme_minimal() +
     theme(panel.grid.major = element_blank(),  # Remove grid lines
           panel.grid.minor = element_blank(),
           plot.background = element_rect(fill = "white"),
           axis.text.y = element_blank(),
           plot.title = element_text(hjust = 0.5)) # centre plot title
   
   # Add the plot to the list
   density_plots[[col_index - 10]] <- plot
 }
 
 
 # Print or save the plots from the list
 for (i in 1:length(density_plots)) {
   print(density_plots[[i]])
   
   # Define the file name for each plot (you can customize the file name)
   file_name <- paste("C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/Misc/ROM & ISO Normative Data/Baseball Throwing/Box Plot ", i, ".png", sep="")
   # Use ggsave to save the plot as a PNG file (you can change the format and options)
   ggsave(filename = file_name, plot = density_plots[[i]], width = 9, height = 6, dpi = 300, device = "png")
 }
 
 rm(density_plots, plot, age_group_colors, age_group_order, i, colnames_data, new_colnames)
 
 
################################################################################
 
 
 # Create a Summary Table of the IQR values

 # Create an empty list to store the summary data frames
 summary_tables <- list()
 
 library(dplyr)
 
  # Iterate through the columns
 for (col_index in variable_range) {
   # Get the column name from the complete_data data frame
   col_name <- names(complete_data)[col_index]
   
   # Create a summary data frame with Q1 and Q3 values for each Age Group
   summary_data <- complete_data %>%
     group_by(`Age Group`) %>%
     summarize(Q1 = quantile(.data[[col_name]], 0.25), Q3 = quantile(.data[[col_name]], 0.75)) %>%
     mutate(Variable = col_name, IQR = Q3 - Q1) %>%
     select(Variable, `Age Group`, Q1, Q3, IQR)  # Rearrange columns
   
   
   # Round columns to desired decimal places
   summary_data$Q1 <- sprintf("%.1f", summary_data$Q1)
   summary_data$Q3 <- sprintf("%.1f", summary_data$Q3)
   summary_data$IQR <- sprintf("%.1f", summary_data$IQR)
   
   
   # Add the summary data frame to the list
   summary_tables[[col_index - 10]] <- summary_data
 }
 
 # Combine all the summary data frames into a single data frame
 summary_table <- bind_rows(summary_tables)
 

################################################################################
 

 # Specify the directory and file path and write the summary table to a CSV file
 file_path <- "C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/Misc/ROM & ISO Normative Data/Baseball Throwing/IQR Summary Table.csv"
 write.csv(summary_table, file = file_path, row.names = FALSE)
 
 rm(summary_data, col_index, col_name, file_name, variable_range, summary_tables)
 
 
