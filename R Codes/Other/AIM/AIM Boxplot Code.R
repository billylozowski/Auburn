
# This script creates box plots for multiple players from an excel file that contains an AIM Score


# Let the user select the file to import
file_path <- file.choose()

library(readxl)
AIM_Data <- read_excel(file_path, col_names = TRUE,)
AIM_Data$Player <- substr(AIM_Data$Player, 1, nchar(AIM_Data$Player) - 9) # remove the visit number + '.csv'


################################################################################


library(tidyverse)
library(ggplot2)
AIM_Data %>%
  ggplot(aes(AIM, Player, Colour = "Player")) +
  geom_boxplot() +
  labs("AIM Score", "Player") +
  theme_bw()


################################################################################


install.packages("lattice")
library(lattice)
# Set the PNG options
png(file = "C:\\Users\\billy\\OneDrive\\Documents\\Auburn\\Sports Medicine & Movement Lab\\Conferences\\ISBS\\2024\\Accuracy-Integrated Metric (AIM) data\\AIM Plots (lattice).png", width = 9, height = 6, units = 'in', res = 300)
bwplot(Player ~ AIM, AIM_Data)
# Save the figure using trellis.device
dev.off()


################################################################################


custom_labels <- c("1", "2", "3", "4", "5", "6", "7")
num_players <- length(custom_labels)

library(ggplot2)

ggplot(AIM_Data, aes(x = AIM, y = as.factor(Player), fill = as.factor(Player))) +
  geom_boxplot(alpha = 0.8) +
  labs(x = "AIM Score", y = "Player", fill = "Player") +
  scale_fill_manual(values = colorRampPalette(c("lightblue", "darkblue"))(num_players), labels = custom_labels) +
  theme_classic() +
  scale_x_discrete(labels = custom_labels) +
  ggtitle("AIM Score and Distribution Across Players") +
  theme(plot.title = element_text(hjust = 0.5)) 


file_name <- paste("C:\\Users\\billy\\OneDrive\\Documents\\Auburn\\Sports Medicine & Movement Lab\\Conferences\\ISBS\\2024\\Accuracy-Integrated Metric (AIM) data\\AIM Plots.png")
       ggsave(filename = file_name, height = 6, width = 9, dpi = 300)
