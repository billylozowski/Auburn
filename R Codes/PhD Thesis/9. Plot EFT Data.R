
# Plot "EFT Data" with ball release (frame 58)

library(readxl)
data <- read_excel("Auburn/Sports Medicine & Movement Lab/PhD Thesis/Final Thesis/Chapter 3 - Methods/Tables and Figures/EFT Data.xlsx")

library(ggplot2)
eft_plot <- ggplot(data, aes(x = Sample, y = `Event - EFT`, group = `File Name`)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_segment(aes(x = 58, xend = 58, y = -2, yend = 0.9), linetype = "dashed", color = "red") +
  annotate("text", x = 58, y = -2.3, label = "BR", vjust = -1, color = "black") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_blank(),
        axis.text = element_blank(),        # Remove axis text
        axis.ticks = element_blank())       # Remove axis text

ggsave("Combined EFT.png", plot = eft_plot, width = 7.5, height = 5, dpi = 600, device = "png")


single <- data[1:500, ] 

library(ggplot2)
single_eft_plot <- ggplot(single, aes(x = Sample, y = `Event - EFT`, group = `File Name`)) +
  geom_line(size = 0.75, alpha = 0.5) +
  geom_segment(aes(x = 58, xend = 58, y = -2, yend = 0.9), linetype = "dashed", color = "red") +
  annotate("text", x = 58, y = -2.3, label = "BR", vjust = -1, color = "black") +
  geom_segment(aes(x = 82, y = -0.5, xend = 82, yend = -0.05), 
               arrow = arrow(length = unit(0.3, "cm")), color = "blue", size = 1) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_blank(),
        axis.text = element_blank(),        # Remove axis text
        axis.ticks = element_blank())       # Remove axis text
  
ggsave("Single EFT.png", plot = single_eft_plot, width = 7.5, height = 5, dpi = 600, device = "png")