library(ggplot2)
library(tidyr)

data <- Aubrie_Lisenby %>%
gather(key = "Variable", value = "Value", 4:23)

data$Variable <- factor(data$Variable, levels = c("L Hip IR ROM", "L Hip ER ROM", "L Hip TROM", "L Hip IR Strength", "L Hip ER Strength",
                                                  "R Hip IR ROM", "R Hip ER ROM", "R Hip TROM", "R Hip IR Strength", "R Hip ER Strength",
                                                  "Dom SH IR ROM", "Dom SH ER ROM", "Dom Shoulder TROM", "Dom SH IR Strength", "Dom SH ER Strength",
                                                  "NDom SH IR ROM", "NDom SH ER ROM", "NDom Shoulder TROM", "NDom SH IR Strength", "NDom SH ER Strength")) # Add your variable names in the desired order

# Plot the data
AL <- ggplot(data, aes(x = `Visit #`, y = Value, color = factor(Variable))) +
  geom_line() +
  facet_wrap(~Variable, scales = "free") +  # facet_wrap by 'Variable'
  labs(x = "Visit #", y = "Value (°)", color = "Variable") + # Customize axis labels and legend title
  theme_classic() +
  ggtitle("Aubrie Lisenby ROM/ISO x Visit") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

#Save the plot
ggsave("C:/Users/billy/OneDrive/Documents/Auburn/Sports Medicine & Movement Lab/Misc/ROM & ISO Normative Data/plot.png", AL, width = 10, height = 8, units = "in", dpi = 600)

