library(dplyr)

Data <- Scaled_Ball_Data_Analysis_sheet %>%
  select(1:12)

rm(Scaled_Ball_Data_Analysis_sheet)

# DEFINE AGE GROUPS

Data <- Data %>%
  mutate(`Age Group` = case_when(
    Age >=7 & Age <= 8 ~ "7-8",
    Age >=9 & Age <= 10 ~ "9-10",
    Age >=11 & Age <= 12 ~ "11-12",
    Age >=13 & Age <= 14 ~ "13-14",
    TRUE ~ "Other"
  )) %>%
  arrange(`Age Group`)

Data <- Data %>%
  head(79)

custom_order <- c("7-8", "9-10", "11-12", "13-14")

Data$`Age Group` <- factor(Data$`Age Group`, levels = custom_order)

library(ggplot2)

plot <- ggplot(data = Data, aes(x = `Preferred Ball (1-5)`, y = `Hand Size (cm^2)`, color = `Age Group`)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.7) +  # Add linear trendline
  theme_classic() +
  facet_wrap(~ `Age Group`) +
  labs(y = expression("Hand Size (cm"^2*")")) + # Superscript 2
  theme(axis.title = element_text(size = 12))

plot2 <- ggplot(data = Data, aes(x = `Preferred Ball (1-5)`, y = `Hand Size (cm^2)`, color = `Age Group`)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.7) +  # Add linear trendline
  theme_classic() +
  labs(y = expression("Hand Size (cm"^2*")")) + # Superscript 2
  theme(axis.title = element_text(size = 12))

# Save the ggplot
ggsave("Ball Choice x Hand Size (age group).png", plot, width = 8, height = 6, dpi = 600)

ggsave("Ball Choice x Hand Size (stacked).png", plot2, width = 8, height = 6, dpi = 600)


plot3 <- ggplot(data = Data, aes(x = `Choice Difference`, y = `Hand Size (cm^2)`, color = `Age Group`)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.7) +  # Add linear trendline
  theme_classic() +
  labs(y = expression("Hand Size (cm"^2*")")) + # Superscript 2
  theme(axis.title = element_text(size = 12))

ggsave("Choice Difference x Hand Size (stacked).png", plot2, width = 8, height = 6, dpi = 600)
