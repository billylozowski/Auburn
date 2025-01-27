
# this script calculates the AIM score for individual throws based on Rapsodo
# strike zone locations


# Let the user select the file to import
file_path <- file.choose()


# Load Rapsodo .csv File

library(readxl)
        data <- read_excel(file_path, col_names = TRUE, skip = 3) 


# Select variables for AIM

library(tidyverse)
        data <- data %>%
          select(Velocity, `Strike Zone Side`, `Strike Zone Height`) #, `Release Extension (ft)`)

data$Velocity <- as.numeric(data$Velocity)        
data$`Strike Zone Side` <- as.numeric(data$`Strike Zone Side`)    
data$`Strike Zone Height` <- as.numeric(data$`Strike Zone Height`)  
# data$`Release Extension (ft)` <- as.numeric(data$`Release Extension (ft)`)                   

complete_data <- data[complete.cases(data), ]
        
        
# Convert into SI units

throw_distance <- 18.288

complete_data$Velocity <- complete_data$Velocity * 0.44704 # mph to m/s
complete_data$`Strike Zone Side` <- complete_data$`Strike Zone Side` * 0.0254 # inches to m
complete_data$`Strike Zone Height` <- complete_data$`Strike Zone Height` * 0.0254 # inches to m
complete_data$distance <- throw_distance #- (complete_data$`Release Extension (ft)` * 0.3048) # ft to m
   
rm(data)     


# Separate into pitch types, specify the intended location, and calculate AIM score
# Be sure to rename all data frames accordingly!

# Fastball        
FB <- complete_ER %>%
  filter(`Pitch Type` == "Fastball")
FB$aim_height <- 30 * 0.0254
FB$aim_side <- 0 * 0.0254

FB$`Strike Zone Side (adj.)` <- FB$`Strike Zone Side` - FB$aim_side
FB$`Strike Zone Height (adj.)` <- FB$`Strike Zone Height` - FB$aim_height

FB$`Displacement (m)` <- sqrt(FB$`Strike Zone Height (adj.)`^2 + FB$`Strike Zone Side (adj.)`^2)

FB$`AIM Score` <- ((FB$Velocity * FB$distance) / FB$`Displacement (m)`) / 100 # calculate A.I.M. score

# Substitute "Fastball" with "FB"
FB$`Pitch Type` <- gsub("Fastball", "FB", FB$`Pitch Type`)


# Change-Up
CU <- complete_ER %>%
  filter(`Pitch Type` == "ChangeUp")
CU$aim_height <- 21 * 0.0254
CU$aim_side <- -7.5 * 0.0254
CU$`Strike Zone Side (adj.)` <- CU$`Strike Zone Side` - CU$aim_side
CU$`Strike Zone Height (adj.)` <- CU$`Strike Zone Height` - CU$aim_height

CU$`Displacement (m)` <- sqrt(CU$`Strike Zone Height (adj.)`^2 + CU$`Strike Zone Side (adj.)`^2)

CU$`AIM Score` <- ((CU$Velocity * CU$distance) / CU$`Displacement (m)`) / 100 # calculate A.I.M. score

# Substitute "Fastball" with "FB"
CU$`Pitch Type` <- gsub("ChangeUp", "CU", CU$`Pitch Type`)


# Riseball        
RB <- complete_ER %>%
  filter(`Pitch Type` == "riser")     
RB$aim_height <- 39 * 0.0254
RB$aim_side <- 7.5 * 0.0254
RB$`Strike Zone Side (adj.)` <- RB$`Strike Zone Side` - RB$aim_side
RB$`Strike Zone Height (adj.)` <- RB$`Strike Zone Height` - RB$aim_height

RB$`Displacement (m)` <- sqrt(RB$`Strike Zone Height (adj.)`^2 + RB$`Strike Zone Side (adj.)`^2)

RB$`AIM Score` <- ((RB$Velocity * RB$distance) / RB$`Displacement (m)`) / 100 # calculate A.I.M. score

# Substitute "Fastball" with "FB"
RB$`Pitch Type` <- gsub("riser", "RB", RB$`Pitch Type`)


# Dropball          
DB<- complete_ER %>%
  filter(`Pitch Type` == "dropball")
DB$aim_height <- 21 * 0.0254
DB$aim_side <- 0 * 0.0254
DB$`Strike Zone Side (adj.)` <- DB$`Strike Zone Side` - DB$aim_side
DB$`Strike Zone Height (adj.)` <- DB$`Strike Zone Height` - DB$aim_height

DB$`Displacement (m)` <- sqrt(DB$`Strike Zone Height (adj.)`^2 + DB$`Strike Zone Side (adj.)`^2)

DB$`AIM Score` <- ((DB$Velocity * DB$distance) / DB$`Displacement (m)`) / 100 # calculate A.I.M. score

# Substitute "Fastball" with "FB"
DB$`Pitch Type` <- gsub("dropball", "DB", DB$`Pitch Type`)


# Curveball  
CB <- complete_ER %>%
  filter(`Pitch Type` == "CurveBall")
CB$aim_height <- 30 * 0.0254
CB$aim_side <- -7.5 * 0.0254
CB$`Strike Zone Side (adj.)` <- CB$`Strike Zone Side` - CB$aim_side
CB$`Strike Zone Height (adj.)` <- CB$`Strike Zone Height` - CB$aim_height

CB$`Displacement (m)` <- sqrt(CB$`Strike Zone Height (adj.)`^2 + CB$`Strike Zone Side (adj.)`^2)

CB$`AIM Score` <- ((CB$Velocity * DB$distance) / CB$`Displacement (m)`) / 100 # calculate A.I.M. score

# Substitute "Fastball" with "FB"
CB$`Pitch Type` <- gsub("CurveBall", "CB", CB$`Pitch Type`)


# FULL GAME FIGURES

library(dplyr)
full_ER <- bind_rows(CB, CU, DB, FB, RB)

rm(CB,CU,FB,DB,RB)

library(ggplot2)

ggplot(full_ER, aes(x = factor(Inning), y = `AIM Score`, fill = factor(`Pitch Type`))) +
  geom_boxplot() +
  facet_wrap(~ factor(`Pitch Type`), ncol = 2, scales = "free") +
  labs(title = " AIM Score x Pitch Type x Inning", x = "Inning", y = "AIM Score", fill = "Inning") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.position = "none")

# Save the plot as PNG file
ggsave(filename = "Auburn/SMML/Misc/Summer Webinar Project/AIM Scores.png", width = 10, height = 6, dpi = 600)


# SPLIT DATA BY INNING TO DETERMINE MEAN AIM SCORE FOR EACH PITCH TYPE.

library(dplyr)

# First
first <- full_ER %>%
  filter(Inning == "1")

first_mean <- aggregate(`AIM Score` ~ `Pitch Type`, data = first, FUN = mean)
first_median <- aggregate(`AIM Score` ~ `Pitch Type`, data = first, FUN = median)

# Second
second <- full_ER %>%
  filter(Inning == "2")

second_mean <- aggregate(`AIM Score` ~ `Pitch Type`, data = second, FUN = mean)
second_median <- aggregate(`AIM Score` ~ `Pitch Type`, data = second, FUN = median)

# Third
third <- full_ER %>%
  filter(Inning == "3")

third_mean <- aggregate(`AIM Score` ~ `Pitch Type`, data = third, FUN = mean)
third_median <- aggregate(`AIM Score` ~ `Pitch Type`, data = third, FUN = median)

# Forth
forth <- full_ER %>%
  filter(Inning == "4")

forth_mean <- aggregate(`AIM Score` ~ `Pitch Type`, data = forth, FUN = mean)
forth_median <- aggregate(`AIM Score` ~ `Pitch Type`, data = forth, FUN = median)

# Fifth
fifth <- full_ER %>%
  filter(Inning == "5")

fifth_mean <- aggregate(`AIM Score` ~ `Pitch Type`, data = fifth, FUN = mean)
fifth_median <- aggregate(`AIM Score` ~ `Pitch Type`, data = fifth, FUN = median)

# Sixth
sixth <- full_ER %>%
  filter(Inning == "6")

sixth_mean <- aggregate(`AIM Score` ~ `Pitch Type`, data = sixth, FUN = mean)
sixth_median <- aggregate(`AIM Score` ~ `Pitch Type`, data = sixth, FUN = median)

# Seventh
seventh <- full_ER %>%
  filter(Inning == "7")

seventh_mean <- aggregate(`AIM Score` ~ `Pitch Type`, data = seventh, FUN = mean)
seventh_median <- aggregate(`AIM Score` ~ `Pitch Type`, data = seventh, FUN = median)


# Merge Innings Stats (CB is first Pitch Type of each innings, RB is last)

library(dplyr)
mean_AIMScore <- bind_rows(first_mean, second_mean, third_mean, forth_mean, fifth_mean, sixth_mean, seventh_mean)
median_AIMScore <- bind_rows(first_median, second_median, third_median, forth_median, fifth_median, sixth_median, seventh_median)

rm(first, second, third, forth, fifth, sixth, seventh)
rm(first_mean, second_mean, third_mean, forth_mean, fifth_mean, sixth_mean, seventh_mean)
rm(first_median, second_median, third_median, forth_median, fifth_median, sixth_median, seventh_median)


# Write file to .csv

write.table(full_ER, file = "ER Complete.csv", sep = ",")
write.table(mean_AIMScore, file = "Mean AIM Scores.csv", sep = ",")
write.table(median_AIMScore, file = "Median AIM Scores.csv", sep = ",")         
