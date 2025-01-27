
# This code will calculate the Coefficient of Variation (CV) across every trial, 
# every event, for every variable. This will then be used to determine the fundamental
# aspects of the overarm throwing motion.

# Load "Cleaned Event Data (combined)"
file_path <- file.choose()

library(readr)
data <- read_csv(file_path)

# Convert a range of columns to numeric
data[, 3:ncol(data)] <- lapply(data[, 3:ncol(data)], as.numeric)

# Separate the data into Event data frames
library(dplyr)

# Rename columns
colnames(data)[colnames(data) == "Back Knee Flexion"] <- "Back Knee Fle-Ext"
colnames(data)[colnames(data) == "Front Knee Flexion"] <- "Front Knee Fle-Ext"
colnames(data)[colnames(data) == "Back Knee Extension Velocity"] <- "Back Knee Fle-Ext Velocity"
colnames(data)[colnames(data) == "Front Knee Extension Velocity"] <- "Front Knee Fle-Ext Velocity"
colnames(data)[colnames(data) == "Pelvis Rotation (Y)"] <- "Pelvis Rotation"
colnames(data)[colnames(data) == "Trunk Flexion"] <- "Trunk Fle-Ext"
colnames(data)[colnames(data) == "Trunk Flexion Velocity"] <- "Trunk Fle-Ext Velocity"
colnames(data)[colnames(data) == "Trunk Rotation (Y)"] <- "Trunk Rotation"
colnames(data)[colnames(data) == "Shoulder H Abduction"] <- "Shoulder H Abd-Add"
colnames(data)[colnames(data) == "Shoulder H Abduction Velocity"] <- "Shoulder H Abd-Add Velocity"
colnames(data)[colnames(data) == "Elbow Flexion"] <- "Elbow Fle-Ext"
colnames(data)[colnames(data) == "Elbow Extension Velocity"] <- "Elbow Fle-Ext Velocity"

# BFC
BFC_data <- data %>%
  filter(Event == "BFC") 

BFC_mean <- BFC_data %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE, .names = "{.col}"))

BFC_sd <- BFC_data %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE, .names = "{.col}"))

BFC <- rbind(BFC_mean, BFC_sd, (BFC_sd/abs(BFC_mean)))

rownames(BFC) <- c("BFC Mean", "BFC SD", "BFC CV")

rm(BFC_mean, BFC_sd, BFC_data)

# FFC
FFC_data <- data %>%
  filter(Event == "FFC") 

FFC_mean <- FFC_data %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE, .names = "{.col}"))

FFC_sd <- FFC_data %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE, .names = "{.col}"))

FFC <- rbind(FFC_mean, FFC_sd, (FFC_sd/abs(FFC_mean)))

rownames(FFC) <- c("FFC Mean", "FFC SD", "FFC CV")

rm(FFC_mean, FFC_sd, FFC_data)

# MAW
MAW_data <- data %>%
  filter(Event == "MAW")

MAW_mean <- MAW_data %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE, .names = "{.col}"))

MAW_sd <- MAW_data %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE, .names = "{.col}"))

MAW <- rbind(MAW_mean, MAW_sd, (MAW_sd/abs(MAW_mean)))

rownames(MAW) <- c("MAW Mean", "MAW SD", "MAW CV")

rm(MAW_mean, MAW_sd, MAW_data)

# BR
BR_data <- data %>%
  filter(Event == "BR") 

BR_mean <- BR_data %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE, .names = "{.col}"))

BR_sd <- BR_data %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE, .names = "{.col}"))

BR <- rbind(BR_mean, BR_sd, (BR_sd/abs(BR_mean)))

rownames(BR) <- c("BR Mean", "BR SD", "BR CV")

rm(BR_mean, BR_sd, BR_data)

# EFT
EFT_data <- data %>%
  filter(Event == "EFT") 

EFT_mean <- EFT_data %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE, .names = "{.col}"))

EFT_sd <- EFT_data %>%
  summarise(across(where(is.numeric), sd, na.rm = TRUE, .names = "{.col}"))

EFT <- rbind(EFT_mean, EFT_sd, (EFT_sd/abs(EFT_mean)))

rownames(EFT) <- c("EFT Mean", "EFT SD", "EFT CV")

rm(EFT_mean, EFT_sd, EFT_data)


# Create a new data frame with the CV rows from each event
CV_data <- rbind(BFC[3, ], FFC[3, ], MAW[3, ], BR[3, ], EFT[3, ])
# Name the rows as the events the data came from
rownames(CV_data) <- c("BFC", "FFC", "MAW", "BR", "EFT")

CV_data <- t(CV_data) # transpose the data frame

summary_data <- rbind(BFC, FFC, MAW, BR, EFT)
rownames(summary_data) <- c("BFC", "FFC", "MAW", "BR", "EFT")
summary_data <- t(summary_data) # transpose the data frame

# Save the CV data and summary data
write.csv(CV_data, "Coefficient of Variation Data.csv", row.names = TRUE, na = "")
write.csv(summary_data, "Means, SD, and CV Data.csv", row.names = TRUE, na = "")

#rm(list = ls())
