
# Let the user select the file to import
file_path <- file.choose()

# Read the data
data <- read.delim(file_path, header = TRUE, skip = 8, check.names = FALSE)
file_name <- tools::file_path_sans_ext(basename(file_path))

# Remove column "51" as it's empty
library(dplyr)
data <- data %>%
  select(! 51) # change to the final column

# Define sampling rate (Hz)
SampleRate <- 100 # change as necessary

# Create and events column and populate with 0s
data$Events <- 0

# Create the column "Time (s)" (divide by sampling rate)
data$`Time (s)` <- data$`Sample #` / SampleRate

# Add a File Name column
data$`File Name` <- file_name


################################################################################

COM_height <- mean(data$COM[1:50, ])

# Define Event Parameters

# find the first instance where Event 1 meets specified threshold
X_index <- which(data$`Event - unloading` <= (COM_height - 0.05))[1] # adjust threshold as necessary

# find the first instance where Event 2 meets specified threshold
Y_index <- which(data$`Event - bottom` == min(COM))[1] # adjust threshold as necessary

# find the first instance where Event 3 meets specified threshold
Z_index <- which(data$`Event - FFC` > 30)[1] # adjust threshold as necessary

# find the first instance where Event 4 meets specified threshold
A_index <- which(data$`Event - FFC` > 30)[1] # adjust threshold as necessary


# Mark X_index with '1'
if (!is.na(X_index)) {
  data$Events[X_index] <- 1
}

# Mark Y_index with '1'
if (!is.na(X_index)) {
  data$Events[Y_index] <- 1
}

# Mark Z_index with '1'
if (!is.na(Z_index)) {
  data$Events[Z_index] <- 1
}

# Mark A_index with '1'
if (!is.na(A_index)) {
  data$Events[A_index] <- 1
}

# Check the number of marked events
sum(data$Events)


events <- data.frame(file_name,
                     X_index,
                     Y_index,
                     Z_index,
                     A_index)

colnames(events) <- c("File Name", "X", "Y", "Z", "A")

print(events)










