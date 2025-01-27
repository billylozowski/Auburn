#Script for creating copies of specified files in a File Explorer folder to a new folder (with a partial file name)
#Will need to have the destination folder already created so the path to it can be set

#install required packages
install.packages(readx1)
install.packages(filecopy)

#load the required packages
library(readxl)
library(filecopy)

#read the Excel document using 'readx1'...
# Specify the file path and sheet name
excel_file <- "path/to/your/excel/file.xlsx"
sheet_name <- "Sheet1"  # Modify this to match your sheet name

# Read the Excel file
df <- read_excel(excel_file, sheet = sheet_name)

# Access the column containing the file names
file_names <- df$`Column Name`  # Modify 'Column Name' to match your column name

# Specify the source directory
source_directory <- "path/to/source/directory/"

# Create a regular expression pattern for matching the partial filename or pattern
pattern <- "your_partial_filename_pattern"

# Use list.files with pattern argument to find files matching the pattern
file_names <- list.files(source_directory, pattern = pattern, full.names = TRUE)

#In the pattern argument, you can use regular expressions or specific patterns to match 
#the desired files. For example, if you have the date and time in the file names and 
#want to search for files with a specific date, you can use a pattern like "YYYY-MM-DD"

#Iterate through the matching file names and copy the files
# Specify the destination directory
destination_directory <- "path/to/destination/directory/"

# Iterate through each file name and copy the corresponding file
for (file_name in file_names) {
  destination_path <- paste0(destination_directory, basename(file_name))
  file.copy(file_name, destination_path)
}

