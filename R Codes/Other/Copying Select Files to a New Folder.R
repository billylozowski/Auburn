#Script for creating copies of specified files in a File Explorer folder to a new folder (with the full file name)
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

#iterate through the file names and copy the files...
# Specify the source directory and the destination directory
source_directory <- "path/to/source/directory/"
destination_directory <- "path/to/destination/directory/"

# Iterate through each file name and copy the corresponding file
for (file_name in file_names) {
  source_path <- paste0(source_directory, file_name)
  destination_path <- paste0(destination_directory, file_name)
  file.copy(source_path, destination_path)
}