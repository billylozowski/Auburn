#Script for creating copies of select files, placing them in a new folder. 
#THis should work for searches across multiple subfolders.

# Required packages
library(readxl)
library(filecopy)

# Specify the paths
excel_file <- "path/to/your/excel/file.xlsx"
source_directory <- "path/to/source/directory/"
destination_directory <- "path/to/destination/directory/"

# Read the Excel file
df <- read_excel(excel_file)

# Access the column containing the partial file names
partial_file_names <- df$Column_Name  # Modify 'Column_Name' to match your column name

# Pattern matching function
match_partial_name <- function(partial_name, source_directory) {
  # Recursive search for files matching the partial name in the subfolders
  files_found <- list.files(path = source_directory,
                            pattern = partial_name,
                            recursive = TRUE,
                            full.names = TRUE)
  
  return(files_found)
}

# Iterate through each partial file name and copy the corresponding files
for (partial_name in partial_file_names) {
  # Search for files matching the partial name in the subfolders
  files_found <- match_partial_name(partial_name, source_directory)
  
  # Copy the files to the destination directory
  for (file_found in files_found) {
    destination_path <- file.path(destination_directory, basename(file_found))
    file.copy(file_found, destination_path)
  }
}
