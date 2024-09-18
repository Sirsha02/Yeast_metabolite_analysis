#[NEED TO SEE IF ANY FILE HAS MULTIPLE ID COLUMNS, RECTIFY MANUALLY]


# Set the working directory
setwd("C:/Users/ASUS/Desktop/Finale/8. JOINING/2_low")  # Replace with the actual path to your directory

# List all files in the directory, excluding directories
all_files <- list.files()
all_files <- all_files[!file.info(all_files)$isdir]

# Select files containing 'neg' in their names and that are CSV or Excel files
neg_files <- grep("pos", all_files, value = TRUE)
neg_files <- neg_files[grepl("\\.(csv|xlsx)$", neg_files)]

# Function to process each file
process_file <- function(file_name) {
  tryCatch({
    if (grepl("\\.csv$", file_name)) {
      # Read CSV file
      data <- read_csv(file_name)
    } else if (grepl("\\.xlsx$", file_name)) {
      # Read Excel file
      data <- read_excel(file_name)
    }
    
    # Ensure 'ID' column is consistent
    names(data) <- gsub("mappingFeatureId", "ID", names(data))
    names(data) <- gsub("id", "ID", names(data))
    
    # Extract the prefix from the file name
    prefix <- unlist(strsplit(file_name, "_"))[1]
    
    # Handle multiple ID columns
    id_columns <- grep("^ID$", names(data))
    if (length(id_columns) > 1) {
      # Keep the first ID column and prefix others
      other_id_columns <- id_columns[-1]
      new_names <- names(data)
      new_names[other_id_columns] <- paste0(prefix, "_", new_names[other_id_columns])
      names(data) <- new_names
    }
    
    # Prefix column names except 'ID'
    new_names <- ifelse(names(data) == "ID", "ID", paste0(prefix, "_", names(data)))
    colnames(data) <- new_names
    
    return(data)
  }, error = function(e) {
    message("Error reading file: ", file_name)
    return(NULL)
  })
}

# Process all selected files
processed_files <- lapply(neg_files, process_file)

# Ensure all dataframes have unique column names before joining
for (i in seq_along(processed_files)) {
  df <- processed_files[[i]]
  if (!is.null(df)) {
    colnames(df) <- make.unique(colnames(df))
    processed_files[[i]] <- df
  }
}

# Perform full joins on the 'ID' column
joined_data <- Reduce(function(x, y) full_join(x, y, by = "ID", relationship = "many-to-many"), processed_files)

# Set the output directory
output_dir <- "C:/Users/ASUS/Desktop/Finale/8. JOINING/2_low/positive/"  # Replace with the actual path to your output directory

# Create the output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Save the combined data to a single CSV file
all_data_file <- paste0(output_dir, "all_data.csv")
write.csv(joined_data, all_data_file, row.names = FALSE)

# Function to save each unique ID's data to separate CSV files
save_unique_id_dataframes <- function(data, output_dir) {
  unique_ids <- unique(data$ID)
  
  for (id in unique_ids) {
    id_data <- filter(data, ID == id)
    
    # Remove columns with only NA values
    id_data <- id_data[, colSums(is.na(id_data)) < nrow(id_data)]
    
    file_name <- paste0(output_dir, "/", id, ".csv")
    write.csv(id_data, file_name, row.names = FALSE)
  }
}

# Save each unique ID's data to separate CSV files
save_unique_id_dataframes(joined_data, output_dir)
