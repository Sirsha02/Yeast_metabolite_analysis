#SIRIUS file given by MZmine ---------------------------------------------------

# Read the text file
text_data <- readLines(sirius_neg)

# Initialize variables
entries <- list()
current_entry <- NULL
inside_entry <- FALSE

# Parse the text data
for (line in text_data) {
  if (grepl("^BEGIN IONS", line)) {
    current_entry <- list()
    inside_entry <- TRUE
    current_entry$peaks <- numeric()
    current_entry$intensity <- numeric()
  } else if (grepl("^END IONS", line)) {
    inside_entry <- FALSE
    if (!is.null(current_entry) && current_entry$MSLEVEL == 2) {
      entries <- append(entries, list(current_entry))
    }
    current_entry <- NULL
  } else if (inside_entry) {
    if (grepl("^FEATURE_ID=", line)) {
      current_entry$FEATURE_ID <- as.numeric(sub("FEATURE_ID=", "", line))
    } else if (grepl("^MSLEVEL=", line)) {
      current_entry$MSLEVEL <- as.numeric(sub("MSLEVEL=", "", line))
    } else if (grepl("^RTINSECONDS=", line)) {
      current_entry$RTINSECONDS <- as.numeric(sub("RTINSECONDS=", "", line))
    } else if (grepl("^PEPMASS=", line)) {
      current_entry$PEPMASS <- as.numeric(sub("PEPMASS=", "", line))
    } else if (grepl("^CHARGE=", line)) {
      current_entry$CHARGE <- sub("CHARGE=", "", line)
    } else if (grepl("^\\d+\\.\\d+ \\d+\\.?\\d*$", line)) {
      parts <- strsplit(line, " ")[[1]]
      current_entry$peaks <- c(current_entry$peaks, as.numeric(parts[1]))
      current_entry$intensity <- c(current_entry$intensity, as.numeric(parts[2]))
    }
  }
}

# Create and save dataframes for each entry with MSLEVEL=2
output_directory <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/sirius_neg_entries/"  # Update this path to your output directory
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)
}

for (i in seq_along(entries)) {
  entry <- entries[[i]]
  df <- data.frame(
    FEATURE_ID = entry$FEATURE_ID,
    RTINSECONDS = entry$RTINSECONDS,
    PEPMASS = entry$PEPMASS,
    CHARGE = entry$CHARGE,
    Peak_MZ = paste(entry$peaks, collapse = ","),
    Intensity = paste(entry$intensity, collapse = ","),
    stringsAsFactors = FALSE
  )
  
  # Add the RT column in minutes and round to 6 decimal places
  df <- df %>%
    mutate(RT = RTINSECONDS / 60) %>%
    mutate(RT = round(RT, 6))
  
  # Save the dataframe to a CSV file
  output_file <- file.path(output_directory, paste0(i, ".csv"))
  write.csv(df, file = output_file, row.names = FALSE)
}
