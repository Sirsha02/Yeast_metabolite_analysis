# Load necessary libraries
library(dplyr)
library(parallel)
library(readr)

# Function to read CSV files in parallel with messages
read_csv_parallel <- function(file_paths) {
  num_cores <- detectCores() - 1  # Use one less than the total number of cores
  cl <- makeCluster(num_cores)
  clusterEvalQ(cl, {
    library(readr)  # Ensure readr is loaded on all nodes
  })
  
  message("Reading ", length(file_paths), " CSV files...")
  data_list <- parLapply(cl, file_paths, function(file) {
    message("Reading file: ", basename(file))
    readr::read_csv(file, col_types = cols())  # Explicitly call read_csv from readr package
  })
  stopCluster(cl)
  message("All CSV files read successfully.")
  return(data_list)
}

# Define the directories and output file for dir1
dir1 <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/sirius_pos_entries/"
# Read all CSV files from dir1 in parallel
csv_files_dir1 <- list.files(dir1, pattern = "\\.csv$", full.names = TRUE)
data_dir1 <- read_csv_parallel(csv_files_dir1)

# Function to calculate ppm difference
ppm_diff <- function(mass1, mass2) {
  abs((mass1 - mass2) / mass2 * 1e6)
}

# Define the base directory for Dataframes
base_dir <- "C:/Users/ASUS/Desktop/Finale/6.Massbank/Dataframes/"

# Generate directory paths from Dataframes/1 to Dataframes/10
# Specify the range of directories to read
dataframe_dirs <- lapply(1:59, function(i) file.path(base_dir, paste0(i)))  # Example range 1 to 4

# Read all CSV files from the specified directories in parallel
data_dir_list <- lapply(dataframe_dirs, function(dir) {
  csv_files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  read_csv_parallel(csv_files)
})

# Flatten the list of lists into a single list of dataframes
data_dir2 <- unlist(data_dir_list, recursive = FALSE)






# Initialize indices of data_dir2 files you want to use
indices_to_use <- 1:20000

# Initialize an empty list to store matching entries
matching_entries <- list()

# Compare each entry of dir1 with each selected entry of dir2
for (i in seq_along(data_dir1)) {
  entry_dir1 <- data_dir1[[i]]
  
  for (index in indices_to_use) {
    entry_dir2 <- data_dir2[[index]]
    
    # Ensure both pepmass and precursor_mz are not NA or NULL
    if (!is.na(entry_dir1$PEPMASS[1]) && !is.na(entry_dir2$Precursor_MZ[1])) {
      # Convert PEPMASS and Precursor_MZ to numeric if they are not already
      pepmass <- as.numeric(entry_dir1$PEPMASS[1])
      precursor_mz <- as.numeric(entry_dir2$Precursor_MZ[1])
      
      # Check if conversion to numeric was successful
      if (!is.na(pepmass) && !is.na(precursor_mz)) {
        # Calculate ppm difference in PEPMASS and PRECURSOR_M/Z
        diff_pepmass_precursor_mz <- ppm_diff(pepmass, precursor_mz)
        
        if (diff_pepmass_precursor_mz < 15) {
          # Compare the peak_mz columns
          peak_mz_dir1 <- as.numeric(strsplit(as.character(entry_dir1$Peak_MZ[1]), ",")[[1]])
          peak_mz_dir2 <- as.numeric(strsplit(as.character(entry_dir2$Peak_MZ[1]), ",")[[1]])
          matched_peak_mz <- c()
          ppm_peak <- c()
          
          for (mz1 in peak_mz_dir1) {
            for (mz2 in peak_mz_dir2) {
              diff_peak <- ppm_diff(mz1, mz2)
              if (diff_peak < 100) {
                matched_peak_mz <- c(matched_peak_mz, mz1)
                ppm_peak <- c(ppm_peak, diff_peak)
              }
            }
          }
          
          if (length(matched_peak_mz) > 0) {
            # Combine the data, ensuring all columns are characters
            entry_dir1_combined <- entry_dir1 %>%
              summarise(
                ID = as.character(first(FEATURE_ID)),
                PEPMASS = as.character(first(PEPMASS)),
                CHARGE = as.character(first(CHARGE)),
                Peak_MZ_dir1 = as.character(first(Peak_MZ)),
                Intensity_dir1 = as.character(first(Intensity)),
                RT = as.character(first(RT))
              )
            
            entry_dir2_combined <- entry_dir2 %>%
              summarise(
                Accession = as.character(first(Accession)),
                Name = paste(Name, collapse = ", "),
                Formula = as.character(first(Formula)),
                Exact_Mass = as.character(first(Exact_Mass)),
                SMILES = as.character(first(SMILES)),
                InChI = as.character(first(InChI)),
                InChIKey = as.character(first(InChIKey)),
                Mass_Spectrometry = as.character(first(Mass_Spectrometry)),
                Ion_Mode = as.character(first(Ion_Mode)),
                Collision_Energy = as.character(first(Collision_Energy)),
                Precursor_MZ = as.character(first(Precursor_MZ)),
                Precursor_Type = as.character(first(Precursor_Type)),
                Peak_MZ = paste(Peak_MZ, collapse = ", "),
                Peak_Intensity = paste(Peak_Intensity, collapse = ", "),
                Peak_Rel_Intensity = paste(Peak_Rel_Intensity, collapse = ", ")
              )
            
            # Add matched peak_mz information
            matched_info <- tibble(
              Matched_Peak_MZ = paste(matched_peak_mz, collapse = ","),
              Matched_Count = as.character(length(matched_peak_mz)),
              ppm = as.character(diff_pepmass_precursor_mz),
              ppm_peak = paste(ppm_peak, collapse = ",")
            )
            
            # Combine all data
            combined_entry <- bind_cols(entry_dir1_combined, entry_dir2_combined, matched_info)
            
            # Add to the list of matching entries
            matching_entries <- append(matching_entries, list(combined_entry))
          }
        }
      }
    }
  }
}

# Combine all data frames into a single data frame
final_df <- bind_rows(matching_entries)

# Output file path for the combined CSV
output_file <- "C:/Users/ASUS/Desktop/Finale/6.Massbank/output_pos/1.csv"

# Write the combined data frame to a CSV file
write_csv(final_df, output_file)

# Print message upon completion
cat("Combined CSV files saved to:", output_file, "\n")






#---------------------------------------------------------------------------------

library(dplyr)
library(readr)

# Directory containing CSV files
csv_directory <- "C:/Users/ASUS/Desktop/Finale/6.Massbank/output_pos/"

# List all CSV files in the directory
csv_files <- list.files(csv_directory, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty list to store data frames
data_frames <- list()

# Read each CSV file with error handling and explicit column types
for (file in csv_files) {
  # Use tryCatch to handle potential read_csv errors
  tryCatch({
    df <- read_csv(file, col_types = cols(
      Matched_Peak_MZ = col_character(),  # Ensure Matched_Peak_MZ is character
      Peak_Intensity = col_double(),       # Example of explicitly specifying column type
      Peak_Rel_Intensity = col_character() # Ensure Peak_Rel_Intensity is character
    ))
    
    # Convert Peak_Rel_Intensity to character if it's not already
    df$Peak_Rel_Intensity <- as.character(df$Peak_Rel_Intensity)
    
    data_frames[[file]] <- df
  }, error = function(e) {
    cat("Error reading file:", file, "\n")
    print(e)  # Print the specific error message
  })
}

# Combine all data frames into a single data frame
combined_df <- bind_rows(data_frames)

# Output file path for the combined CSV
output_file <- "C:/Users/ASUS/Desktop/Finale/6.Massbank/output_pos/mass_match_combined_pos.csv"

# Write the combined data frame to a CSV file
write_csv(combined_df, output_file)

# Print message upon completion
cat("Combined CSV files saved to:", output_file, "\n")

