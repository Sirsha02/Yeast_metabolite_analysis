# Define the main directory where .github and .scripts folders are located
main_directory <- "C:/Users/ASUS/Desktop/Finale/4.Massbank/MassBank-data-2024.06/"

# List all subdirectories except .github and .scripts
subdirectories <- list.files(main_directory, full.names = TRUE)
subdirectories <- subdirectories[!basename(subdirectories) %in% c(".github", ".scripts")]

# Loop through subdirectories 19 to 30
for (i in 56:60) {
  subdir <- subdirectories[i]
  output_directory <- paste0("C:/Users/ASUS/Desktop/Finale/4.Massbank/Dataframes/", i)  # Output directory for each subdirectory
  
  # List all text files in the current subdirectory
  files <- list.files(subdir, pattern = "\\.txt$", full.names = TRUE)
  
  # Initialize list to store dataframes for each text file
  all_dataframes <- list()
  
  # Function to parse text file and create dataframe
  parse_text_file <- function(file_path) {
    text_data <- readLines(file_path)
    
    # Initialize variables to store parsed information
    accession <- NA
    ch_names <- character(0)
    ch_formula <- NA
    ch_exact_mass <- NA
    ch_smiles <- NA
    ch_inchi <- NA
    ch_inchikey <- NA
    ac_mass_spectrometry <- NA
    ac_ion_mode <- NA
    ac_collision_energy <- NA
    ms_precursor_mz <- NA
    ms_precursor_type <- NA
    pk_peak <- list()
    
    # Initialize a flag to indicate whether we are inside the PK$PEAK section
    inside_pk_peak <- FALSE
    
    # Parse the text data
    for (line in text_data) {
      if (grepl("^ACCESSION:", line)) {
        accession <- sub("ACCESSION: MSBNK-\\w+-", "", line)
      } else if (grepl("^CH\\$NAME:", line)) {
        ch_names <- c(ch_names, sub("CH\\$NAME: ", "", line))
      } else if (grepl("^CH\\$FORMULA:", line)) {
        ch_formula <- sub("CH\\$FORMULA: ", "", line)
      } else if (grepl("^CH\\$EXACT_MASS:", line)) {
        ch_exact_mass <- sub("CH\\$EXACT_MASS: ", "", line)
      } else if (grepl("^CH\\$SMILES:", line)) {
        ch_smiles <- sub("CH\\$SMILES: ", "", line)
      } else if (grepl("^CH\\$IUPAC:", line)) {
        ch_inchi <- sub("CH\\$IUPAC: ", "", line)
      } else if (grepl("^CH\\$LINK: INCHIKEY", line)) {
        ch_inchikey <- sub("CH\\$LINK: INCHIKEY ", "", line)
      } else if (grepl("^AC\\$MASS_SPECTROMETRY: MS_TYPE", line)) {
        ac_mass_spectrometry <- sub("AC\\$MASS_SPECTROMETRY: MS_TYPE ", "", line)
      } else if (grepl("^AC\\$MASS_SPECTROMETRY: ION_MODE", line)) {
        ac_ion_mode <- sub("AC\\$MASS_SPECTROMETRY: ION_MODE ", "", line)
      } else if (grepl("^AC\\$MASS_SPECTROMETRY: COLLISION_ENERGY", line)) {
        ac_collision_energy <- sub("AC\\$MASS_SPECTROMETRY: COLLISION_ENERGY ", "", line)
      } else if (grepl("^MS\\$FOCUSED_ION: PRECURSOR_M/Z", line)) {
        ms_precursor_mz <- sub("MS\\$FOCUSED_ION: PRECURSOR_M/Z ", "", line)
      } else if (grepl("^MS\\$FOCUSED_ION: PRECURSOR_TYPE", line)) {
        ms_precursor_type <- sub("MS\\$FOCUSED_ION: PRECURSOR_TYPE ", "", line)
      } else if (grepl("^PK\\$PEAK:", line)) {
        inside_pk_peak <- TRUE
      } else if (inside_pk_peak && grepl("^\\s{2,}", line)) {
        pk_peak <- c(pk_peak, line)
      } else if (inside_pk_peak && grepl("^//", line)) {
        inside_pk_peak <- FALSE
      }
    }
    
    # Check if pk_peak is empty or not properly parsed
    if (length(pk_peak) == 0) {
      stop("Error: No PK$PEAK data found in file ", file_path)
    }
    
    # Extract peak data into separate vectors
    mz <- numeric()
    intensity <- numeric()
    rel_intensity <- numeric()
    
    for (peak in pk_peak) {
      parts <- strsplit(trimws(peak), " ")[[1]]
      mz <- c(mz, as.numeric(parts[1]))
      intensity <- c(intensity, as.numeric(parts[2]))
      rel_intensity <- c(rel_intensity, as.numeric(parts[3]))
    }
    
    # Create a dataframe
    df <- data.frame(
      Accession = accession,
      Name = paste(ch_names, collapse = ", "),
      Formula = ch_formula,
      Exact_Mass = ch_exact_mass,
      SMILES = ch_smiles,
      InChI = ch_inchi,
      InChIKey = ch_inchikey,
      Mass_Spectrometry = ac_mass_spectrometry,
      Ion_Mode = ac_ion_mode,
      Collision_Energy = ac_collision_energy,
      Precursor_MZ = ms_precursor_mz,
      Precursor_Type = ms_precursor_type,
      Peak_MZ = paste(mz, collapse = ", "),
      Peak_Intensity = paste(intensity, collapse = ", "),
      Peak_Rel_Intensity = paste(rel_intensity, collapse = ", "),
      stringsAsFactors = FALSE
    )
    
    return(df)
  }
  
  # Process each text file in the current subdirectory
  for (file in files) {
    # Parse text file and create dataframe
    df <- parse_text_file(file)
    
    # Store dataframe in the list
    all_dataframes[[basename(file)]] <- df
  }
  
  # Ensure the output directory exists or create it
  if (!file.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  
  # Save each dataframe to a CSV file in the output directory
  for (key in names(all_dataframes)) {
    write.csv(all_dataframes[[key]], file = file.path(output_directory, paste0(key, ".csv")), row.names = FALSE)
  }
}
