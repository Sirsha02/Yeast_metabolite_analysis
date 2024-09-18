
# SIRIUS SUMMARY ---------------------------------------------------------------

# Read and filter the data
structures_all_raw_pos <- fread(structure_identifications_all_p)%>%
  filter(structurePerIdRank <= 10)  %>%
  mutate(RT = retentionTimeInSeconds / 60)%>%
  mutate(RT = round(RT, 6))

sirius_structure_summary_pos <- structures_all_raw_pos  %>%
  select(mappingFeatureId) %>%
  distinct()

sirius_structure_summary_pos  #77 & 159

#write.csv(structures_all_raw, "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/pos/structure_summary.csv", row.names = FALSE)

# Read and filter the data
structures_all_raw_neg <- fread(structure_identifications_all_n)%>%
  filter(structurePerIdRank <= 10)  %>%
  mutate(RT = retentionTimeInSeconds / 60)%>%
  mutate(RT = round(RT, 6))

sirius_structure_summary_neg <- structures_all_raw_neg  %>%
  select(mappingFeatureId) %>%
  distinct()

sirius_structure_summary_neg  #77 & 159

#write.csv(structures_all_raw, "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/pos/structure_summary.csv", row.names = FALSE)

#READING SIRIUS FILE [need update] ------------------------------------------------------------

# Initialize vectors to store the extracted data
feature_ids <- c()
pepmasses <- c()
rtinseconds <- c()
charge <- c()
filenames <- c()

# Function to parse the MGF file
parse_mgf <- function(file_path) {
  lines <- readLines(file_path)
  
  # Variables to hold temporary data
  current_block <- list()
  inside_block <- FALSE
  
  for (line in lines) {
    line <- trimws(line)
    
    if (line == "BEGIN IONS") {
      inside_block <- TRUE
      current_block <- list()
      
    } else if (line == "END IONS") {
      if (inside_block && current_block$MSLEVEL == "2") {
        # Append the extracted data to the vectors
        feature_ids <<- c(feature_ids, current_block$FEATURE_ID)
        pepmasses <<- c(pepmasses, current_block$PEPMASS)
        rtinseconds <<- c(rtinseconds, current_block$RTINSECONDS)
        filenames <<- c(filenames, current_block$FILENAME)
        charge <<- c(charge, current_block$CHARGE)
      }
      inside_block <- FALSE
      
    } else if (inside_block) {
      if (grepl("=", line)) {
        key_value <- strsplit(line, "=", fixed = TRUE)[[1]]
        key <- key_value[1]
        value <- key_value[2]
        current_block[[key]] <- value
      }
    }
  }
  
  # Create a data frame from the vectors
  df <- data.frame(
    feature_id = feature_ids,
    pepmass = pepmasses,
    rtinseconds = rtinseconds,
    filename = filenames,
    charge = charge,
    stringsAsFactors = FALSE
  )
  
  return(df)
}

# Parse the MGF file
mgf_file <- "C:/Users/ASUS/Desktop/Final/CSV/sirius_4pos.mgf"
mzmine_exported_sirius <- parse_mgf(mgf_file)

write.csv(mzmine_exported_sirius, "C:/Users/ASUS/Desktop/Final/mzmine_sirius_pos.csv", row.names = FALSE)

