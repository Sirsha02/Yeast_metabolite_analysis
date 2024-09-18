
# 1. INCHIKEY MATCH
our_data <- structures_all_raw
targeted_data <- read_excel(metabolite_adductmass_details)

# Convert both columns to character to ensure consistency
targeted_data$InChiKey <- as.character(targeted_data$InChiKey)
our_data$InChIkey2D <- as.character(our_data$InChIkey2D)

# Initialize an empty vector to store the matched values
matched_keys <- c()

# Loop over each value in our_data$InChIkey2D
for (key in our_data$InChIkey2D) {
  # Check if the key is present in any value of felix_data$InChiKey
  if (any(grepl(key, targeted_data$InChiKey))) {
    matched_keys <- c(matched_keys, key)  # Store the matched key
  }
}

# Filter our_data based on matched keys
matched_rows <- our_data[our_data$InChIkey2D %in% matched_keys, ]

write.csv(matched_rows, "C:/Users/ASUS/Desktop/Finale/inchikey_matches_neg.csv", row.names = FALSE)


#2. MASS MATCH

# Define file paths
file1_path <- ms2_all_pos
file2_path <- ms2_all_neg
file3_path <- metabolite_adductmass_details

# Read specific columns from the first CSV file (positive mode)
file1_data <- read_csv(file1_path, col_types = cols_only(id = col_character(), rt = col_double(), mz = col_double())) %>%
  select(id, rt, mz) %>%
  mutate(mode = "pos")

# Read specific columns from the second CSV file (negative mode)
file2_data <- read_csv(file2_path, col_types = cols_only(id = col_character(), rt = col_double(), mz = col_double())) %>%
  select(id, rt, mz) %>%
  mutate(mode = "neg")

# Read specific columns from the third Excel file, excluding Sum Formula
file3_data <- read_excel(file3_path) %>%
  select(Compound, `m+h`, `m-h`, `m+na`, isomeric_SMILES)

# Function to calculate absolute ppm differences
calculate_ppm <- function(mz_value, m_value) {
  abs(((m_value - mz_value) / m_value) * 1e6)
}

# Initialize empty list to store results
results_list <- list()

# Process file1_data with m+h and m+na
for (i in 1:nrow(file1_data)) {
  id <- file1_data$id[i]
  mz <- file1_data$mz[i]
  rt <- file1_data$rt[i]
  
  for (m_col in c("m+h", "m+na")) {
    m_values <- file3_data[[m_col]]
    compound_names <- file3_data$Compound
    smiles <- file3_data$isomeric_SMILES
    
    # Calculate ppm differences
    ppm_diff <- calculate_ppm(mz, m_values)
    
    # Create result data frame
    result_df <- data.frame(
      id = id,
      mz = mz,
      rt = rt,
      mode = "pos",
      Compound = compound_names,
      SMILES = smiles,
      m_col = m_col,
      m_value = m_values,
      ppm_diff = ppm_diff
    )
    
    # Append result to results_list
    results_list <- append(results_list, list(result_df))
  }
}

# Process file2_data with m-h
for (i in 1:nrow(file2_data)) {
  id <- file2_data$id[i]
  mz <- file2_data$mz[i]
  rt <- file2_data$rt[i]
  
  m_col <- "m-h"
  m_values <- file3_data[[m_col]]
  compound_names <- file3_data$Compound
  smiles <- file3_data$isomeric_SMILES
  
  # Calculate ppm differences
  ppm_diff <- calculate_ppm(mz, m_values)
  
  # Create result data frame
  result_df <- data.frame(
    id = id,
    mz = mz,
    rt = rt,
    mode = "neg",
    Compound = compound_names,
    SMILES = smiles,
    m_col = m_col,
    m_value = m_values,
    ppm_diff = ppm_diff
  )
  
  # Append result to results_list
  results_list <- append(results_list, list(result_df))
}

# Combine all results into a single data frame
results_df <- do.call(rbind, results_list)

# Pivot the data to have separate columns for m_col and ppm_diff
results_df <- results_df %>%
  pivot_wider(
    id_cols = c(id, mz, rt, mode, Compound, SMILES),
    names_from = m_col,
    values_from = c(m_value, ppm_diff)
  )

# Replace ppm values greater than 5 with NA
ppm_cols <- grep("ppm_diff_", names(results_df))
results_df[ppm_cols] <- lapply(results_df[ppm_cols], function(x) ifelse(abs(x) > 5, NA, x))

# Filter rows where at least one ppm_diff column is not NA
filtered_results_df <- results_df[rowSums(!is.na(results_df[ppm_cols])) > 0, ]

# Write the final results to a CSV file
write.csv(filtered_results_df, "C:/Users/ASUS/Desktop/Finale/targeted_matches_all.csv", row.names = FALSE)
