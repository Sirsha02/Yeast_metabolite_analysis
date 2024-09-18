library(data.table)
library(dplyr)

# Load the data
filee <- "C:/Users/ASUS/Desktop/Finale/8. JOINING/2_low/negative/all_data.csv"
df <- fread(filee)

# Identify columns that contain 'structurePerIdRank' in their names
structure_columns <- grep("structurePerIdRank", names(df), value = TRUE)

# Function to find the minimum non-NA value across specified columns
find_min_non_na <- function(df, columns) {
  df %>%
    rowwise() %>%
    mutate(min_value = min(c_across(all_of(columns)), na.rm = TRUE)) %>%
    ungroup()
}

# Function to process each group
process_group <- function(group_df, columns) {
  # Compute the minimum non-NA values for each row
  group_df <- find_min_non_na(group_df, columns)
  
  # Filter rows where the value matches the minimum non-NA value
  min_value <- group_df %>%
    summarise(min_value = min(min_value, na.rm = TRUE)) %>%
    pull(min_value)
  
  # Check if there is at least one non-NA minimum value
  if (!is.na(min_value)) {
    group_df <- group_df %>%
      filter(min_value == min_value) %>%
      slice(1)  # Keep only the first occurrence of the minimum value
  }
  
  return(group_df)
}

# Group by 'id' and process each group
df_min_per_group <- df %>%
  group_by(ID) %>%
  do(process_group(., structure_columns)) %>%
  ungroup()


df22<- df_min_per_group %>% 
  select(ID,contains('name'))


# Define the output file path
output_file <- "C:/Users/ASUS/Desktop/Finale/8. JOINING/2_low/negative/processed_data.csv"

# Save the processed data to a CSV file
fwrite(df_min_per_group, file = output_file)

# Print confirmation message
cat("Processed data saved to", output_file, "\n")




# KEEPING MULTIPLE MASSBANK ENTRIES

library(data.table)
library(dplyr)

# Load the data
filee <- "C:/Users/ASUS/Desktop/Finale/8. JOINING/2_low/positive/all_data.csv"
df <- fread(filee)

# Print the first few rows of the data for inspection
print(head(df))

# Identify columns that contain 'structurePerIdRank' in their names
structure_columns <- grep("structurePerIdRank", names(df), value = TRUE)

# Print the columns of interest
print(structure_columns)

# Function to find the minimum non-NA value across specified columns
find_min_non_na <- function(df, columns) {
  df %>%
    rowwise() %>%
    mutate(min_value = min(c_across(all_of(columns)), na.rm = TRUE)) %>%
    ungroup()
}

# Function to process each group
process_group <- function(group_df, columns) {
  group_df %>%
    rowwise() %>%
    mutate(min_value = find_min_non_na(group_df, columns) %>%
             filter(row_number() == 1) %>% pull(min_value)) %>%
    ungroup() %>%
    filter(across(all_of(columns), ~ . == min_value | is.na(.)))
}

# Group by 'id' and process each group
df_min_per_group <- df %>%
  group_by(ID) %>%
  do(process_group(., structure_columns)) %>%
  ungroup()

# Define the output file path
output_file <- "C:/Users/ASUS/Desktop/Finale/8. JOINING/2_low/positive/processed_data_multiple.csv"

# Save the processed data to a CSV file
fwrite(df_min_per_group, file = output_file)

# Print confirmation message
cat("Processed data saved to", output_file, "\n")


