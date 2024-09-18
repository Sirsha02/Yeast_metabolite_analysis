
#1. Seperating pos and neg from main file

filename_mz <- "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mzcloud_main.xlsx"


df_pos <- read_excel(filename_mz, sheet = 1) %>%
  filter(adduct != '[M-H]-1', adduct != '[M-H-H2O]-1')%>%
  select(Name, RT, mz, Formula,contains('Search') ,adduct, MS2,contains('area')) %>%
  #filter(Name != 'FALSE', Name != 'Checked') %>%
  rename(MZ = mz) %>%
  mutate(across(c(MZ, RT), as.numeric)) %>%  # Ensure MZ and RT are numeric
  mutate(Formula = gsub(" ", "", Formula))  # Remove all spaces in Formula column

### mzCloud Results# mzCloud Results# mzCloud Results# mzCloud Results
# Read the Excel file and filter the data
df_neg <- read_excel(filename_mz, sheet = 1) %>%
  filter(adduct == '[M-H]-1'| adduct == '[M-H-H2O]-1')%>%
  select(Name, RT, mz, Formula,contains('Search') ,adduct, MS2,contains('area')) %>%
  #filter(Name != 'FALSE', Name != 'Checked') %>%
  rename(MZ = mz) %>%
  mutate(across(c(MZ, RT), as.numeric)) %>%  # Ensure MZ and RT are numeric
  mutate(Formula = gsub(" ", "", Formula))  # Remove all spaces in Formula column


# Save the result to a CSV file
output_file <- "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mzcloud_pos.csv"
fwrite(df_pos, output_file)

output_file <- "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mzcloud_neg.csv"
fwrite(df_neg, output_file)



#2. Comparing sirius by formula and mass

# Define the file paths for the two CSV files
csv_file1 <-  "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mzcloud_pos.csv"

# Read the first CSV file
df1 <- fread(csv_file1)

# Read and filter the data
structures_all_raw <- fread(structure_identifications_all_p) %>%
  filter(structurePerIdRank <= 10)%>%
  mutate(RT = retentionTimeInSeconds / 60) %>%
  mutate(RT = round(RT, 6))

df2 <- structures_all_raw%>%
  rename(Formula = molecularFormula)

# Perform a left join by 'Formula' column
merged_data <- df1 %>%
  left_join(df2, by = "Formula", relationship = 'many-to-many')


# Function to calculate the absolute ppm difference
calculate_ppm <- function(mz1, mz2) {
  abs((mz1 - mz2) / mz2 * 1e6)
}

filter_df <- merged_data%>%
  filter(calculate_ppm(ionMass, MZ) < 10, abs(RT.x - RT.y) < 0.3) 



# Save the filtered data to a CSV file
output_file <- "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mz_sirius_pos.csv"
fwrite(filter_df, output_file)


# 3. Merging with Comptox data

# Define the file paths for the two CSV files
csv_file1 <-  "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mzcloud_pos.csv"
csv_file2 <- "C:/Users/ASUS/Desktop/Finale/7.mzcloud/Comptox_pos.csv"

# Read the first CSV file
df1 <- fread(csv_file1)

# Read the second CSV file
df2 <- fread(csv_file2, fill = TRUE)%>%
  rename('Formula' = 'MOLECULAR FORMULA')

# Perform a left join by 'Formula' column
merged_data <- df1 %>%
  full_join(df2, by = "Formula", relationship = 'many-to-many')


# Save the result to a CSV file
output_file <- "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mz_compt_pos.csv"
fwrite(merged_data, output_file)


#4. Comparing with sirius by inchikey

# Define the file paths for the two CSV files
csv_file1 <- "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mz_compt_pos.csv"

# Read the first CSV file
df1 <- fread(csv_file1)

# Read and filter the data
structures_all_raw <- fread(structure_identifications_all_p) %>%
  mutate(RT = retentionTimeInSeconds / 60) %>%
  mutate(RT = round(RT, 6))
df2 <- structures_all_raw

# Convert both columns to character to ensure consistency
df1$INCHIKEY <- as.character(df1$INCHIKEY)
df2$InChIkey2D <- as.character(df2$InChIkey2D)

# Initialize an empty list to store matched dataframes
matched_data_list <- list()

# Loop over each value in df2$InChIkey2D
for (key in df2$InChIkey2D) {
  # Check if the key is present in any value of df1$INCHIKEY
  matching_indices <- grep(key, df1$INCHIKEY)
  if (length(matching_indices) > 0) {
    matched_data <- cbind(df1[matching_indices, ], df2[df2$InChIkey2D == key, ])
    matched_data_list[[length(matched_data_list) + 1]] <- matched_data
  }
}

#Combine all matched dataframes into one
if (length(matched_data_list) > 0) {
  matched_rows <- do.call(rbind, matched_data_list)
} else {
  matched_rows <- data.frame()  # if no matches found, create an empty dataframe
}

# Print the matched rows
print(matched_rows)

# Save the result to a CSV file
output_file <- "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mz_compt_sirius_pos.csv"
fwrite(matched_rows, output_file)

#5. Comparing with mzmine by mass and rt


# Define the file paths for the two CSV files
csv_file1 <-  "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mzcloud_neg.csv"

# Read the first CSV file
df1 <- fread(csv_file1)

df2 <- fread(ms2_all_neg)%>%
  select(id, rt, mz) %>%
  mutate(across(c(mz, rt), as.numeric))

# Function to calculate the absolute ppm difference
calculate_ppm <- function(mz1, mz2) {
  abs((mz1 - mz2) / mz2 * 1e6)
}

# Create all possible combinations of rows from both dataframes
combined_df <- crossing(df2, df1)

# Filter based on the conditions
result_df <- combined_df %>%
  filter(calculate_ppm(mz, MZ) < 1, abs(rt - RT) < 0.1) %>%
  select(Name, id, Formula, everything())

# Save the filtered data to a CSV file
output_file <- "C:/Users/ASUS/Desktop/Finale/7.mzcloud/mzcloud_mzmine_mzrt_neg.csv"
fwrite(result_df, output_file)
