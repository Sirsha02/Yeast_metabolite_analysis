
df1 <- fread("C:/Users/ASUS/Desktop/Finale/6.massbank/output_pos/mass_match_combined_pos.csv")%>%
  select(-contains('Accession'), - contains('ppm'), - contains('peak'), - contains('collision_energy')) %>% 
  filter(Matched_Count > 1) %>% 
  distinct()

# # Path to the output CSV file
# output_file <- "C:/Users/ASUS/Desktop/Finale/6.massbank/massbnk_match_pos.csv"
# 
# # Write the result to the output CSV file
# write_csv(df1, output_file)
# 
# 

df2 <- fread(structure_identifications_all_n)%>%
  filter(structurePerIdRank <= 10)  %>%
  mutate(RT = retentionTimeInSeconds / 60)%>%
  mutate(RT = round(RT, 6))%>%
  rename(ID = mappingFeatureId)


# Convert both columns to character to ensure consistency
df1$InChIKey <- as.character(df1$InChIKey)
df2$InChIkey2D <- as.character(df2$InChIkey2D)

# Initialize an empty list to store matched dataframes
matched_data_list <- list()

# Loop over each value in df2$InChIkey2D
for (key in df2$InChIkey2D) {
  # Check if the key is present in any value of df1$INCHIKEY
  matching_indices <- grep(key, df1$InChIKey)
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


# Path to the output CSV file
output_file <- "C:/Users/ASUS/Desktop/Finale/6.massbank/massbnk_sirius_inchi_neg.csv"

# Write the result to the output CSV file
write_csv(matched_rows, output_file)


