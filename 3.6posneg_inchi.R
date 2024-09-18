
# Read and filter the data for negative mode
structures_all_raw_neg <- fread(structure_identifications_all_n) %>%
  #filter(structurePerIdRank <= 10) %>%
  mutate(RT = retentionTimeInSeconds / 60) %>%
  mutate(RT = round(RT, 6))
our_data_neg <- structures_all_raw_neg

# Read and filter the data for positive mode
structures_all_raw_pos <- fread(structure_identifications_all_p) %>%
  #filter(structurePerIdRank <= 10) %>%
  mutate(RT = retentionTimeInSeconds / 60) %>%
  mutate(RT = round(RT, 6))
our_data_pos <- structures_all_raw_pos

# Convert both columns to character to ensure consistency
our_data_pos$InChIkey2D <- as.character(our_data_pos$InChIkey2D)
our_data_neg$InChIkey2D <- as.character(our_data_neg$InChIkey2D)

# Perform an inner join on the InChIkey2D column to get the matched rows
matched_rows <- inner_join(our_data_pos, our_data_neg, by = "InChIkey2D", suffix = c("_pos", "_neg"), relationship = 'many-to-many')

# Print the matched rows
print(matched_rows)

# Write the matched rows to a CSV file
write.csv(matched_rows, "C:/Users/ASUS/Desktop/Finale/2.sirius exported/posVneg_inchikey_allrank.csv", row.names = FALSE)
