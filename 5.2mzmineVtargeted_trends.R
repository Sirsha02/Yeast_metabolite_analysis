
# STEP 1 : READING TARGETED FILES

# Define the file path and sheet name
file_path <- amino_acid_file
sheet_name <- "fig2D_wt_semeco_extraAA_U_raw" # replace "Sheet1" with the actual sheet name

# Read the data from the specified sheet in the Excel file
data <- read_excel(file_path, sheet = sheet_name)

# Calculate average and standard deviation
agg_data <- data %>%
  group_by(metabolite, timeday, Strain) %>%
  summarize(
    avg_conc = mean(conc, na.rm = TRUE),
    stdev = sd(conc, na.rm = TRUE)
  ) %>%
  ungroup()

# Pivot the data to create the desired columns
pivoted_data <- agg_data %>%
  pivot_wider(
    names_from = timeday,
    values_from = c(avg_conc, stdev),
    names_prefix = "t",
    names_sep = "_"
  ) %>%
  arrange(metabolite, Strain)

# Rename columns to desired format
colnames(pivoted_data) <- sub("avg_conc_", "mean_", colnames(pivoted_data))
colnames(pivoted_data) <- sub("stdev_", "stdev_", colnames(pivoted_data))

# Add trend analysis columns
pivoted_data <- pivoted_data %>%
  mutate(
    trend_t1_t2 = case_when(
      mean_t1 < mean_t2 ~ "+",
      mean_t1 > mean_t2 ~ "-",
      TRUE ~ "NA"
    ),
    trend_t2_t3 = case_when(
      mean_t2 < mean_t8 ~ "+",
      mean_t2 > mean_t8 ~ "-",
      TRUE ~ "NA"
    )
  )
# 
# # Define the output file path
# output_file_path <- "C:/Users/ASUS/Desktop/Final/AA_trend_data.csv"
# 
# # Save the resulting data frame to a CSV file
# write.csv(pivoted_data, output_file_path, row.names = FALSE)
# 
# 

# Define the file path and sheet name
file_path <- glycerol_intermediates_file

# Read the data from the specified sheet in the Excel file
data <- read_excel(file_path)

# Calculate average and standard deviation
agg_data2 <- data %>%
  group_by(metabolite, timeday, Strain) %>%
  summarize(
    avg_conc = mean(conc, na.rm = TRUE),
    stdev = sd(conc, na.rm = TRUE)
  ) %>%
  ungroup()

# Pivot the data to create the desired columns
pivoted_data2 <- agg_data2 %>%
  dplyr::filter(timeday %in% c(0.7, 2, 8))%>%
  pivot_wider(
    names_from = timeday,
    values_from = c(avg_conc, stdev),
    names_prefix = "t",
    names_sep = "_"
  ) %>%
  arrange(metabolite, Strain)

# Rename columns to desired format for the second file
colnames(pivoted_data2) <- sub("avg_conc_t0.7", "avg_conc_t1", colnames(pivoted_data2))
colnames(pivoted_data2) <- sub("stdev_t0.7", "stdev_t1", colnames(pivoted_data2))
colnames(pivoted_data2) <- sub("avg_conc_", "mean_", colnames(pivoted_data2))
colnames(pivoted_data2) <- sub("stdev_", "stdev_", colnames(pivoted_data2))

# Add trend analysis columns for the second file
pivoted_data2 <- pivoted_data2 %>%
  mutate(
    trend_t1_t2 = case_when(
      mean_t1 < mean_t2 ~ "+",
      mean_t1 > mean_t2 ~ "-",
      TRUE ~ "NA"
    ),
    trend_t2_t3 = case_when(
      mean_t2 < mean_t8 ~ "+",
      mean_t2 > mean_t8 ~ "-",
      TRUE ~ "NA"
    )
  )


# Join the data frames by column names (excluding "metabolite" and "Strain")
common_columns <- intersect(names(pivoted_data), names(pivoted_data2))
combined_data <- full_join(pivoted_data, pivoted_data2, by = common_columns)

# # Define the output file path for the combined data
# output_file_path_combined <- "C:/Users/ASUS/Desktop/Final/combined_AA_glycerol_data.csv"
# 
# # Save the resulting combined data frame to a CSV file
# write.csv(combined_data, output_file_path_combined, row.names = FALSE)
#

# Combine dataframes
common_columns <- intersect(names(pivoted_data), names(pivoted_data2))
combined_data <- full_join(pivoted_data, pivoted_data2, by = common_columns)

# Separate data by strain and pivot wider
combined_data_wide <- combined_data %>%
  pivot_wider(
    names_from = Strain,
    values_from = starts_with("mean_t") | starts_with("stdev_t") | starts_with("trend_"),
    names_sep = "_"
  )
# Rename trend columns
combined_data_wide <- combined_data_wide %>%
  rename_with(~ gsub("trend_t1_t2_SeMeCo", "4p_12", .), starts_with("trend_t1_t2_SeMeCo")) %>%
  rename_with(~ gsub("trend_t1_t2_MET15-SeMeCo", "M3p_12", .), starts_with("trend_t1_t2_MET15-SeMeCo")) %>%
  rename_with(~ gsub("trend_t1_t2_WT", "WT_12", .), starts_with("trend_t1_t2_WT")) %>%
  rename_with(~ gsub("trend_t2_t3_SeMeCo", "4p_23", .), starts_with("trend_t2_t3_SeMeCo")) %>%
  rename_with(~ gsub("trend_t2_t3_MET15-SeMeCo", "M3p_23", .), starts_with("trend_t2_t3_MET15-SeMeCo")) %>%
  rename_with(~ gsub("trend_t2_t3_WT", "WT_23", .), starts_with("trend_t2_t3_WT"))



# Define the output file path for the combined data
#output_file_path_combined <- "C:/Users/ASUS/Desktop/Final/combined_AA_glycerol_data_grouped.csv"

# Save the resulting combined data frame to a CSV file
#write.csv(combined_data_wide, output_file_path_combined, row.names = FALSE)


# STEP 2 : READING UNTARGETED FILES   ------------------------------------------
feature_cluster <- data_file_cluster_neg

#--------------------
# Read the CSV file containing column names to filter and their labels
columns_to_filter <- features_to_plot

# Read the CSV file into a data frame
columns_to_filter_df <- fread(columns_to_filter)

# Ensure the column names and their labels are in the correct format
columns_to_filter <- as.character(columns_to_filter_df[['neg']]) # > > > >
columns_labels <- as.character(columns_to_filter_df[['nname']])  # Assuming 'pos1' is the column with the names

# Filter the ids present in columns to filter from colnames of sample feature cluster
columns_present <- columns_to_filter[columns_to_filter %in% colnames(feature_cluster)]

# Create a named vector to map column names to their labels
column_name_label_map <- setNames(columns_labels, columns_to_filter)

# Create a data frame with filtered columns and their labels
filtered_sample_feature <- feature_cluster[, c("cluster", columns_present), drop = FALSE]

# Add corresponding labels as a new row
label_row <- c("Metabolite", column_name_label_map[columns_present])
filtered_sample_feature <- rbind(label_row, filtered_sample_feature)

# Create a new row with the current column names
id_row <- c("id", colnames(filtered_sample_feature)[-1])
filtered_sample_feature <- rbind(id_row, filtered_sample_feature)


filtered_sample_feature[1, 1] <- "id"
# Change the name of the first column
colnames(filtered_sample_feature)[1] <- "cluster"  # Replace "NewColumnName" with your desired name

# Convert back to data frame
filtered_sample_feature <- as.data.frame(filtered_sample_feature)

peaks_t_df <- filtered_sample_feature
# Convert the cluster column to factor for grouping
peaks_t_df$cluster <- as.factor(peaks_t_df$cluster)

# Save original column names
original_column_names <- names(peaks_t_df)

# Ensure unique column names by renaming duplicates temporarily
names(peaks_t_df) <- make.unique(names(peaks_t_df))

# Assuming 'id' rows are in the first two rows
id_row <- peaks_t_df[1:2, ]

# Exclude the 'id' rows from the data (note the correct indexing)
peaks_t_df_no_id <- peaks_t_df[-c(1, 2), ]

# Calculate the column-wise average for each cluster
peak_averages <- peaks_t_df_no_id %>%
  group_by(cluster) %>%
  summarise(across(everything(), ~ mean(as.numeric(.), na.rm = TRUE), .names = "{.col}"))


# Add the 'id' row back (if necessary)
peak_averages <- rbind(id_row, peak_averages)%>%
  mutate(across(everything(), as.character))


# Move cluster to rownames and transpose
peak_averages <- as.data.frame(peak_averages)
rownames(peak_averages) <- peak_averages$cluster
peak_averages <- peak_averages %>%
  select(-cluster) %>%
  t()

# Convert to data frame
peak_averages_df <- as.data.frame(peak_averages)

# Concatenate the values of the first two columns
# peak_averages_df_concatenated <- peak_averages_df %>%
#   mutate(concatenated_col = paste(.[[2]], .[[1]], sep = "-")) %>%
#   select(concatenated_col, everything(), -c(1, 2)) # Move concatenated_col to the front
# 
# 
# 
# # Rename the first column to "Metabolite"
# names(peak_averages_df_concatenated)[1] <- "Metabolite"


# Define a function to compare columns and return the appropriate value
compare_columns <- function(col1, col2) {
  ifelse(col2 > col1, "+", ifelse(col1 > col2, "-", NA))
}

# Compare columns and create new columns using the comparison function
peak_averages_df <- peak_averages_df %>%
  rowwise() %>%
  mutate(
    WT_12 = compare_columns(`1`, `4`),
    WT_23 = compare_columns(`4`, `7`),
    WT_13 = compare_columns(`1`, `7`),
    `4p_12` = compare_columns(`2`, `5`),
    `4p_23` = compare_columns(`5`, `8`),
    `4p_13` = compare_columns(`2`, `8`),
    `M3p_12` = compare_columns(`3`, `6`),
    `M3p_23` = compare_columns(`6`, `9`),
    `M3p_13` = compare_columns(`3`, `9`)
  )


# STEP 3 : COMPARE -------------------------------------------------------------


# Assume `peak_averages_df` and `combined_data_wide` are already loaded and preprocessed
# Rename the first column to "Metabolite"
names(combined_data_wide)[1] <- "Metabolite"


# Load necessary data frames
# combined_data_wide <- read.csv("path_to_combined_data_wide.csv") # Replace with actual path
# peak_averages_df <- read.csv("path_to_peak_averages_df.csv") # Replace with actual path

# Extract Metabolite columns
combined_metabolites <- combined_data_wide %>% select(Metabolite)
peak_averages_metabolites <- peak_averages_df %>% select(Metabolite)

# Prepare a results data frame with all combinations
results <- combined_data_wide %>%
  select(Metabolite, starts_with("WT_"), starts_with("4p_"), starts_with("M3p_")) %>%
  left_join(peak_averages_df %>%
              select(id,Metabolite, starts_with("WT_"), starts_with("4p_"), starts_with("M3p_")),
            by = "Metabolite",
            suffix = c("_targeted", "_untargeted"))

# Perform row-wise mutation to compare columns and mark as 'matched'
results <- results %>%
  rowwise() %>%
  mutate(
    WT_trend = case_when(
      !is.na(WT_12_targeted) & !is.na(WT_12_untargeted) & WT_12_targeted == WT_12_untargeted ~ 'matched',
      !is.na(WT_23_targeted) & !is.na(WT_23_untargeted) & WT_23_targeted == WT_23_untargeted ~ 'matched',
      TRUE ~ NA_character_
    ),
    `4p_trend` = case_when(
      !is.na(`4p_12_targeted`) & !is.na(`4p_12_untargeted`) & `4p_12_targeted` == `4p_12_untargeted` ~ 'matched',
      !is.na(`4p_23_targeted`) & !is.na(`4p_23_untargeted`) & `4p_23_targeted` == `4p_23_untargeted` ~ 'matched',
      TRUE ~ NA_character_
    ),
    `M3p_trend` = case_when(
      !is.na(`M3p_12_targeted`) & !is.na(`M3p_12_untargeted`) & `M3p_12_targeted` == `M3p_12_untargeted` ~ 'matched',
      !is.na(`M3p_23_targeted`) & !is.na(`M3p_23_untargeted`) & `M3p_23_targeted` == `M3p_23_untargeted` ~ 'matched',
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

results

# Save the results to a CSV file
write.csv(results, "C:/Users/ASUS/Desktop/Finale/5.Targeted/trends_matched_neg.csv", row.names = FALSE)

