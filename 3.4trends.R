
# PREPROCESS DATA > > >
trend_data <- data_file_cluster_neg

# Convert the cluster column to factor for grouping
trend_data$cluster <- as.factor(trend_data$cluster)

# Calculate the column-wise average for each cluster
averages <- trend_data %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))

# Move cluster to rownames and transpose
averages <- as.data.frame(averages)
rownames(averages) <- averages$cluster
averages <- averages %>%
  select(-cluster) %>%
  t()

# Convert to data frame
averages_df <- as.data.frame(averages)%>%
  mutate(id = as.integer(row.names(.))) %>%
  select(id, everything())



#1. Feature-wise trends :: ONLY MZMINE FILE ------------------------------------

# Define a function to compare columns and return the appropriate value
compare_columns <- function(col1, col2) {
  ifelse(col2 > col1, "+", ifelse(col1 > col2, "-", NA))
}

# Compare columns and create new columns using the comparison function
trend_df <- averages_df %>%
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


# Create a new data frame with columns 'id', and those containing '_12' or '_23'
trends_df <- trend_df %>% # Add row names as a column named 'id'
  select(id, contains("_12"), contains("_23"))

# Define a function to determine trend based on column values
determine_trend <- function(col1, col2) {
  case_when(
    col1 == "+" & col2 == "+" ~ "increasing",
    col1 == "-" & col2 == "-" ~ "decreasing",
    col1 == "+" & col2 == "-" ~ "+,-",
    col1 == "-" & col2 == "+" ~ "-,+",
    TRUE ~ NA_character_
  )
}

# Apply the function to create trend columns
trends_df <- trends_df %>%
  mutate(
    WT_trend = determine_trend(WT_12, WT_23),
    `4p_trend` = determine_trend(`4p_12`, `4p_23`),
    `M3p_trend` = determine_trend(`M3p_12`, `M3p_23`)
  )

trend_output_df <- trends_df%>%
  select(id,contains('trend'))

# Define the path where you want to save the file
#output_filename <- "C:/Users/ASUS/Desktop/Finale/trends_pos.csv"
# Save the data frame to a CSV file
#write_csv(trend_output_df, output_filename)

# 2. Feature-wise trends :: CORRELATION ----------------------------------------

cor_data <- averages_df

# Define the sets of columns and custom column names
column_sets <- list(
  'WT' = c("1", "4", "7"),
  '4p' = c("2", "5", "8"),
  'M3p' = c("3", "6", "9")
)

custom_names <- list(
  'WT' = c("WT1", "WT2", "WT3"),
  '4p' = c("4p1", "4p2", "4p3"),
  'M3p' = c("M3p1", "M3p2", "M3p3")
)

# Time vector
time_vector <- c(1, 2, 8)

# Function to calculate correlation
calculate_correlation <- function(row, time_vector) {
  intensity_vector <- as.numeric(row)
  if (sd(intensity_vector) == 0) {
    return(NA)
  } else {
    return(cor(time_vector, intensity_vector, use = "complete.obs"))
  }
}

# Process each set and combine results
cor_results <- lapply(names(column_sets), function(set_name) {
  cols <- column_sets[[set_name]]
  new_names <- custom_names[[set_name]]
  
  # Rename columns in a copy of cor_data
  temp_cor_data <- cor_data
  names(temp_cor_data)[match(cols, names(temp_cor_data))] <- new_names
  
  # Calculate correlations for the current set
  cor_subset <- temp_cor_data %>%
    select(id, all_of(new_names)) %>%
    mutate(across(all_of(new_names), ~ if_else(. == 0, 0, log10(.)))) %>%
    rowwise() %>%
    mutate(Correlation = calculate_correlation(c_across(all_of(new_names)), time_vector)) %>%
    ungroup() %>%
    select(id, Correlation) %>%
    rename(!!paste0("Correlation_", set_name) := Correlation)
  
  return(cor_subset)
})

# Combine all correlation results by left joining on 'id'
all_correlations <- reduce(cor_results, left_join, by = "id")

# Define the path where you want to save the file
#output_filename <- "C:/Users/ASUS/Desktop/Finale/correlation_trends_pos.csv"
# Save the data frame to a CSV file
#write_csv(all_correlations, output_filename)
