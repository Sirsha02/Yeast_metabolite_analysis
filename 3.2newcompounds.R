
compounds_ <- data_file_cluster_pos

# Function to filter in columns where specified clusters have values of 0
filter_columns <- function(df, clusters) {
  zero_in_clusters <- apply(df[, -1], 2, function(col) {
    all(col[df[, 1] %in% clusters] == 0)
  })
  df <- df[, c(TRUE, zero_in_clusters)]
  return(df)
}

# Specify the clusters you want to filter for
clusters_to_keep <- c(2,5,8)

# Apply the filter function
compounds <- filter_columns(compounds_, clusters_to_keep)

# Convert back to data frame (if needed)
compounds <- as.data.frame(compounds)

# Save the filtered dataframe to a CSV file
output_file <- "C:/Users/ASUS/Desktop/Finale/1.MZmine_exported/newcompounds/M3p_T1_neg.csv"
fwrite(compounds, output_file)
