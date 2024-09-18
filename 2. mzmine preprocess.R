# FILE PROCESSING
# All parts can run at once
# For simply viewing/analysing the data, use: data_file_pos or, data_file_cluster_pos
# For some specific graphs, strictly use: data_file_cluster_pos

# PART 1 : BASIC PROCESSING -----------------------------------------------------

# Files exported from mzmine
# csv
# Input filename > > > > >
filename = ms2_all_pos

raw_file_pos <- fread(filename)

filtered_file_pos <- raw_file_pos %>%
  select(id, mz, rt, charge, height, area, contains('area')) %>%  # Write the names of columns that you want
  select(-contains("blank")) %>%  # Here, we are removing the blank samples
  replace(is.na(.), 0)  # All NA values are replaced with 0

# Check for peaks with small height/area ratio i.e. flat peaks and filter them out
# Using this is optional [If not using COMMENT OUT]
filtered_file_pos <- filtered_file_pos %>%
  mutate(peakcheck = height / area) %>%
  dplyr::filter(peakcheck > 1)

data_pos <- as.data.frame(filtered_file_pos)

# PART 2 : SAMPLE NAME MODIFICATION FOR EASE ------------------------------------
# Modify the sample names such that only the sample number remains, makes everything much easier

data_file_pos <- data_pos %>%
  column_to_rownames('id')

# Keep only columns containing ":area"
area_col_pos <- grep(":area", colnames(data_file_pos), value = TRUE)

# Subset the data_file_pos to include only columns with ":area"
data_file_area_pos <- data_file_pos[, colnames(data_file_pos) %in% area_col_pos]

# Clean up column names by removing '211015' and keeping only numeric parts
colnames(data_file_area_pos) <- gsub("211015", "", colnames(data_file_area_pos))
colnames(data_file_area_pos) <- gsub("[^0-9]", "", colnames(data_file_area_pos))

# Sort column names in ascending order based on numeric values
data_file_area_ordered_pos <- data_file_area_pos[, order(as.numeric(colnames(data_file_area_pos)))]

# Convert row names to a column named 'id'
#data_file_area_pos <- rownames_to_column(data_file_area_pos, var = "id")
# Transpose the sample feature matrix
data_file_area_pos_ <- t(data_file_area_ordered_pos)
data_file_area_ms1_pos <- data_file_area_pos_[rownames(data_file_area_pos_) != '4.1', ]
data_file_area_ms1_pos <- data_file_area_ms1_pos[rownames(data_file_area_ms1_pos) != '5.1', ]
# Create a data frame with 'id' column
data_file_area_ms1_pos <- as.data.frame(data_file_area_ms1_pos)

# PART 3 : CLUSTERING SAMPLES [needed for some graphs]---------------------------

data_file_c_pos <- data_file_area_ms1_pos

# Extract numeric parts from row names to use for clustering
row_names_pos <- rownames(data_file_c_pos)
row_numbers_pos <- as.numeric(row_names_pos)

#Our sample has 8 replicas, so this function assigns the replicas to 1 cluster
# Define a function to assign clusters based on numeric ranges
assign_cluster_pos <- function(num) {
  return(cut(num, breaks = seq(0, 72, by = 8), labels = FALSE))
}

# Apply the cluster assignment function to row numbers
cluster_assignments_pos <- sapply(row_numbers_pos, assign_cluster_pos)

# Bind the cluster assignments as a new column in the transposed data frame
data_file_cluster_pos <- cbind(cluster = cluster_assignments_pos, data_file_c_pos)

# ## OUTPUT>>>>>>>>>>>>>>>>>>>>>--------------------------------------------------
# # Directory and file path
# dir_path_pos <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/modified/"
# # Create directory if it doesn't exist
# if (!dir.exists(dir_path_pos)) {
#   dir.create(dir_path_pos, recursive = TRUE)
# }
# 
# file_path_pos <- file.path(dir_path_pos, "filtered_ms2_all_pos.csv")
# # Save data as CSV file
# write.csv(filtered_file_pos, file_path_pos, row.names = FALSE)
# 
# file_path_pos <- file.path(dir_path_pos, "cleaned_ms2_all_pos.csv")
# # Save data as CSV file
# write.csv(data_file_area_pos, file_path_pos, row.names = FALSE)
# 
# file_path_pos <- file.path(dir_path_pos, "clustered_ms2_all_pos.csv")
# # Save data as CSV file
# write.csv(data_file_cluster_pos, file_path_pos, row.names = FALSE)
# 


###
###     N E G A T I V E  --   M O D E  -> -> -> -> 
###




# FILE PROCESSING
# All parts can run at once
# For simply viewing/analysing the data, use: data_file_neg or, data_file_cluster_neg
# For some specific graphs, strictly use: data_file_cluster_neg



# PART 1 : BASIC PROCESSING -----------------------------------------------------

# Files exported from mzmine
# csv
# Input filename > > > > >
filename_neg = ms2_all_neg
raw_file_neg <- fread(filename_neg)

filtered_file_neg <- raw_file_neg %>%
  select(id, mz, rt, charge, height, area, contains('area')) %>%  # Write the names of columns that you want
  select(-contains("blank")) %>%  # Here, we are removing the blank samples
  replace(is.na(.), 0)  # All NA values are replaced with 0

# Check for peaks with small height/area ratio i.e. flat peaks and filter them out
# Using this is optional [If not using COMMENT OUT]
filtered_file_neg <- filtered_file_neg %>%
  mutate(peakcheck = height / area) %>%
  dplyr::filter(peakcheck > 1)

data_neg <- as.data.frame(filtered_file_neg)




# PART 2 : SAMPLE NAME MODIFICATION FOR EASE ------------------------------------
# Modify the sample names such that only the sample number remains, makes everything much easier

data_file_neg <- data_neg %>%
  column_to_rownames('id')

# Keep only columns containing ":area"
area_col_neg <- grep(":area", colnames(data_file_neg), value = TRUE)

# Subset the data_file to include only columns with ":area"
data_file_area_neg <- data_file_neg[, colnames(data_file_neg) %in% area_col_neg]

# Clean up column names by removing '211015' and keeping only numeric parts
colnames(data_file_area_neg) <- gsub("211015", "", colnames(data_file_area_neg))
colnames(data_file_area_neg) <- gsub("[^0-9]", "", colnames(data_file_area_neg))

# Sort column names in ascending order based on numeric values
data_file_area_ordered_neg <- data_file_area_neg[, order(as.numeric(colnames(data_file_area_neg)))]

# Convert row names to a column named 'id'
# data_file_area_neg <- rownames_to_column(data_file_area_neg, var = "id")
# Transpose the sample feature matrix
data_file_area_neg_ <- t(data_file_area_ordered_neg)
data_file_area_ms1_neg <- data_file_area_neg_[rownames(data_file_area_neg_) != '4.1', ]
data_file_area_ms1_neg <- data_file_area_ms1_neg[rownames(data_file_area_ms1_neg) != '5.1', ]
# Create a data frame with 'id' column
data_file_area_ms1_neg <- as.data.frame(data_file_area_ms1_neg)



#PART 3 : CLUSTERING SAMPLES [needed for some graphs]---------------------------

data_file_c_neg <- data_file_area_ms1_neg

# Extract numeric parts from row names to use for clustering
row_names_neg <- rownames(data_file_c_neg)
row_numbers_neg <- as.numeric(row_names_neg)

# Our sample has 8 replicas, so this function assigns the replicas to 1 cluster
# Define a function to assign clusters based on numeric ranges
assign_cluster_neg <- function(num) {
  return(cut(num, breaks = seq(0, 72, by = 8), labels = FALSE))
}

# Apply the cluster assignment function to row numbers
cluster_assignments_neg <- sapply(row_numbers_neg, assign_cluster_neg)

# Bind the cluster assignments as a new column in the transposed data frame
data_file_cluster_neg <- cbind(cluster = cluster_assignments_neg, data_file_c_neg)




# ## OUTPUT>>>>>>>>>>>>>>>>>>>>>--------------------------------------------------
# # Directory and file path
# dir_path_neg <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/modified/"
# # Create directory if it doesn't exist
# if (!dir.exists(dir_path_neg)) {
#   dir.create(dir_path_neg, recursive = TRUE)
# }
# 
# file_path_neg <- file.path(dir_path_neg, "filtered_ms2_all_neg.csv")
# # Save data as CSV file
# write.csv(filtered_file_neg, file_path_neg, row.names = FALSE)
# 
# file_path_neg <- file.path(dir_path_neg, "cleaned_ms2_all_neg.csv")
# # Save data as CSV file
# write.csv(data_file_area_neg, file_path_neg, row.names = FALSE)
# 
# file_path_neg <- file.path(dir_path_neg, "clustered_ms2_all_neg.csv")
# # Save data as CSV file
# write.csv(data_file_cluster_neg, file_path_neg, row.names = FALSE)
# 
