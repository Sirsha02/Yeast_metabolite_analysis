library(VennDiagram)
library(grid)


# VENN 1 -----------------------------------------------------------------------
# File paths
file2 <- ms2_all_pos
file1 <- "C:/Users/ASUS/Desktop/FINALE/7.MZcloud/raw/mzcloud_pos.csv"

# Read the files
df1 <- fread(file1)
df2 <- fread(file2)%>%
  select(id, rt, mz) %>%
  mutate(across(c(mz, rt), as.numeric))

# Select and extract unique values from the specified columns
unique_values_df1 <- df1$Name  # Replace ColumnName1 with your column name
unique_values_df2 <- unique(df2$id)  # Replace ColumnName2 with your column name


# Function to calculate the absolute ppm difference
calculate_ppm <- function(mz1, mz2) {
  abs((mz1 - mz2) / mz2 * 1e6)
}

# Create all possible combinations of rows from both dataframes
combined_df <- crossing(df2, df1)

# Filter based on the conditions
result_df <- combined_df %>%
  filter(calculate_ppm(mz, MZ) < 5, abs(rt - RT) < 0.1) %>%
  select(id, everything())

# Calculate intersection
intersection <- result_df %>% 
  select(id) %>% 
  distinct()

# Number of unique values in each set and the intersection
num_unique_df1 <- length(unique_values_df1)
num_unique_df2 <- length(unique_values_df2)
num_intersection <- length(intersection$id)

# Print results
cat("Number of unique values in file 1:", num_unique_df1, "\n")
cat("Number of unique values in file 2:", num_unique_df2, "\n")
cat("Number of intersecting values:", num_intersection, "\n")

# # create Venn diagram with three sets 
# draw.triple.venn(area1=40, area2=15, area3=10,  
#                  n12=5, n23=12, n13=4, n123=2,  
#                  category=c("Science","Economics","English"), 
#                  col="Red",fill=c("Green","Yellow","Blue"))


output_dir <- "C:/Users/ASUS/Desktop/Finale/8. JOINING/Venn_mzVmz"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

png(file.path(output_dir, "all_mzrt_pos.png"), width = 600, height = 600)


# move to new plotting page 
grid.newpage() 

# Create pairwise Venn diagram with equal circle sizes
draw.pairwise.venn(
  area1 = num_unique_df1,
  area2 = num_unique_df2,
  cross.area = num_intersection,
  category = c("MZcloud", "MZmine"),
  fill = c("red", "blue"),
  scaled = FALSE,
  cat.pos = c(35, -35),
  cex = 3,             # Text size for counts
  cex.category = 5     # Text size for category names
)

dev.off()  # Close the device





# VENN 2 -----------------------------------------------------------------------
# File paths
file2 <- onlyms2_pos
file1 <- "C:/Users/ASUS/Desktop/FINALE/7.MZcloud/raw/mzcloud_pos.csv"

# Read the files
df1 <- fread(file1) %>% 
  filter(MS2 != 'No MS2')
df2 <- fread(file2)%>%
  select(id, rt, mz) %>%
  mutate(across(c(mz, rt), as.numeric))

# Select and extract unique values from the specified columns
unique_values_df1 <- df1$Name  # Replace ColumnName1 with your column name
unique_values_df2 <- unique(df2$id)  # Replace ColumnName2 with your column name


# Function to calculate the absolute ppm difference
calculate_ppm <- function(mz1, mz2) {
  abs((mz1 - mz2) / mz2 * 1e6)
}

# Create all possible combinations of rows from both dataframes
combined_df <- crossing(df2, df1)

# Filter based on the conditions
result_df <- combined_df %>%
  filter(calculate_ppm(mz, MZ) < 5, abs(rt - RT) < 0.1) %>%
  select(id, everything())

# Calculate intersection
intersection <- result_df %>% 
  select(id) %>% 
  distinct()

# Number of unique values in each set and the intersection
num_unique_df1 <- length(unique_values_df1)
num_unique_df2 <- length(unique_values_df2)
num_intersection <- length(intersection$id)

# Print results
cat("Number of unique values in file 1:", num_unique_df1, "\n")
cat("Number of unique values in file 2:", num_unique_df2, "\n")
cat("Number of intersecting values:", num_intersection, "\n")

# # create Venn diagram with three sets 
# draw.triple.venn(area1=40, area2=15, area3=10,  
#                  n12=5, n23=12, n13=4, n123=2,  
#                  category=c("Science","Economics","English"), 
#                  col="Red",fill=c("Green","Yellow","Blue"))


output_dir <- "C:/Users/ASUS/Desktop/Finale/8. JOINING/Venn_mzVmz"
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }

png(file.path(output_dir, "ms2_mzrt_pos.png"), width = 600, height = 600)


# move to new plotting page 
grid.newpage() 

# Create pairwise Venn diagram with equal circle sizes
draw.pairwise.venn(
  area1 = num_unique_df1,
  area2 = num_unique_df2,
  cross.area = num_intersection,
  category = c("MZcloud", "MZmine"),
  fill = c("red", "blue"),
  scaled = FALSE,
  cat.pos = c(35, -35),
  cex = 3,             # Text size for counts
  cex.category = 5     # Text size for category names
)

dev.off()  # Close the device




# VENN 3 -----------------------------------------------------------------------
# File paths
file2 <- sirius_summary_neg
file1 <- "C:/Users/ASUS/Desktop/FINALE/7.MZcloud/raw/mz_compt_neg.csv"

# Read the files
df1 <- fread(file1) %>% 
  select(-contains('area')) %>% 
  filter(MS2 != 'No MS2')

df2 <- fread(file2) %>% 
  rename(id = mappingFeatureId)

# Select and extract unique values from the specified columns
unique_values_df1 <- df1$Name  # Replace ColumnName1 with your column name
unique_values_df2 <- unique(df2$id)  # Replace ColumnName2 with your column name

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
# Calculate intersection
intersection <- matched_rows %>% 
  select(id) %>% 
  distinct()

# Number of unique values in each set and the intersection
num_unique_df1 <- length(unique_values_df1)
num_unique_df2 <- length(unique_values_df2)
num_intersection <- length(intersection$id)

# Print results
cat("Number of unique values in file 1:", num_unique_df1, "\n")
cat("Number of unique values in file 2:", num_unique_df2, "\n")
cat("Number of intersecting values:", num_intersection, "\n")

# # create Venn diagram with three sets 
# draw.triple.venn(area1=40, area2=15, area3=10,  
#                  n12=5, n23=12, n13=4, n123=2,  
#                  category=c("Science","Economics","English"), 
#                  col="Red",fill=c("Green","Yellow","Blue"))


output_dir <- "C:/Users/ASUS/Desktop/Finale/8. JOINING/Venn_mzVmz"
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }

png(file.path(output_dir, "scaled_ms2_inchi_neg.png"), width = 600, height = 600)


# move to new plotting page 
grid.newpage() 

# Create pairwise Venn diagram with equal circle sizes
draw.pairwise.venn(
  area1 = num_unique_df1,
  area2 = num_unique_df2,
  cross.area = num_intersection,
  category = c("MZcloud", "MZmine"),
  fill = c("red", "blue"),
  scaled = TRUE,
  cat.pos = c(10, -10),
  cex = 3,             # Text size for counts
  cex.category = 5     # Text size for category names
)

dev.off()  # Close the device
