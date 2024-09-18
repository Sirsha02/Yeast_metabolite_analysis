#LOADING FILE TO DO ALL THE GRAPHS:


# Ensure peak_data is a data frame and convert all columns to numeric
peak_data <- as.data.frame(data_file_area)
peak_data[] <- lapply(peak_data, function(x) as.numeric(as.character(x)))

# Normalize each column by the highest value in the column
peak_data <- as.data.frame(lapply(peak_data, function(col) col / max(col, na.rm = TRUE)))

peak_data <- as.data.frame(sapply(peak_data, function(col) {
  col[is.na(col)] <- 0
  col[is.infinite(col)] <- 0
  return(col)
}))

#1. PCA-------------------------------------------------------------------------
# Transpose the data for PCA
pca_data <- as.data.frame(peak_data)

# #>>>>>
# # #PERFORM THE BELOW CODE TO FIND PCA OF POSITIVE AND NEGATIVE COMBINED::
# 
# data1 <- fread(filtered_ms2_all_pos)
# data2<- fread(filtered_ms2_all_neg)
# 
# # Full join based on row names
# full_join_data <- full_join(data1 %>% rownames_to_column("id"),
#                             data2 %>% rownames_to_column("id"),
#                             by = "id") %>%
#   column_to_rownames("id")
# 
# pca_data <- as.data.frame(full_join_data)
# #>>>>>



# Eliminate constant and zero columns
constant_cols <- apply(pca_data, 2, function(col) length(unique(col)) == 1)
pca_data <- pca_data[, !constant_cols]

zero_cols <- apply(pca_data, 2, function(col) all(col == 0))
pca_data <- pca_data[, !zero_cols]

# Ensure peak_data has row names
rownames(pca_data) <- rownames(peak_data)

# Define the assign_group function to work with row names
assign_group <- function(row_names) {
  group <- rep(NA, length(row_names))
  for (i in seq_along(row_names)) {
    num <- suppressWarnings(as.numeric(row_names[i]))  # Use suppressWarnings to avoid warnings for non-numeric values
    if (is.na(num)) next  # Skip if conversion fails
    if (num >= 1 && num <= 8) group[i] <- 'A1'
    else if (num >= 9 && num <= 16) group[i] <- 'B1'
    else if (num >= 17 && num <= 24) group[i] <- 'C1'
    else if (num >= 25 && num <= 32) group[i] <- 'A2'
    else if (num >= 33 && num <= 40) group[i] <- 'B2'
    else if (num >= 41 && num <= 48) group[i] <- 'C2'
    else if (num >= 49 && num <= 56) group[i] <- 'A3'
    else if (num >= 57 && num <= 64) group[i] <- 'B3'
    else if (num >= 65 && num <= 72) group[i] <- 'C3'
  }
  return(group)
}

# Define custom mappings for colors and shapes
color_mapping <- c('A1' = 'WT', 'A2' = 'WT', 'A3' = 'WT',
                   'B1' = '4p', 'B2' = '4p', 'B3' = '4p',
                   'C1' = 'M3p', 'C2' = 'M3p', 'C3' = 'M3p')

shape_mapping <- c('A1' = 'T1', 'B1' = 'T1', 'C1' = 'T1',
                   'A2' = 'T2', 'B2' = 'T2', 'C2' = 'T2',
                   'A3' = 'T3', 'B3' = 'T3', 'C3' = 'T3')


# Apply the function to row names of pca_data
group_labels <- assign_group(rownames(pca_data))

# Perform PCA
set.seed(123)
data_pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)
PC1var <- round(summary(data_pca)$importance[2, 1] * 100, 1)
PC2var <- round(summary(data_pca)$importance[2, 2] * 100, 1)

# Use group_labels in the PCA table
pca_table <- as.data.frame(data_pca$x) %>%
  rownames_to_column("FileName") %>%
  mutate(Group = group_labels,
         ColorGroup = color_mapping[Group],
         ShapeGroup = shape_mapping[Group])

# Define custom color and shape palettes
color_palette <- viridis::viridis(3)
shape_palette <- c(16, 17, 18)  # Shapes corresponding to T1, T2, T3

# Plotting PCA with custom colors and shapes
pca <- ggplot(data = pca_table) +
  geom_point(mapping = aes(x = PC1, y = PC2, colour = ColorGroup, shape = ShapeGroup), size = 4, alpha = 0.7) + 
  scale_color_manual(values = color_palette) +
  scale_shape_manual(values = shape_palette) +
  xlab(paste("PC1, ", PC1var, "%", sep = "")) +
  ylab(paste("PC2, ", PC2var, "%", sep = "")) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set plot background to white
    text = element_text(color = "black")              # Set text color to black
  )
pca
#2. UMAP------------------------------------------------------------------------
# Load peak data into a data frame
umap_data <- as.data.frame(peak_data)

# Assign group numbers based on row names
group <- assign_group(rownames(umap_data))

# Convert group numbers to factor for better visualization
group <- factor(group)

# Perform UMAP
set.seed(123)
umap_result <- umap(umap_data)

# Combine UMAP results with groups
umap_df <- as.data.frame(umap_result$layout)
umap_df$group <- group

# Set column names for umap_df
colnames(umap_df) <- c("UMAP1", "UMAP2")

# Define a custom color palette
colors <- c("#800000", "#e6194B", "#fabed4",  
            "#808000", "#ffe119", "#fff595",
            "#911eb4", "#f032e6", "#dcbeff")
names(colors) <- levels(group)

# Plot UMAP results with different colors for each group
p_umap <- ggplot(data = umap_df) +
  geom_point(mapping = aes(x = UMAP1, y = UMAP2, color = group), size = 4, alpha = 0.65) +
  scale_color_manual(values = colors, name = "Group") +
  theme_minimal() +
  labs(title = "UMAP Clustering", x = "UMAP1", y = "UMAP2") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set plot background to white
    text = element_text(color = "black")              # Set text color to black
  )

p_umap

#3. HEATMAP --------------------------------------------------------------------

# Load peak data into a data frame
heatmap_data <- peak_data%>%
  t()


# Transpose the data
data_transposed_sorted <- as.data.frame(t(heatmap_data))

# Calculate the distance matrix
chosen_distance <- "canberra"  # methods: "binary", "manhattan", "euclidean"
distance <- dist(data_transposed_sorted, method = chosen_distance)
distance_matrix <- as.data.frame(as.matrix(distance))

# Generate heatmap data matrix
heatmapdata_matrix <- as.matrix(distance_matrix)

# Plot the heatmap
p_heat<-ggplot(data = melt(heatmapdata_matrix), aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  labs(x = "Columns", y = "Rows", fill = "Values") +
  scale_fill_viridis_c(begin = 0, end = 1) +  # Adjust the color scale as needed
  scale_x_continuous(breaks = seq(1, ncol(heatmapdata_matrix), by = 4), labels = colnames(heatmapdata_matrix)[seq(1, ncol(heatmapdata_matrix), by = 4)]) + 
  scale_y_continuous(breaks = seq(1, nrow(heatmapdata_matrix), by = 4), labels = rownames(heatmapdata_matrix)[seq(1, nrow(heatmapdata_matrix), by = 4)]) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+  # Replace with your custom theme if needed
  theme(
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set plot background to white
    text = element_text(color = "black")               # Set text color to black
  )


p_heat

# ##OUTPUT>>>>>>>>>>>>>>>>>>>>>
# # Directory and file path
# dir_path <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/graphs/"
# # Create directory if it doesn't exist
# if (!dir.exists(dir_path)) {
#   dir.create(dir_path, recursive = TRUE)
# }
# 
# file_path <- file.path(dir_path, "normk_pca_pos.png")
# # Save plot with specific dimensions (width = 8 inches, height = 6 inches)
# ggsave(file_path, plot = pca, width = 8, height = 6, units = "in")
# 
# 
# file_path <- file.path(dir_path, "normk_umap_pos.png")
# # Save plot with specific dimensions (width = 8 inches, height = 6 inches)
# ggsave(file_path, plot = p_umap, width = 8, height = 6, units = "in")
# 
# file_path <- file.path(dir_path, "normk_heatmap_pos.png")
# # Save plot with specific dimensions (width = 8 inches, height = 6 inches)
# ggsave(file_path, plot = p_heat, width = 8, height = 6, units = "in")
# 
# 
# 
