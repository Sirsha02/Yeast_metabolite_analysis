
# Function to process each file
process_file <- function(filename) {
  ms1peaks <- fread(filename) %>%
    mutate(peakcheck = height / area) %>%
    select(id, rt, mz, height, area, peakcheck, contains("area")) %>%
    dplyr::filter(peakcheck > 1)
  
  sample_feature <- ms1peaks %>%
    select(id, contains(":area")) %>%
    select(-contains("PBQC")) %>%
    select(-contains("blank")) %>%  # Eliminate columns containing 'blank'
    column_to_rownames("id") %>%
    replace(is.na(.), 0)
  
  sample_feature <- as.data.frame(t(sample_feature))
  
  return(sample_feature)
}

# File paths
filename1 <- ms2_all_pos
filename2 <- ms2_all_neg

# Process both files
data1 <- process_file(filename1)
data2 <- process_file(filename2)

# Full join based on row names
full_join_data <- full_join(data1 %>% rownames_to_column("id"),
                            data2 %>% rownames_to_column("id"),
                            by = "id") %>%
  column_to_rownames("id")

# Print the resulting data frame
print(full_join_data)




sample_feature <- full_join_data
# Eliminating zero rows and columns and continuing with clustering
constant_cols <- apply(sample_feature, 2, function(col) length(unique(col)) == 1)
sample_feature <- sample_feature[, !constant_cols]

zero_cols <- apply(sample_feature, 2, function(col) all(col == 0))
sample_feature <- sample_feature[, !zero_cols]

# Assigning groups based on column names
assign_group <- function(col_names) {
  group <- rep(NA, length(col_names))
  for (i in seq_along(col_names)) {
    num <- as.numeric(sub(".*:CC211015_(\\d+)\\.mzML:area", "\\1", col_names[i]))
    if (!is.na(num)) {
      if (num >= 1 && num <= 8) {
        group[i] <- 'A1'
      } else if (num >= 9 && num <= 16) {
        group[i] <- 'B1'
      } else if (num >= 17 && num <= 24) {
        group[i] <- 'C1'
      } else if (num >= 25 && num <= 32) {
        group[i] <- 'A2'
      } else if (num >= 33 && num <= 40) {
        group[i] <- 'B2'
      } else if (num >= 41 && num <= 48) {
        group[i] <- 'C2'
      } else if (num >= 49 && num <= 56) {
        group[i] <- 'A3'
      } else if (num >= 57 && num <= 64) {
        group[i] <- 'B3'
      } else if (num >= 65 && num <= 72) {
        group[i] <- 'C3'
      }
    }
  }
  return(group)
}

group_labels <- assign_group(rownames(sample_feature))

# Define custom mappings for colors and shapes
color_mapping <- c('A1' = 'WT', 'A2' = 'WT', 'A3' = 'WT',
                   'B1' = '4p', 'B2' = '4p', 'B3' = '4p',
                   'C1' = 'M3p', 'C2' = 'M3p', 'C3' = 'M3p')

shape_mapping <- c('A1' = 'T1', 'B1' = 'T1', 'C1' = 'T1',
                   'A2' = 'T2', 'B2' = 'T2', 'C2' = 'T2',
                   'A3' = 'T3', 'B3' = 'T3', 'C3' = 'T3')

# PCA
set.seed(123)
data_pca <- prcomp(sample_feature, center = TRUE, scale. = TRUE)
PC1var <- round(summary(data_pca)$importance[2, 1] * 100, 1)
PC2var <- round(summary(data_pca)$importance[2, 2] * 100, 1)

pca_table <- as.data.frame(data_pca$x) %>%
  rownames_to_column("FileName") %>%
  mutate(Group = rep(group_labels, each = nrow(data_pca$x) / length(group_labels)),
         ColorGroup = color_mapping[Group],
         ShapeGroup = shape_mapping[Group])

# Define custom color and shape palettes
color_palette <- viridis::viridis(3)
shape_palette <- c(16, 17, 18)  # Shapes corresponding to T1, T2, T3

# Plotting PCA with custom colors and shapes
p <-ggplot(data = pca_table) +
  geom_point(mapping = aes(x = PC1, y = PC2, colour = ColorGroup, shape = ShapeGroup), size = 4, alpha = 0.7) + 
  scale_color_manual(values = color_palette) +
  scale_shape_manual(values = shape_palette) +
  xlab(paste("PC1, ", PC1var, "%", sep = "")) +
  ylab(paste("PC2, ", PC2var, "%", sep = "")) +
  theme_minimal() +  # Replace with your custom theme if needed
  theme(
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white"),   # Set plot background to white
    text = element_text(color = "black")               # Set text color to black
  )
p
# Save plot with specific dimensions (width = 8 inches, height = 6 inches)
ggsave("C:/Users/ASUS/Desktop/Finale/1.MZmine_exported/graphs/ms1/pca_posneg.png", plot = p, width = 8, height = 6, units = "in")
