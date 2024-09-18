

# Define the directory path
output_dir <- "C:/Users/ASUS/Desktop/FINALE/1.MZmine exported/graphs"

# Create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}



# > >                                      > > >
# Ensure peak_data is a data frame and convert all columns to numeric
peak_data <- as.data.frame(data_file_area_ms1_pos)
peak_data[] <- lapply(peak_data, function(x) as.numeric(as.character(x)))

# Normalize each column by the highest value in the column
peak_data <- as.data.frame(lapply(peak_data, function(col) col / max(col, na.rm = TRUE)))

peak_data <- as.data.frame(sapply(peak_data, function(col) {
  col[is.na(col)] <- 0
  col[is.infinite(col)] <- 0
  return(col)
}))

# Transpose the data for PCA
pca_data <- as.data.frame(peak_data)


# Eliminate constant and zero columns
constant_cols <- apply(pca_data, 2, function(col) length(unique(col)) == 1)
pca_data <- pca_data[, !constant_cols]

zero_cols <- apply(pca_data, 2, function(col) all(col == 0))
pca_data <- pca_data[, !zero_cols]

# Ensure peak_data has row names
rownames(pca_data) <- rownames(peak_data)

set.seed(123)
data_pca <- prcomp(pca_data, rank = 3)
PC1var <- round(summary(data_pca)$importance[2,1]*100,1)
PC2var <- round(summary(data_pca)$importance[2,2]*100,1)
pca_fp <- as.data.frame(data_pca$x) %>% 
  rownames_to_column("FileName")
loadings <- as.data.frame(data_pca$rotation) %>% 
  rownames_to_column("featureId")

# Remove prefix X from featureId values
loadings$featureId <- sub("^X", "", loadings$featureId)

# Create a custom theme with white background
white_bg_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  )


# PLOT 1 - - - (MZ)

N <- 200 # Number of top features to select
top_loadings <- loadings %>% 
  mutate(abs_PC1 = abs(PC1), abs_PC2 = abs(PC2)) %>% 
  rowwise() %>% 
  mutate(max_abs_loading = max(abs_PC1, abs_PC2)) %>% 
  ungroup() %>% 
  arrange(desc(max_abs_loading)) %>% 
  slice(1:N) %>% 
  left_join(filtered_file_neg %>%  select(id, rt, mz) %>% mutate(featureId = as.character(id)))
# > >                                                            


# Plot using ggplot2 and ggrepel with increased max.overlaps
p1 <-ggplot(top_loadings, aes(x = PC1, y = PC2, label = featureId, color = mz)) +
  geom_point() +
  geom_text_repel(
    max.overlaps = Inf,  # Set to Inf or a high number to allow more overlaps
    nudge_x = 0.001,       # Adjust as needed to avoid overlap
    nudge_y = 0.001        # Adjust as needed to avoid overlap
  ) +
  labs(title = "PCA Loadings Plot",
       x = paste0('PC1 (', PC1var, "% variance)"),
       y = paste0('PC2 (', PC2var, "% variance)")) +
  scale_color_viridis(option = "H") +
  white_bg_theme


# PLOT 2 - - - (RT)

# Plot using ggplot2 and ggrepel with increased max.overlaps
p2 <- ggplot(top_loadings, aes(x = PC1, y = PC2, label = featureId, color = rt)) +
  geom_point() +
  geom_text_repel(
    max.overlaps = Inf,  # Set to Inf or a high number to allow more overlaps
    nudge_x = 0.001,       # Adjust as needed to avoid overlap
    nudge_y = 0.001        # Adjust as needed to avoid overlap
  ) +
  labs(title = "PCA Loadings Plot",
       x = paste0('PC1 (', PC1var, "% variance)"),
       y = paste0('PC2 (', PC2var, "% variance)")) +
  scale_color_viridis(option = "H") +
  white_bg_theme



#ggsave(filename = file.path(output_dir, "PCaloading_mz_neg.png"), plot = p1, width = 10, height = 8, dpi = 300)
#ggsave(filename = file.path(output_dir, "PCaloading_rt_neg.png"), plot = p2, width = 10, height = 8, dpi = 300)


# FOR PLOT 3 - - -  - - - - - - - - - -

# # Convert pca_data to a matrix if it's not already one
# pca_data <- as.matrix(pca_data)
# 
# # Transpose, convert to data frame, and add row names as a column
# pca_data <- t(pca_data) %>%
#   as.data.frame() %>%
#   rownames_to_column("id")
# # Remove prefix X from featureId values
# pca_data$id <- sub("^X", "", pca_data$id)

# > >
# Perform the left join and filter to include only those ids present in mappingFeatureId

loadings_ <- loadings %>% 
  left_join(filtered_file_pos %>%  select(id, rt, mz) %>% mutate(featureId = as.character(id)))


filtered_loadings <- loadings_ %>%
  left_join(sirius_structure_summary_pos %>%
              mutate(featureId= as.character(mappingFeatureId)),
            by = "featureId") %>%
  filter(!is.na(mappingFeatureId))# Keep only rows with non-NA mappingFeatureId

# Ensure mappingFeatureId is character in canopus
canopus <- canopus %>%
  mutate(featureId= as.character(id))

# Second left join with canopus
filtered_loadings <- filtered_loadings %>%
  left_join(canopus, by = "featureId")


## PLOT 3 - - - (MZ)

# Plot using ggplot2 and ggrepel with increased max.overlaps
p3 <-ggplot(filtered_loadings, aes(x = PC1, y = PC2, label = featureId, color = mz)) +
  geom_point() +
  geom_text_repel(
    max.overlaps = Inf,  # Set to Inf or a high number to allow more overlaps
    nudge_x = 0.001,       # Adjust as needed to avoid overlap
    nudge_y = 0.001        # Adjust as needed to avoid overlap
  ) +
  labs(title = "PCA Loadings Plot",
       x = paste0('PC1 (', PC1var, "% variance)"),
       y = paste0('PC2 (', PC2var, "% variance)")) +
  scale_color_viridis(option = "H") +
  white_bg_theme



## PLOT 4 - - - (RT)

# Plot using ggplot2 and ggrepel with increased max.overlaps
p4 <- ggplot(filtered_loadings, aes(x = PC1, y = PC2, label = featureId, color = rt)) +
  geom_point() +
  geom_text_repel(
    max.overlaps = Inf,  # Set to Inf or a high number to allow more overlaps
    nudge_x = 0.001,       # Adjust as needed to avoid overlap
    nudge_y = 0.001        # Adjust as needed to avoid overlap
  ) +
  labs(title = "PCA Loadings Plot",
       x = paste0('PC1 (', PC1var, "% variance)"),
       y = paste0('PC2 (', PC2var, "% variance)")) +
  scale_color_viridis(option = "H") +
  white_bg_theme

## PLOT 5 - - - (class)

# Plot using ggplot2 and ggrepel with increased max.overlaps
# Plot using ggplot2 and ggrepel with increased max.overlaps
p5 <- ggplot(filtered_loadings, aes(x = PC1, y = PC2, label = featureId, color = class)) +
  geom_point() +
  geom_text_repel(
    max.overlaps = Inf,  # Set to Inf or a high number to allow more overlaps
    nudge_x = 0.001,     # Adjust as needed to avoid overlap
    nudge_y = 0.001      # Adjust as needed to avoid overlap
  ) +
  labs(title = "PCA Loadings Plot",
       x = paste0('PC1 (', PC1var, "% variance)"),
       y = paste0('PC2 (', PC2var, "% variance)")) +
  scale_color_viridis_d(option = "H") +  # Use scale_color_viridis_d() for discrete values
  white_bg_theme


ggsave(filename = file.path(output_dir, "PCAloading_sirius_mz_pos.png"), plot = p3, width = 10, height = 8, dpi = 300)
ggsave(filename = file.path(output_dir, "PCAloading_sirius_rt_pos.png"), plot = p4, width = 10, height = 8, dpi = 300)
ggsave(filename = file.path(output_dir, "PCAloading_sirius_class_pos.png"), plot = p5, width = 10, height = 8, dpi = 300)

