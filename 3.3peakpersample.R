
sample_feature_cluster <- data_file_cluster

#>>>>>>>>>GRAPH1>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
s_f_nocluster <- sample_feature_cluster[,-1]
# Count number of non-zero values in each row of sample_feature_cluster
non_zero_counts <- rowSums(s_f_nocluster != 0)

# Create a data frame with row names and cluster assignments
cluster_counts <- data.frame(
  Row = row.names(sample_feature_cluster),
  NonZeroCount = non_zero_counts,
  cluster = sample_feature_cluster[, 1]  # Assuming cluster is the first column
)

# Calculate average non-zero values per cluster
average_non_zero_per_cluster <- cluster_counts %>%
  group_by(cluster) %>%
  summarize(avg_non_zero_count = mean(NonZeroCount))


# Custom labels for clusters
custom_labels <- c("T1_WT", "T1_4P", "T1_M3P", "T2_WT", 
                   "T2_4P", "T2_M3P", "T3_WT", "T3_4P", "T3_M3P")

# Define a grouped color palette
my_colors <- c(
  "#66C2A5", "#66C2A5", "#66C2A5",  # Group 1 (similar shades)
  "#FC8D62", "#FC8D62", "#FC8D62",  # Group 2 (similar shades)
  "#8DA0CB", "#8DA0CB", "#8DA0CB"   # Group 3 (similar shades)
)

# Plotting the bar graph with custom labels
plot <- ggplot(average_non_zero_per_cluster, aes(x = factor(cluster), y = avg_non_zero_count)) +
  geom_bar(stat = "identity", fill = my_colors, color = "black", width = 0.7) +  # Adjust bar width and color
  labs(
    x = "Sample", y = "Average number of peaks",
    title = "Number of peaks per Sample"
  ) +
  scale_x_discrete(labels = custom_labels) +  # Specify custom labels for x-axis
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10, color = "#333333", face = "bold"),  # Adjust x-axis text
    axis.text.y = element_text(size = 10, color = "#333333", face = "bold"),  # Adjust y-axis text
    axis.title = element_text(size = 12, face = "bold", color = "#333333"),  # Adjust axis titles
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "#333333"),  # Adjust plot title
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.margin = margin(1, 1, 1, 1, "cm"),
    axis.line = element_line(color = "black"),  # Add black axis lines
    legend.position = "none"  # Remove legend
  ) +
  geom_text(aes(label = round(avg_non_zero_count, 1), y = avg_non_zero_count + 0.5), vjust = -0.5, size = 3.5, color = "#333333") +  # Add labels to bars with adjusted text color
  coord_flip()  # Flip coordinates for horizontal bars

plot
# Save the plot as a PNG file
ggsave("C:/Users/ASUS/Desktop/Finale/1.MZmine exported/neg_all.png", plot, width = 8, height = 6, dpi = 300)


#>>>>>>>>>>>>>>>>>>>>>GRAPH2>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
s_f_nocluster <- sample_feature_cluster[,-1]
# Count number of non-zero values in each row of sample_feature_cluster
non_zero_counts <- rowSums(s_f_nocluster != 0)

# Create a data frame with row names and cluster assignments
cluster_counts <- data.frame(
  Row = row.names(sample_feature_cluster),
  NonZeroCount = non_zero_counts,
  cluster = sample_feature_cluster[, 1]  # Assuming cluster is the first column
)

average_non_zero_per_cluster_grouped <- cluster_counts %>%
  mutate(cluster_group = case_when(
    cluster %in% 1:3 ~ "Cluster Group 1",
    cluster %in% 4:6 ~ "Cluster Group 2",
    cluster %in% 7:9 ~ "Cluster Group 3"
  )) %>%
  group_by(cluster_group) %>%
  summarize(avg_non_zero_count = mean(NonZeroCount))

# Custom labels for grouped clusters
custom_group_labels <- c("T1", "T2", "T3")

# Assuming average_non_zero_per_cluster_grouped is defined correctly before this point

# Define a custom color palette with three distinct colors
my_colors_grouped <- c("#66C2A5", "#FC8D62", "#8DA0CB")

# Plotting the grouped bar graph with custom colors
plot2<- ggplot(average_non_zero_per_cluster_grouped, aes(x = factor(cluster_group), y = avg_non_zero_count, fill = factor(cluster_group))) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = my_colors_grouped) +  # Set custom colors for fill
  labs(
    x = "Sample Group", y = "Average Non-Zero Count",
    title = "Average Non-Zero Features per Sample Group"
  ) +
  scale_x_discrete(labels = custom_group_labels) +  # Specify custom labels for x-axis
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Adjust x-axis text orientation
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size
    axis.title = element_text(size = 12, face = "bold"),  # Adjust axis title size and style
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Adjust plot title size and alignment
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),  # Customize major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "none"  # Remove legend
  ) +
  geom_text(aes(label = round(avg_non_zero_count, 1)), vjust = -0.5, size = 3.5)  # Add labels to bars


ggsave("C:/Users/ASUS/Desktop/Finale/1.MZmine exported/neg_all_T.png", plot2, width = 8, height = 6, dpi = 300)
