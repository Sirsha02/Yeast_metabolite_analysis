
# Set a common output directory
output_dir <- "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/graphs/bubbleplots_neg"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# STEP 1 : DATA > > >

# Convert to data frame and add 'id' column
bubble_data <- as.data.frame(data_file_area_ordered, stringsAsFactors = FALSE) %>%
  mutate(id = as.integer(row.names(.))) %>%
  select(id, everything())

# Read SIRIUS TSV file
canopus <- fread(canopus_structure_summary_n) %>%
  rename(id = mappingFeatureId)

# Merge data and filter to ensure data contains 'NPC#pathway'
data <- bubble_data %>%
  left_join(canopus, by = "id") %>%
  filter(!is.na(`NPC#pathway`))

# Define classifiers and column groups
classifiers <- c("NPC#pathway", "NPC#superclass", "NPC#class", 
                 "ClassyFire#superclass", "ClassyFire#class", 
                 "ClassyFire#subclass", "ClassyFire#level 5", "ClassyFire#most specific class")

column_groups <- list(
  group1 = 1:8, group2 = 9:16, group3 = 17:24, group4 = 25:32, 
  group5 = 33:40, group6 = 41:48, group7 = 49:56, group8 = 57:64, group9 = 65:72
)

y_axis_labels <- c(
  "average_group1" = "WT_1", "average_group2" = "4p_1", "average_group3" = "M3p_1",
  "average_group4" = "WT_2", "average_group5" = "4p_2", "average_group6" = "M3p_2",
  "average_group7" = "WT_3", "average_group8" = "4p_3", "average_group9" = "M3p_3"
)

group_labels <- c("WT_1", "4p_1", "M3p_1", "WT_2", "4p_2", "M3p_2", "WT_3", "4p_3", "M3p_3")

for (Classifier in classifiers) {
  column_groups[[Classifier]] <- Classifier
  
  # Calculate row-wise averages for each group of columns including the classifier
  averages <- data.frame(
    id = seq_len(nrow(data)),
    lapply(column_groups, function(cols) {
      if (is.character(cols)) {
        data[[cols]]
      } else {
        rowMeans(data[cols], na.rm = TRUE)
      }
    })
  )
  
  # Rename columns based on column groups
  colnames(averages)[-1] <- ifelse(names(column_groups) == Classifier, Classifier, paste0("average_", names(column_groups)))
  
  # Calculate column sums across NPC pathways
  column_sums <- list()
  for (col in names(averages)[2:10]) {  # Assuming columns start from NPC pathway to average group9
    column_sum <- averages %>%
      group_by(.data[[Classifier]]) %>%
      summarise(
        sum = sum(.data[[col]], na.rm = TRUE),
        .groups = "drop"  # Drop the grouping after summarizing
      )
    
    column_sum$Column <- col  # Add a column to identify the original column
    column_sums[[col]] <- column_sum
  }
  
  # Combine results into a single dataframe
  column_sums_df <- bind_rows(column_sums)
  
  # Pivot the dataframe to reshape it
  column_sums_df <- column_sums_df %>%
    pivot_wider(names_from = Column, values_from = sum)
  
  # Rename the NPC pathway column
  df <- column_sums_df %>%
    rename(pathway = !!sym(Classifier))
  
  # Reshape data for plotting
  df_melted <- melt(df, id.vars = "pathway", variable.name = "group", value.name = "sum")

  # Plotting the bubble plot with the sum values
  p1 <- ggplot(df_melted, aes(x = pathway, y = group, size = sum, color = pathway)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(1, 10)) +  # Adjust the range as needed for bubble sizes
    scale_y_discrete(labels = y_axis_labels) +  # Apply custom y-axis labels
    labs(title = "Compound classes",
         x = "Classes", y = "Samples",
         size = "Sum", color = "Pathway") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      legend.background = element_rect(fill = "white"),
      axis.text.x = element_text(size=15,angle = 90, hjust = 1),
      axis.text.y = element_text(size = 14)
    ) +
    guides(color = "none")
  
  ggsave(file.path(output_dir, paste0("bubbleplot_sum_", gsub("#", "_", Classifier), ".png")), plot = p1, width = 10, height = 8, dpi = 300)
  
  # Generate count-based bubble plot
  df_repeated <- bind_rows(
    lapply(group_labels, function(label) {
      data %>%
        count(!!sym(Classifier)) %>%
        mutate(group = label) %>%
        rename(pathway = !!sym(Classifier))
    })
  )
  
  # Plotting the bubble plot with the count values
  p2 <- ggplot(df_repeated, aes(x = pathway, y = group, size = n, color = pathway)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(1, 10)) +
    labs(title = paste("Compound classes -", Classifier, "Count"), x = "Classes", y = "Samples", size = "Count", color = "Pathway") +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"), axis.text.x = element_text(size = 15, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 14)) +
    guides(color = "none")
  
  ggsave(file.path(output_dir, paste0("bubbleplot_count_", gsub( "#","_", Classifier), ".png")), plot = p2, width = 10, height = 8, dpi = 300)
}
