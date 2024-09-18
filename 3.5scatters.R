
# Step 1: Transpose and convert data_file_cluster_pos to a data frame, adding row names as 'id'
d1 <- averages_df %>%    #[From 3.4]
  as.data.frame()

# Step 2: Convert 'id' column in d1 to character
d1$id <- as.character(d1$id)

# Step 5: Select relevant columns from filtered_file_pos and convert 'id' to character
d2 <- filtered_file_neg %>%
  select(id, mz, rt) %>%
  mutate(id = as.character(id))

# Step 5: Left join d1 and d2 by 'id' column
result <- d1 %>%
  left_join(d2, by = 'id')%>%
  select(id,mz,rt, everything())

# Step 6: Create a named vector for custom names
custom_names <- c(
  "1" = "WT_1",
  "2" = "4P_1",
  "3" = "M3P_1",
  "4" = "WTgive code_2",
  "5" = "4P_2",
  "6" = "M3P_2",
  "7" = "WT_3",
  "8" = "4P_3",
  "9" = "M3P_3"
)

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

#PLOT TYPE 1 - - - - - -

# Step 6: Reshape data to long format for columns 4 to 13
plot_data <- result %>%
  pivot_longer(cols = 4:12, names_to = "intensity_type", values_to = "intensity") %>%
  mutate(intensity_type = recode(intensity_type, !!!custom_names))


# Step 8: Create the plot using facet_wrap with custom names
p1 <- ggplot(plot_data, aes(x = mz, y = rt)) +
  geom_point(aes(size = intensity)) +
  facet_wrap(~ intensity_type, scales = "free") +
  labs(title = "Intensity Plot", x = "m/z", y = "Retention Time", size = "Intensity") +
  white_bg_theme


p1
# PLOT TYPE 2 - - - - - 
# Step 6: Reshape data to long format for columns 4 to 13
plot_data <- result %>%
  pivot_longer(cols = 4:12, names_to = "intensity_type", values_to = "intensity") %>%
  filter(intensity > 0) %>%
  mutate(intensity = log(intensity))%>%
  mutate(intensity_type = recode(intensity_type, !!!custom_names))


# Step 7: Create the plot using facet_wrap with custom names
p2 <-ggplot(plot_data, aes(x = mz, y = intensity)) +
  geom_point() +
  facet_wrap(~ intensity_type, scales = "free_y") +
  labs(title = "Log-transformed Intensity Plot", x = "m/z", y = "Intensity-Log") +
  white_bg_theme



# PLOT TYPE 3 - - - - - 

# Step 7: Create the plot using facet_wrap with custom names
p3 <- ggplot(plot_data, aes(x = rt, y = intensity)) +
  geom_point() +
  facet_wrap(~ intensity_type, scales = "free_y") +
  labs(title = "Log-transformed Intensity Plot", x = "rt", y = "Intensity-Log") +
  white_bg_theme






# Define the directory path
output_dir <- "C:/Users/ASUS/Desktop/FINALE/1.MZmine exported/graphs"

# Create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save plots to PNG files
ggsave(filename = file.path(output_dir, "Intensity,mz,rt_neg.png"), plot = p1, width = 10, height = 8, dpi = 300)
ggsave(filename = file.path(output_dir, "Log_Transformed_Intensity,mz_neg.png"), plot = p2, width = 10, height = 8, dpi = 300)
ggsave(filename = file.path(output_dir, "Log_Transformed_Intensity,rt_neg.png"), plot = p3, width = 10, height = 8, dpi = 300)