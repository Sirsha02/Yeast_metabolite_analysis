#HISTOGRAM TYPE 1

# Define the file path to the MZmine-created CSV file
filename <- ms2_all_neg

# Read the CSV file and process the data
ms1peaks <- fread(filename) %>% 
  select(id, rt, mz)

# Plotting the histogram for mz values
histogram_mz <- ggplot(ms1peaks, aes(x = mz)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Histogram of mz Values", x = "mz", y = "Count") +
  theme_minimal()

# Display the histogram
print(histogram_mz)
png_file <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/hist_mz_neg.png"
ggsave(filename = png_file, plot = histogram_mz, width = 8, height = 6, dpi = 300)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>------------
#HISTOGRAM TYPE 2 ::

# Convert matrix to data frame
hist_data <- as.data.frame(data_file_cluster)

# Calculate the average values for each cluster across columns
hist_data_avg <- hist_data %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean))


hist_data_avg_log <- hist_data_avg %>%
  mutate(across(-cluster, ~ log(.x) / log(10)))  # Logarithm base 10

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>------------
# Filter for Cluster 1 only
cluster_1 <- hist_data_avg_log %>%
  filter(cluster == 9)

# Reshape data for plotting
cluster_1_long <- cluster_1 %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")

# Calculate optimal binwidth using Freedman-Diaconis rule
fd_binwidth <- function(x) {
  IQR_x <- IQR(x, na.rm = TRUE)
  n <- length(x)
  bw <- 2 * IQR_x / (n^(1/3))
  return(bw)
}

# Calculate binwidth for Cluster 1
binwidth_cluster_1 <- fd_binwidth(cluster_1)
print(binwidth_cluster_1)

# Plotting histogram for Cluster 1 with variable binwidth
histogram_cluster_1 <- ggplot(cluster_1_long, aes(x = value)) +
  geom_histogram(binwidth = binwidth_cluster_1, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Cluster 9 average values (log) histogram", x = "Average Values", y = "Count") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),  # Set panel background to white
    plot.background = element_rect(fill = "white")    # Set plot background to white
  )

# Display the histogram for Cluster 1
print(histogram_cluster_1)

png_file <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/cluster_9.png"
ggsave(filename = png_file, plot = histogram_cluster_1, width = 8, height = 6, dpi = 300)

