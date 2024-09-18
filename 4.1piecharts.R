
#OUTPUT DIRECTORY >>>

# Set a common output directory
output_dir <- "C:/Users/ASUS/Desktop/Finale/2.Sirius exported"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 1. INTERACTIVE PIECHART ------------------------------------------------------

# Read in the data
canopus <- fread(canopus_structure_summary_p) %>%
  select(mappingFeatureId,`NPC#pathway`, `NPC#superclass`, `NPC#class`)

# Rename columns
colnames(canopus) <- c("id","class", "subclass", "sub_subclass")

canopus <- canopus %>%
  mutate(across(everything(), ~ str_replace_all(.x, "-", " ")))

df <- canopus
df$count <- paste(df$class, df$subclass, df$sub_subclass, sep = '-')

df %>%
  select(count) %>%
  group_by(count) %>%
  summarise(Tots = n()) -> dfcount

sb <- sunburst(dfcount)
saveWidget(sb, file = file.path(output_dir, "sunburst_plot_pos.html"), selfcontained = TRUE)

# 2. INTERACTIVE PIECHART EXTENDED ---------------------------------------------

# Read in the data and select the required columns
canopus <- fread(canopus_structure_summary_n) %>%
  select(
    `NPC#pathway`, 
    `NPC#superclass`, 
    `NPC#class`, 
    `ClassyFire#superclass`, 
    `ClassyFire#class`, 
    `ClassyFire#subclass`, 
    `ClassyFire#level 5` 
  )

# Rename columns
colnames(canopus) <- c(
  "class", 
  "subclass", 
  "sub_subclass", 
  "ClassyFire_superclass", 
  "ClassyFire_class", 
  "ClassyFire_subclass", 
  "ClassyFire_level_5" 
)

# Replace '-' with ' ' in all values
canopus <- canopus %>%
  mutate(across(everything(), ~ str_replace_all(.x, "-", " ")))

# Create the count column
canopus <- canopus %>%
  mutate(count = paste(class, subclass, sub_subclass, ClassyFire_superclass, ClassyFire_class, ClassyFire_subclass, ClassyFire_level_5, sep = '-'))

# Group by count and summarize
dfcount <- canopus %>%
  select(count) %>%
  group_by(count) %>%
  summarise(Tots = n())

# Create the sunburst plot
sb <- sunburst(dfcount)

# Save the sunburst plot as an HTML file
saveWidget(sb, file = file.path(output_dir, "sunburst_plot_extended_neg.html"), selfcontained = TRUE)

# 3. NORMAL PIECHART -----------------------------------------------------------

# Read in the data
canopus <- fread(canopus_structure_summary_n)  %>%
  select(`NPC#pathway`, `NPC#superclass`, `NPC#class`)

# Rename columns
colnames(canopus) <- c("class", "subclass", "sub_subclass")

# Create a combined count column
df <- canopus
df$count <- paste(df$class, df$subclass, df$sub_subclass, sep = '-')

# Summarize counts at different levels
# Class level
dfcount_class <- df %>%
  group_by(class) %>%
  summarise(count = n())

# Subclass level
dfcount_subclass <- df %>%
  group_by(class, subclass) %>%
  summarise(count = n())

# Subsubclass level
dfcount_subsubclass <- df %>%
  group_by(class, subclass, sub_subclass) %>%
  summarise(count = n())

# Function to generate pie chart with percentages and save as PNG
generate_pie_chart <- function(data, level_name, filename) {
  # Calculate percentages
  data <- data %>%
    mutate(percent = count / sum(count) * 100)
  
  # Define color palette
  my_colors <- viridisLite::viridis(length(unique(data[[level_name]])))
  
  plot <- ggplot(data, aes(x = "", y = count, fill = !!sym(level_name), label = percent)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = my_colors) +  # Custom color palette
    theme_void() +
    labs(title = paste("Level:", level_name)) +
    geom_text(aes(label = paste0(round(percent), "%")), 
              position = position_stack(vjust = 0.5), 
              size = 2, color = "white", fontface = "bold")  # Adjust text size and color
  
  ggsave(filename, plot = plot, width = 6, height = 6)
}

# Generate and save pie charts
generate_pie_chart(dfcount_class, "class", file.path(output_dir, "pie_chart_class_neg.png"))
generate_pie_chart(dfcount_subclass, "subclass", file.path(output_dir, "pie_chart_subclass_neg.png"))
generate_pie_chart(dfcount_subsubclass, "sub_subclass", file.path(output_dir, "pie_chart_subsubclass_neg.png"))
