#LIBRARIES:
# Libraries for data manipulation and visualization
library(ggplot2)   # For plotting
library(dplyr) # For data manipulation
library(tidyr)     # For data tidying
library(data.table) # For fread
library(reshape2)  # For melting data frames
library(tidyverse)
library(readxl)
library(readr)
library(sunburstR)
library(stringr)
library(viridisLite)
library(purrr)  
library(parallel)
library(viridis)   # For color scales
library(umap)      # For UMAP dimensionality reduction
library(ggrepel)
library(gridExtra)

#-------------------------------------------------------------------------------

#VARIABLES

#ctrl shft N -> New
#ctrl shft C -> multiple comment
#ctrl shft alt M -> multiple occurences
#ctrl shft M  -> %>% 
#-------------------------------------------------------------------------------

#Files exported from MZmine
#CSV
ms1_file <- "path"
ms2_all_pos <- "C:/Users/ASUS/Desktop/FINALE/1. MZmine/mzmine/ms2_4pos_all.csv"
ms2_all_neg <- "C:/Users/ASUS/Desktop/FINALE/1. MZmine/mzmine/ms2_5neg_all.csv"
onlyms2_pos <- "C:/Users/ASUS/Desktop/FINALE/1. MZmine/mzmine/ms2_4pos_onlyms2.csv"
onlyms2_neg <- "C:/Users/ASUS/Desktop/FINALE/1. MZmine/mzmine/ms2_5neg_onlyms2.csv"

#PREPROCESSED
filtered_ms2_all_pos <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/modified/filtered_ms2_all_pos.csv"
cleaned_ms2_all_pos <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/modified/cleaned_ms2_all_pos.csv"
clustered_ms2_all_pos <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/modified/clustered_ms2_all_pos.csv"

filtered_ms2_all_neg <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/modified/filtered_ms2_all_neg.csv"
cleaned_ms2_all_neg <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/modified/cleaned_ms2_all_neg.csv"
clustered_ms2_all_neg <- "C:/Users/ASUS/Desktop/Finale/1.MZmine exported/modified/clustered_ms2_all_neg.csv"


#mgf
ms2_rawscans_file <- ""
sirius_pos <- "C:/Users/ASUS/Desktop/FINALE/1.MZmine exported/sirius_4pos.mgf"
sirius_neg <- "C:/Users/ASUS/Desktop/FINALE/1.MZmine exported/sirius_5neg.mgf"

#-------------------------------------------------------------------------------
  
#Files exported from sirius
structure_identifications_all_p <- "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/pos/structure_identifications_all.tsv" 
formula_identifications_all_p <-  "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/pos/formula_identifications_all.tsv"
canopus_structure_summary_p <- "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/pos/canopus_structure_summary.tsv"

#Files exported from sirius
structure_identifications_all_n <- "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/neg/structure_identifications_all.tsv" 
formula_identifications_all_n <-  "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/neg/formula_identifications_all.tsv"
canopus_structure_summary_n <- "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/neg/canopus_structure_summary.tsv"
#[[Any other file can be substituted in their place, name doesnot matter but function needs to change based on need


sirius_summary_pos <- "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/pos/structure_summary.csv"
sirius_summary_neg <- "C:/Users/ASUS/Desktop/Finale/2.Sirius exported/neg/structure_summary.csv"
#-------------------------------------------------------------------------------


#Files for comparison : TARGETED
amino_acid_file <- "C:/Users/ASUS/Desktop/FINALE/3.Clara data/2.1AA Levels WT and Semeco Cell Paper.xlsx"
glycerol_intermediates_file <- "C:/Users/ASUS/Desktop/FINALE/3.Clara data/2.2Glycerol and glycolytic intermediates semeco.xlsx"
metabolite_adductmass_details <- "C:/Users/ASUS/Desktop/FINALE/3.Clara data/1.2metabolites semeco_felix_ M+H+ corrected.xlsx"
  
#Supporting files needed to produce plots for multiple features
features_to_plot <- "C:/Users/ASUS/Desktop/FINALE/5.Targeted/targeted_matches_all.csv"




# df <- "C:/Users/ASUS/Desktop/Finale/8. JOINING/1/mzcloud_compt_sirius_pos - Copy.csv"
# df <- fread(df)
# df <- as.data.table(df)
# 
# # Ensure all column names are unique
# setnames(df, make.unique(names(df)))
# 
# df_ <- df %>%
#   rename('id' = 'mappingFeatureId')
# 
# num_unique_ids <- df_%>%
#   summarise(unique_count = n_distinct(id))
# 
# print(num_unique_ids)




# # analysis_script.R

# 
# # Define the filename dynamically based on the argument
# filename <- paste0("C:/Users/ASUS/Desktop/Final/CSV/ms2_all_", script_argument, ".csv")
# 
# # Example: Print the filename to verify
# print(filename)
# 
# # Load the data
# ms1peaks <- fread(filename) %>%
#   mutate(peakcheck = height / area) %>%  # Calculate the peakcheck value
#   select(id, rt, mz, height, area, peakcheck, contains("area"))
# 
# # Continue with the rest of the analysis
# # ...
# # Path to the R script
# script_path <- "C:/Users/ASUS/Desktop/Finale/analysis_script.R"
# 
# # Specify "pos" or "neg"
# script_argument <- "pos"  # Change to "neg" as needed
# 
# # Create a temporary script that sources the main script with the argument
# temp_script <- tempfile(fileext = ".R")
# writeLines(
#   c(
#     paste0('script_argument <- "', script_argument, '"'),
#     paste0('source("', script_path, '")')
#   ),
#   temp_script
# )
# 
# # Run the temporary script
# source(temp_script)
# 
# # Clean up temporary script
# unlink(temp_script)
# 
