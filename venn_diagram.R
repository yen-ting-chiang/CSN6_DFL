# Install required packages if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("ggvenn", quietly = TRUE)) install.packages("ggvenn")

library(dplyr)
library(openxlsx)
library(ggplot2)
library(ggvenn)

# Set working directory
setwd("C:/Users/danny/Documents/R_project/CSN6_DFL/venn_diagram")

# Read data, ensure column names are read exactly as they are
prot_data <- read.csv("prot_mass_all.csv", stringsAsFactors = FALSE, check.names = FALSE)
prot_data <- prot_data[, !is.na(names(prot_data)) & names(prot_data) != ""]

ub_data <- read.csv("ub_mass_all.csv", stringsAsFactors = FALSE, check.names = FALSE)
ub_data <- ub_data[, !is.na(names(ub_data)) & names(ub_data) != ""]

# Generate list 1: prot_up_list
prot_up_list <- prot_data %>%
  filter(`KO/WT Ratio` > 1.5 & `KO/WT P value` < 0.05 & `p value` < 0.05) %>%
  pull(`Gene name`) %>%
  na.omit() %>%
  unique()
prot_up_list <- prot_up_list[prot_up_list != ""]

# Generate list 2: ub_down_list
ub_down_list <- ub_data %>%
  filter(`KO/WT Ratio` < 0.6666666 & `KO/WT P value` < 0.05 & `p value` < 0.05) %>%
  pull(`Gene name`) %>%
  na.omit() %>%
  unique()
ub_down_list <- ub_down_list[ub_down_list != ""]

# Generate list 3: prot_down_list
prot_down_list <- prot_data %>%
  filter(`KO/WT Ratio` < 0.6666666 & `KO/WT P value` < 0.05 & `p value` < 0.05) %>%
  pull(`Gene name`) %>%
  na.omit() %>%
  unique()
prot_down_list <- prot_down_list[prot_down_list != ""]

# Generate list 4: ub_up_list
ub_up_list <- ub_data %>%
  filter(`KO/WT Ratio` > 1.5 & `KO/WT P value` < 0.05 & `p value` < 0.05) %>%
  pull(`Gene name`) %>%
  na.omit() %>%
  unique()
ub_up_list <- ub_up_list[ub_up_list != ""]

# Helper function to save lists
save_venn_lists <- function(list1, list2, name1, name2, prefix_name) {
  only_1 <- setdiff(list1, list2)
  only_2 <- setdiff(list2, list1)
  intersection_1_2 <- intersect(list1, list2)
  union_1_2 <- union(list1, list2)
  
  max_len <- max(length(only_1), length(only_2), length(intersection_1_2), length(union_1_2), length(list1), length(list2))
  
  pad_na <- function(v, l) { c(v, rep(NA, l - length(v))) }
  
  df_export <- data.frame(
    List1 = pad_na(list1, max_len),
    List2 = pad_na(list2, max_len),
    L1 = pad_na(only_1, max_len),
    L2 = pad_na(only_2, max_len),
    Intersection = pad_na(intersection_1_2, max_len),
    Union = pad_na(union_1_2, max_len),
    stringsAsFactors = FALSE
  )
  
  colnames(df_export) <- c(name1,
                           name2,
                           paste0("Only_in_", name1), 
                           paste0("Only_in_", name2), 
                           "Intersection", 
                           "Union")
  
  write.csv(df_export, paste0(prefix_name, "_all_lists_combined.csv"), row.names = FALSE, na = "")
  write.xlsx(df_export, paste0(prefix_name, "_all_lists_combined.xlsx"))
  
  # Create the region_list subdirectory if it does not exist
  if (!dir.exists("region_list")) {
    dir.create("region_list")
  }
  
  # Output each region as a separate file inside region_list folder
  write.csv(data.frame(Gene_name = list1, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_", name1, ".csv"), row.names = FALSE)
  write.xlsx(data.frame(Gene_name = list1, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_", name1, ".xlsx"))
  
  write.csv(data.frame(Gene_name = list2, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_", name2, ".csv"), row.names = FALSE)
  write.xlsx(data.frame(Gene_name = list2, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_", name2, ".xlsx"))
  
  write.csv(data.frame(Gene_name = only_1, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_Only_in_", name1, ".csv"), row.names = FALSE)
  write.xlsx(data.frame(Gene_name = only_1, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_Only_in_", name1, ".xlsx"))
  
  write.csv(data.frame(Gene_name = only_2, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_Only_in_", name2, ".csv"), row.names = FALSE)
  write.xlsx(data.frame(Gene_name = only_2, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_Only_in_", name2, ".xlsx"))
  
  write.csv(data.frame(Gene_name = intersection_1_2, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_Intersection.csv"), row.names = FALSE)
  write.xlsx(data.frame(Gene_name = intersection_1_2, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_Intersection.xlsx"))
  
  write.csv(data.frame(Gene_name = union_1_2, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_Union.csv"), row.names = FALSE)
  write.xlsx(data.frame(Gene_name = union_1_2, stringsAsFactors = FALSE), paste0("region_list/", prefix_name, "_Union.xlsx"))
}

# Set global ggplot2 text geom defaults to Arial (sans) and bold
update_geom_defaults("text", list(family = "sans", colour = "black", fontface = "bold"))

# Helper function to plot venn diagram using ggvenn
plot_venn_wrapper <- function(list1, list2, name1, name2, color1, color2, prefix_name) {
  
  # Create a named list for ggvenn
  venn_data <- list()
  venn_data[[name1]] <- list1
  venn_data[[name2]] <- list2
  
  # Helper to darken a color for the border
  darken_color <- function(color, factor = 0.7) {
    col <- col2rgb(color)
    col <- col * factor
    rgb(col[1,], col[2,], col[3,], maxColorValue = 255)
  }
  
  # Generate ggvenn plot
  # show_percentage = FALSE ensures only counts are shown
  # size is converted to points by dividing by .pt
  p <- ggvenn(
    venn_data,
    show_percentage = FALSE,
    fill_color = c(color1, color2),
    stroke_color = c(darken_color(color1), darken_color(color2)),
    stroke_size = 0.5,
    set_name_size = 7 / .pt, 
    text_size = 7 / .pt,
    set_name_color = "black",
    text_color = "black"
  ) + 
  theme(text = element_text(family = "sans", color = "black", size = 7, face = "bold"))
  
  # Save as TIFF
  ggsave(filename = paste0(prefix_name, ".tiff"), plot = p, 
         width = 2.5, height = 2.5, units = "in", dpi = 300, compression = "lzw", bg = "white")
  
  # Save as PDF
  ggsave(filename = paste0(prefix_name, ".pdf"), plot = p, 
         width = 2.5, height = 2.5, units = "in", bg = "white")
}

# Plot and save for Venn Diagram 1
save_venn_lists(prot_up_list, ub_down_list, "prot_up_list", "ub_down_list", "Venn1_prot_up_vs_ub_down")
plot_venn_wrapper(prot_up_list, ub_down_list, "prot_up_list", "ub_down_list", "#FFA500", "#32CD32", "Venn1_prot_up_vs_ub_down")

# Plot and save for Venn Diagram 2
save_venn_lists(prot_down_list, ub_up_list, "prot_down_list", "ub_up_list", "Venn2_prot_down_vs_ub_up")
plot_venn_wrapper(prot_down_list, ub_up_list, "prot_down_list", "ub_up_list", "#00BFC4", "#FF1493", "Venn2_prot_down_vs_ub_up")

print("Venn diagrams and list exports completed successfully.")

# ============================================================================
# E3 Target Annotation for all region_list files
# ============================================================================

# Define E3 ligase sheet names (these are also the annotation column names)
e3_sheet_names <- c("MDM2 Targets", "RFWD2 Targets", "UBE3A Targets", 
                    "SKP2 Targets", "FBXW7 Targets", "TRIM21 Targets", 
                    "DCAF1 Targets", "BTRC Targets")

# Read all E3 target sheets into a named list
e3_file <- "E3 Target Summary.xlsx"
e3_data <- list()
for (sheet_name in e3_sheet_names) {
  df <- read.xlsx(e3_file, sheet = sheet_name)
  # Keep only Gene, Confidence, and Validation columns (some sheets have extra columns)
  df <- df[, c("Gene", "Confidence", "Validation")]
  e3_data[[sheet_name]] <- df
}

# Helper function to annotate a gene against one E3 sheet
annotate_gene <- function(gene, e3_df) {
  idx <- which(e3_df$Gene == gene)
  if (length(idx) == 0) {
    return("not_target")
  }
  # Use the first match
  validation <- e3_df$Validation[idx[1]]
  confidence <- e3_df$Confidence[idx[1]]
  # If Validation is "Known" (case-insensitive), return "Known"
  if (!is.na(validation) && toupper(trimws(validation)) == "KNOWN") {
    return("Known")
  }
  # Otherwise return the Confidence value
  if (!is.na(confidence)) {
    return(as.character(confidence))
  }
  return("not_target")
}

# Get all CSV files in the region_list directory (exclude the summary stats file if it already exists)
region_csv_files <- list.files("region_list", pattern = "\\.csv$", full.names = TRUE)
region_csv_files <- region_csv_files[!grepl("Annotation_Summary_Statistics", region_csv_files)]

for (csv_file in region_csv_files) {
  # Read the region list CSV
  region_df <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Skip if Gene_name column does not exist or is empty
  if (!"Gene_name" %in% colnames(region_df) || nrow(region_df) == 0) next
  
  # Annotate with each E3 ligase sheet
  for (sheet_name in e3_sheet_names) {
    e3_df <- e3_data[[sheet_name]]
    region_df[[sheet_name]] <- sapply(region_df$Gene_name, annotate_gene, e3_df = e3_df)
  }
  
  # Overwrite the CSV file with annotated version
  write.csv(region_df, csv_file, row.names = FALSE)
  
  # Overwrite the corresponding XLSX file with annotated version
  xlsx_file <- sub("\\.csv$", ".xlsx", csv_file)
  write.xlsx(region_df, xlsx_file)
}

print("E3 target annotation completed successfully.")

# ============================================================================
# E3 Target Annotation Statistics
# ============================================================================

summary_stats <- data.frame(
  List_Name = character(),
  Target = character(),
  Category = character(),
  Count = integer(),
  Percentage = character(),
  stringsAsFactors = FALSE
)

for (csv_file in region_csv_files) {
  region_df <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
  list_name <- tools::file_path_sans_ext(basename(csv_file))
  
  total_genes <- nrow(region_df)
  if (total_genes == 0) next
  
  for (sheet_name in e3_sheet_names) {
    if (sheet_name %in% colnames(region_df)) {
      cat_counts <- table(region_df[[sheet_name]])
      
      for (cat_name in names(cat_counts)) {
        count <- as.numeric(cat_counts[cat_name])
        percentage <- round((count / total_genes) * 100, 2)
        
        summary_stats <- rbind(summary_stats, data.frame(
          List_Name = list_name,
          Target = sheet_name,
          Category = cat_name,
          Count = count,
          Percentage = paste0(percentage, "%"),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

write.csv(summary_stats, "region_list/Annotation_Summary_Statistics.csv", row.names = FALSE)
write.xlsx(summary_stats, "region_list/Annotation_Summary_Statistics.xlsx")

print("E3 target annotation statistics completed successfully.")

# ============================================================================
# Heatmap Generation for Intersection Lists
# ============================================================================

if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
library(tidyr)

# Create heatmap directory
if (!dir.exists("heatmap")) {
  dir.create("heatmap")
}

# Define multiple bright color palettes
palettes <- list(
  Palette_1_Warm = c("High Confidence" = "#E31A1C", "Middle Confidence" = "#FD8D3C", "Low Confidence" = "#FFEDA0", "Not Annotated" = "#F0F0F0"),
  Palette_2_Cool = c("High Confidence" = "#2171B5", "Middle Confidence" = "#6BAED6", "Low Confidence" = "#C6DBEF", "Not Annotated" = "#F0F0F0"),
  Palette_3_Neon = c("High Confidence" = "#9400D3", "Middle Confidence" = "#FF1493", "Low Confidence" = "#FFB6C1", "Not Annotated" = "#F0F0F0"),
  Palette_4_Emerald = c("High Confidence" = "#238B45", "Middle Confidence" = "#74C476", "Low Confidence" = "#C7E9C0", "Not Annotated" = "#F0F0F0"),
  Palette_5_Mixed1 = c("High Confidence" = "#D94801", "Middle Confidence" = "#2171B5", "Low Confidence" = "#74C476", "Not Annotated" = "#F0F0F0"),
  Palette_6_Mixed2 = c("High Confidence" = "#6A51A3", "Middle Confidence" = "#EF3B2C", "Low Confidence" = "#FDBB84", "Not Annotated" = "#F0F0F0"),
  Palette_7_Vibrant = c("High Confidence" = "#E31A1C", "Middle Confidence" = "#00BFC4", "Low Confidence" = "#F8766D", "Not Annotated" = "#F0F0F0"),
  Palette_8_Pop = c("High Confidence" = "#FF1493", "Middle Confidence" = "#00FF00", "Low Confidence" = "#00FFFF", "Not Annotated" = "#F0F0F0"),
  Palette_9_Viridis = c("High Confidence" = "#FDE725", "Middle Confidence" = "#21918C", "Low Confidence" = "#440154", "Not Annotated" = "#F0F0F0"),
  Palette_10_Plasma = c("High Confidence" = "#F0F921", "Middle Confidence" = "#CC4678", "Low Confidence" = "#0D0887", "Not Annotated" = "#F0F0F0"),
  Palette_11_Spectral = c("High Confidence" = "#D53E4F", "Middle Confidence" = "#FEE08B", "Low Confidence" = "#3288BD", "Not Annotated" = "#F0F0F0"),
  Palette_12_Nature = c("High Confidence" = "#E64B35", "Middle Confidence" = "#4DBBD5", "Low Confidence" = "#00A087", "Not Annotated" = "#F0F0F0"),
  Palette_13_Lancet = c("High Confidence" = "#ED0000", "Middle Confidence" = "#00468B", "Low Confidence" = "#42B540", "Not Annotated" = "#F0F0F0"),
  Palette_14_JAMA = c("High Confidence" = "#DF8F44", "Middle Confidence" = "#374E55", "Low Confidence" = "#00A1D5", "Not Annotated" = "#F0F0F0")
)

# Helper function to create and save heatmaps
plot_intersection_heatmap <- function(csv_path, y_targets, prefix_name, max_width = 7.8) {
  if (!file.exists(csv_path)) {
    warning(paste("File does not exist:", csv_path))
    return()
  }
  
  df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(df) == 0) return()
  
  # Select only Gene_name and the specified y_targets
  keep_cols <- c("Gene_name", y_targets)
  
  # Check if all y_targets exist in df, if not, fill with 'not_target'
  missing_targets <- setdiff(y_targets, colnames(df))
  if (length(missing_targets) > 0) {
    for (mt in missing_targets) {
      df[[mt]] <- "not_target"
    }
  }
  
  df_sub <- df[, keep_cols, drop = FALSE]
  
  # Reshape to long format
  df_long <- pivot_longer(df_sub, cols = -Gene_name, names_to = "Target", values_to = "Annotation")
  
  # Map annotation labels to new names
  df_long$Annotation[df_long$Annotation == "HIGH"] <- "High Confidence"
  df_long$Annotation[df_long$Annotation == "MIDDLE"] <- "Middle Confidence"
  df_long$Annotation[df_long$Annotation == "LOW"] <- "Low Confidence"
  df_long$Annotation[df_long$Annotation == "not_target"] <- "Not Annotated"
  
  # Remove 'Known' if it exists, as requested
  df_long <- df_long[df_long$Annotation != "Known" & df_long$Annotation != "KNOWN", ]
  
  # Ensure Gene_name is ordered alphabetically
  df_long$Gene_name <- factor(df_long$Gene_name, levels = sort(unique(as.character(df_long$Gene_name))))
  
  # Set factors for Y axis to plot top to bottom
  df_long$Target <- factor(df_long$Target, levels = rev(y_targets))
  
  # Set factors for Annotation to control legend order
  ann_levels <- c("High Confidence", "Middle Confidence", "Low Confidence", "Not Annotated")
  df_long$Annotation <- factor(df_long$Annotation, levels = ann_levels)
  
  # Calculate dimensions based on number of genes and targets, capped at max_width
  w <- min(max_width, max(4, length(unique(df_long$Gene_name)) * 0.25 + 2))
  # Formula: Total Height = (Number of Targets * Row Height) + Fixed Margin/Legend Height
  # This guarantees mathematically identical row heights across all heatmaps.
  h <- length(y_targets) * 0.4 + 1.5
  
  # Generate plots for each palette
  for (pal_name in names(palettes)) {
    p <- ggplot(df_long, aes(x = Gene_name, y = Target, fill = Annotation)) +
      geom_tile(color = "white") +
      scale_fill_manual(values = palettes[[pal_name]], drop = FALSE) +
      theme_minimal() +
      labs(x = "Gene Name", y = "E3 Targets", fill = "Annotation") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, family = "sans", size = 7, color = "black"),
        axis.text.y = element_text(family = "sans", size = 7, color = "black"),
        axis.title = element_text(family = "sans", size = 7, color = "black"),
        legend.title = element_text(family = "sans", size = 7, color = "black"),
        legend.text = element_text(family = "sans", size = 7, color = "black"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid = element_blank()
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top", title.hjust = 0.5))
    
    # Save TIFF
    tiff_file <- paste0("heatmap/", prefix_name, "_", pal_name, ".tiff")
    ggsave(filename = tiff_file, plot = p, width = w, height = h, units = "in", dpi = 300, compression = "lzw", bg = "white")
    
    # Save PDF
    pdf_file <- paste0("heatmap/", prefix_name, "_", pal_name, ".pdf")
    ggsave(filename = pdf_file, plot = p, width = w, height = h, units = "in", bg = "white")
  }
}

# 1. Venn 1 Heatmap
v1_csv <- "region_list/Venn1_prot_up_vs_ub_down_Intersection.csv"
v1_targets <- c("MDM2 Targets", "RFWD2 Targets", "UBE3A Targets")
plot_intersection_heatmap(v1_csv, v1_targets, "Heatmap_Venn1_Intersection")

# 2. Venn 2 Heatmap
v2_csv <- "region_list/Venn2_prot_down_vs_ub_up_Intersection.csv"
v2_targets <- c("FBXW7 Targets", "TRIM21 Targets", "DCAF1 Targets", "BTRC Targets")
plot_intersection_heatmap(v2_csv, v2_targets, "Heatmap_Venn2_Intersection", max_width = 4.0)

print("Heatmap generation completed successfully.")
