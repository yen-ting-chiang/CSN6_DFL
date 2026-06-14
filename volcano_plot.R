# Load necessary libraries for data manipulation and visualization
library(ggplot2)
library(ggrepel)
library(dplyr)
library(readr)
library(tools)

# Register Arial font for Windows system
windowsFonts(Arial = windowsFont("Arial"))

# Configuration variables
# Define the number of top genes to label for both Up and Down groups
num_top_labels <- 5

# Define the base directory containing the .csv files
base_dir <- "C:/Users/danny/Documents/R_project/CSN6_DFL/volcano_plot"

# Find all .csv files in the base directory (non-recursive)
csv_files <- list.files(path = base_dir, pattern = "\\.csv$", full.names = TRUE, recursive = FALSE)

# Define multiple journal-quality color schemes
# "Down" refers to KO/WT Ratio < 0.6666666, "Up" refers to KO/WT Ratio > 1.5
color_schemes <- list(
  Classic = c("Down" = "blue", "Up" = "red", "Not_Sig" = "grey"),
  Nature = c("Down" = "#377eb8", "Up" = "#e41a1c", "Not_Sig" = "#bdbdbd"), # Nature Publishing Group typical palette
  Lancet = c("Down" = "#00468B", "Up" = "#ED0000", "Not_Sig" = "#ADB6B6"), # Lancet typical palette
  Colorblind = c("Down" = "#0072B2", "Up" = "#D55E00", "Not_Sig" = "#999999"), # Wong's colorblind palette
  NEJM = c("Down" = "#0072B5", "Up" = "#E18727", "Not_Sig" = "#BCBCBC"), # New England Journal of Medicine style (Blue/Orange)
  JAMA = c("Down" = "#374E55", "Up" = "#DF8F44", "Not_Sig" = "#B2B2B2"), # Journal of the American Medical Association style (Dark Teal/Orange)
  Cell = c("Down" = "#85B22E", "Up" = "#E29827", "Not_Sig" = "#A3A3A3"), # Cell journal style (Green/Gold)
  Purple_Green = c("Down" = "#1B9E77", "Up" = "#7570B3", "Not_Sig" = "#D9D9D9"), # ColorBrewer Dark2 style (Green/Purple)
  Teal_Orange = c("Down" = "#00A087", "Up" = "#E64B35", "Not_Sig" = "#cccccc"), # AAAS / Science journal style alternative (Teal/Brick Red)
  Screenshot_Style = c("Down" = "#00BFC4", "Up" = "#FFA500", "Not_Sig" = "#B3B3B3"), # Custom Cyan/Orange style based on user screenshot
  Bright_Pink_Green = c("Down" = "#32CD32", "Up" = "#FF1493", "Not_Sig" = "#E0E0E0"), # Bright Lime Green and Deep Pink
  Neon_Magenta_Cyan = c("Down" = "#00E5FF", "Up" = "#FF3399", "Not_Sig" = "#D3D3D3"), # Neon Cyan and Magenta
  Vibrant_Red_Blue = c("Down" = "#3399FF", "Up" = "#FF3333", "Not_Sig" = "#CCCCCC"), # Bright Blue and Red
  Pastel_Coral_Turquoise = c("Down" = "#40E0D0", "Up" = "#FF7F50", "Not_Sig" = "#E6E6E6") # Bright Coral and Turquoise
)

# Loop through each .csv file found in the directory
for (file_path in csv_files) {
  
  # Read the CSV file
  data <- read_csv(file_path, show_col_types = FALSE)
  file_name <- basename(file_path)
  
  # Ensure all necessary columns exist to avoid script termination
  req_cols <- c("p value", "KO/WT Ratio", "Gene name")
  missing_cols <- setdiff(req_cols, colnames(data))
  
  if (length(missing_cols) > 0) {
    warning(sprintf("Missing required columns in %s. Please check your data. Skipping...", file_name))
    next
  }
  
  # Calculate log2 of KO/WT Ratio and -log10 of p value
  data$log2_ratio <- log2(data$`KO/WT Ratio`)
  data$neg_log10_p <- -log10(data$`p value`)
  
  # Categorize points based on significance and fold change boundaries
  data$Significance <- "Not_Sig"
  data$Significance[data$log2_ratio < log2(0.6666666) & data$neg_log10_p > -log10(0.05)] <- "Down"
  data$Significance[data$log2_ratio > log2(1.5) & data$neg_log10_p > -log10(0.05)] <- "Up"
  
  # Convert to factor to ensure consistent color mapping
  data$Significance <- factor(data$Significance, levels = c("Down", "Up", "Not_Sig"))
  
  # Generate labels specifically for the requested points
  if (file_name == "ub_mass_all.csv") {
    if (!("Position" %in% colnames(data))) {
       warning(paste("Missing 'Position' column in", file_name, "for labeling. Using 'Gene name' instead."))
       data$Label_Text <- data$`Gene name`
    } else {
       data$Label_Text <- paste0(data$`Gene name`, "_K", data$Position)
    }
  } else {
    data$Label_Text <- data$`Gene name`
  }
  
  # Filter the top genes with the smallest log2(KO/WT Ratio) within the blue (Down) group
  down_top <- data %>%
    filter(Significance == "Down") %>%
    arrange(log2_ratio) %>%
    head(num_top_labels)
  
  # Filter the top genes with the largest log2(KO/WT Ratio) within the red (Up) group
  up_top <- data %>%
    filter(Significance == "Up") %>%
    arrange(desc(log2_ratio)) %>%
    head(num_top_labels)
  
  # Combine the datasets for labeling
  label_data <- bind_rows(down_top, up_top)
  
  # Generate plots for each color scheme
  for (scheme_name in names(color_schemes)) {
    
    colors <- color_schemes[[scheme_name]]
    
    # Create an output subdirectory for the specific color scheme
    out_dir <- file.path(base_dir, scheme_name)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    
    # Create the volcano plot using ggplot2
    p <- ggplot(data, aes(x = log2_ratio, y = neg_log10_p)) +
      geom_point(aes(color = Significance), alpha = 0.8, size = 1.5) +
      scale_color_manual(values = colors) +
      geom_vline(xintercept = c(log2(0.6666666), log2(1.5)), linetype = "dashed", color = "black", alpha = 0.5) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black", alpha = 0.5) +
      # Use ggrepel to prevent label overlap, using size 7 pt and Arial font
      geom_text_repel(
        data = label_data,
        aes(label = Label_Text),
        size = 7 / .pt, # Convert 7 pt to mm for ggplot2
        family = "Arial",
        color = "black",
        box.padding = 0.5,
        max.overlaps = Inf,
        show.legend = FALSE
      ) +
      theme_classic(base_family = "Arial") +
      labs(
        x = expression("log"[2]*"(KO/WT Ratio)"),
        y = expression("-log"[10]*"(p value)"),
        title = file_path_sans_ext(file_name)
      ) +
      theme(
        text = element_text(family = "Arial", color = "black"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none" # Remove the legend for a cleaner journal look
      )
    
    # Define file names for PDF and TIFF exports
    pdf_out <- file.path(out_dir, paste0(file_path_sans_ext(file_name), "_volcano.pdf"))
    tiff_out <- file.path(out_dir, paste0(file_path_sans_ext(file_name), "_volcano.tiff"))
    
    # Save the plot in high resolution for journal submission
    ggsave(filename = pdf_out, plot = p, width = 3.5, height = 3, units = "in", dpi = 300, device = cairo_pdf)
    ggsave(filename = tiff_out, plot = p, width = 3.5, height = 3, units = "in", dpi = 300, compression = "lzw")
  }
}
