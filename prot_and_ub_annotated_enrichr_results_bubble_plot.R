# prot_and_ub_annotated_enrichr_results_bubble_plot.R
#
# Purpose:
#   Generate bubble plots from annotated Enrichr results (.xlsx files) located
#   in the prot_and_ub_annotated_enrichr_results directory.
#
#   For each .xlsx file and each worksheet within it, the script:
#     1. Detects rows where the "Term" column cell has a background fill color
#        (i.e., highlighted/annotated rows).
#     2. If no rows are highlighted in a worksheet, that worksheet is skipped.
#     3. Creates a bubble plot for the highlighted rows only.
#
#   Bubble plot specifications:
#     - x-axis: Odds Ratio
#     - y-axis: Term, ordered by -log10(Adjusted P-value) from largest to smallest
#     - Bubble color: -log10(Adjusted P-value)
#     - Bubble size: Count (number of genes in the "Genes" column, separated by ";")
#
#   Plot style is adapted from the ORA bubble plot in:
#     phos_with_PN_DPS_meta_vs_phos_without_PN_DPS_meta_venn_and_ORA.R
#
# Input:
#   - prot_and_ub_annotated_enrichr_results/*.xlsx
#     Each .xlsx file contains 4 worksheets with Enrichr output columns:
#       Term, Overlap, P-value, Adjusted.P-value, Old.P-value,
#       Old.Adjusted.P-value, Odds.Ratio, Combined.Score, Genes
#
# Output:
#   - prot_and_ub_annotated_enrichr_results/bubble_plot/
#       {xlsx_basename}_{sheet_name}_bubble.pdf
#       {xlsx_basename}_{sheet_name}_bubble.tiff

# Load necessary libraries
library(openxlsx)
library(ggplot2)
library(dplyr)
library(grid)
library(gtable)

# ==============================================================================
# USER CONFIGURATION: Bubble Plot Color Palettes
# ==============================================================================
# Select a color gradient for bubble plot -log10(Adjusted P-value) mapping.
# The gradient runs from low -log10(padj) (least significant) to
# high -log10(padj) (most significant).
# Change palette_bubble_choice to switch palette.
#
# Available palettes:
#   1 = NPG Red to Blue         : low="#E64B35", high="#4DBBD5"
#   2 = Lancet Red to Teal      : low="#ED0000", high="#0099B4"
#   3 = NEJM Red to Blue        : low="#BC3C29", high="#0072B5"
#   4 = JAMA Warm to Cool       : low="#DF8F44", high="#374E55"
#   5 = JCO Gold to Blue        : low="#EFC000", high="#0073C2"
#   6 = Viridis-inspired        : low="#FDE725", high="#440154"
#   7 = Magma-inspired          : low="#FCFDBF", high="#000004"
#   8 = Nature Red to Green     : low="#E64B35", high="#00A087"
#   9 = Orange to Teal          : low="#FF8C00", high="#008080"
#  10 = Yellow-Green to Purple  : low="#9ACD32", high="#9932CC"

palette_bubble_choices <- c(10)  # Provide a vector of numbers to select multiple bubble palettes (e.g., c(1, 6, 8, 9, 10))

bubble_palettes <- list(
  c("#E64B35", "#4DBBD5"),  # 1: NPG Red to Blue
  c("#ED0000", "#0099B4"),  # 2: Lancet Red to Teal
  c("#BC3C29", "#0072B5"),  # 3: NEJM Red to Blue
  c("#DF8F44", "#374E55"),  # 4: JAMA Warm to Cool
  c("#EFC000", "#0073C2"),  # 5: JCO Gold to Blue
  c("#FDE725", "#440154"),  # 6: Viridis-inspired
  c("#FCFDBF", "#000004"),  # 7: Magma-inspired
  c("#E64B35", "#00A087"),  # 8: Nature Red to Green
  c("#FF8C00", "#008080"),  # 9: Orange to Teal
  c("#9ACD32", "#9932CC")   # 10: Yellow-Green to Purple
)

palette_names <- c(
  "1_NPG_Red_to_Blue",
  "2_Lancet_Red_to_Teal",
  "3_NEJM_Red_to_Blue",
  "4_JAMA_Warm_to_Cool",
  "5_JCO_Gold_to_Blue",
  "6_Viridis_inspired",
  "7_Magma_inspired",
  "8_Nature_Red_to_Green",
  "9_Orange_to_Teal",
  "10_YellowGreen_to_Purple"
)

cat("Selected bubble plot palettes:", paste(palette_bubble_choices, collapse = ", "), "\n")

# ==============================================================================
# USER CONFIGURATION: Panel (Plot Area) Dimensions
# ==============================================================================
# Panel width is fixed. Panel height scales with the number of y-axis labels
# (terms) using height_per_term, so plots with more terms are proportionally
# taller. The total output size is automatically calculated to accommodate
# all y-axis labels, legends, and titles at their current font sizes.

fixed_panel_width <- 2.0    # inches (width of the bubble plot area only)
height_per_term   <- 0.18   # inches per y-axis label (term)
min_panel_height  <- 1.2    # inches (minimum panel height to prevent legend
                            #         clipping when only a few terms exist)

cat("Panel width:", fixed_panel_width, "in | Height per term:",
    height_per_term, "in | Min panel height:", min_panel_height, "in\n")

# ==============================================================================
# Setup: Paths
# ==============================================================================

input_dir  <- "C:/Users/danny/Documents/R_project/CSN6_DFL/prot_and_ub_annotated_enrichr_results"
output_dir <- file.path(input_dir, "bubble_plot")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ==============================================================================
# Helper Function: Get green-highlighted Term row indices from a worksheet
# ==============================================================================
# This function examines the styleObjects of a loaded workbook to find rows
# where the "Term" column (column 1) has a GREEN fill foreground color
# (hex: #C6EFCE / FFC6EFCE), indicating user-selected terms for plotting.
# Rows with other fill colors (e.g., yellow #FFEB9C) are ignored.
#
# Args:
#   wb:         A loaded openxlsx Workbook object
#   sheet_name: The name of the worksheet to check
#
# Returns:
#   A numeric vector of data row indices (1-indexed, where 1 = first data row,
#   corresponding to Excel row 2) that have a green fill on the Term cell.

get_highlighted_term_rows <- function(wb, sheet_name) {
  # Green fill color used in the annotated Enrichr results
  # (Excel "Good" conditional format: #C6EFCE, stored as FFC6EFCE with alpha)
  green_fill_hex <- "FFC6EFCE"

  style_objects <- wb[["styleObjects"]]
  highlighted_excel_rows <- c()

  for (so in style_objects) {
    # Check if this style object belongs to the target sheet
    if (so$sheet != sheet_name) next

    # Check if this style has a fill foreground color
    fill_fg <- so$style$fill$fillFg
    if (is.null(fill_fg)) next

    # Only include rows with green fill color; skip other colors
    fill_fg_str <- toupper(paste0(format(fill_fg), collapse = ""))
    if (!grepl(green_fill_hex, fill_fg_str, fixed = TRUE)) next

    # Find rows in this style object where column == 1 (Term column)
    # and row > 1 (exclude header row)
    rows_vec <- so$rows
    cols_vec <- so$cols
    term_rows <- rows_vec[cols_vec == 1 & rows_vec > 1]

    if (length(term_rows) > 0) {
      highlighted_excel_rows <- c(highlighted_excel_rows, term_rows)
    }
  }

  # Remove duplicates and sort
  highlighted_excel_rows <- sort(unique(highlighted_excel_rows))

  # Convert from Excel row numbers (header = row 1) to data row indices
  # Excel row 2 = data row 1, Excel row 3 = data row 2, etc.
  data_row_indices <- highlighted_excel_rows - 1

  return(data_row_indices)
}

# ==============================================================================
# Helper Function: Count genes in the Genes column
# ==============================================================================
# Genes are separated by ";" in the Genes column.
#
# Args:
#   genes_str: A character string containing genes separated by ";"
#
# Returns:
#   An integer count of genes

count_genes <- function(genes_str) {
  if (is.na(genes_str) || genes_str == "") return(0L)
  genes <- unlist(strsplit(genes_str, ";"))
  # Trim whitespace and remove empty strings
  genes <- trimws(genes)
  genes <- genes[genes != ""]
  return(length(genes))
}

# ==============================================================================
# Main Processing Loop
# ==============================================================================

# Get all .xlsx files in the input directory
xlsx_files <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)

if (length(xlsx_files) == 0) {
  stop("No .xlsx files found in: ", input_dir)
}

cat("\nFound", length(xlsx_files), ".xlsx files to process.\n\n")

for (xlsx_path in xlsx_files) {
  xlsx_basename <- tools::file_path_sans_ext(basename(xlsx_path))
  cat("Processing:", xlsx_basename, "\n")

  # Load workbook (needed for style detection)
  wb <- loadWorkbook(xlsx_path)
  sheet_names <- getSheetNames(xlsx_path)

  for (sheet_name in sheet_names) {
    cat("  Sheet:", sheet_name, "\n")

    # Read data from the sheet
    df <- read.xlsx(xlsx_path, sheet = sheet_name)

    if (is.null(df) || nrow(df) == 0) {
      cat("    -> Empty sheet, skipping.\n")
      next
    }

    # Get highlighted row indices (1-indexed data rows)
    highlighted_rows <- get_highlighted_term_rows(wb, sheet_name)

    if (length(highlighted_rows) == 0) {
      cat("    -> No highlighted Term rows found, skipping.\n")
      next
    }

    cat("    -> Found", length(highlighted_rows), "highlighted rows.\n")

    # Filter to only highlighted rows
    plot_df <- df[highlighted_rows, , drop = FALSE]

    # Validate required columns exist
    required_cols <- c("Term", "Adjusted.P-value", "Odds.Ratio", "Genes")
    # Also check for alternative column names with dots replacing special chars
    col_names <- colnames(plot_df)

    # Map column names (openxlsx may convert special characters)
    term_col    <- "Term"
    padj_col    <- grep("^Adjusted[.]P", col_names, value = TRUE)[1]
    odds_col    <- grep("^Odds[.]Ratio", col_names, value = TRUE)[1]
    genes_col   <- "Genes"

    if (is.na(padj_col) || is.na(odds_col)) {
      cat("    -> Required columns not found, skipping.\n")
      cat("       Available columns:", paste(col_names, collapse = ", "), "\n")
      next
    }

    # Calculate Count (number of genes per row)
    plot_df$Count <- sapply(plot_df[[genes_col]], count_genes)

    # Calculate -log10(Adjusted P-value)
    plot_df$neg_log10_padj <- -log10(as.numeric(plot_df[[padj_col]]))

    # Get Odds Ratio as numeric
    plot_df$OddsRatio <- as.numeric(plot_df[[odds_col]])

    # Remove rows with NA in essential columns
    plot_df <- plot_df[!is.na(plot_df$neg_log10_padj) &
                       !is.na(plot_df$OddsRatio) &
                       !is.na(plot_df$Term), ]

    if (nrow(plot_df) == 0) {
      cat("    -> No valid data after filtering, skipping.\n")
      next
    }

    # Clean Term labels based on sheet type for cleaner y-axis display
    # WikiPathways: remove trailing " WP####" identifiers
    if (grepl("WikiPathways", sheet_name, ignore.case = TRUE)) {
      plot_df$Term <- trimws(gsub("\\s*WP\\d+$", "", plot_df$Term))
    }
    # GO Biological Process: remove trailing " (GO:nnnnnnn)" identifiers
    if (grepl("GO_Biological_Process", sheet_name, ignore.case = TRUE)) {
      plot_df$Term <- trimws(gsub("\\s*\\(GO:\\d+\\)$", "", plot_df$Term))
    }

    # Order Term factor by -log10(Adjusted P-value) descending
    # (largest at top of y-axis)
    plot_df <- plot_df[order(plot_df$neg_log10_padj), ]
    plot_df$Term <- factor(plot_df$Term, levels = plot_df$Term)

    # Control plot title visibility (TRUE to show, FALSE to hide)
    show_plot_title <- TRUE
    plot_title_text <- if (show_plot_title) {
      paste0(xlsx_basename, "\n", sheet_name)
    } else {
      NULL
    }

    for (palette_choice in palette_bubble_choices) {
      bubble_color_low  <- bubble_palettes[[palette_choice]][1]
      bubble_color_high <- bubble_palettes[[palette_choice]][2]

      # Bubble plot (adapted from ORA bubble plot style)
      p <- ggplot(plot_df, aes(x = OddsRatio, y = Term)) +
        geom_point(aes(size = Count, color = neg_log10_padj)) +
        scale_color_gradient(low = bubble_color_high, high = bubble_color_low,
                             name = expression(-log[10](Adjusted~P-value))) +
        labs(title = plot_title_text, x = "Odds Ratio", y = "") +
        scale_x_continuous(expand = expansion(mult = 0.15)) +
        scale_y_discrete(expand = expansion(mult = 0.1)) +
        theme_bw() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          axis.line = element_line(color = "black"),
          axis.text.y = element_text(size = 7, face = "bold", color = "black"),
          axis.text.x = element_text(size = 7, face = "bold", color = "black"),
          axis.title = element_text(size = 7, face = "bold", color = "black"),
          plot.title = element_text(size = 7, face = "bold", hjust = 0.5,
                                    color = "black"),
          legend.position = "right",
          legend.box.margin = margin(0, 0, 0, 10),
          legend.title = element_text(size = 7, face = "bold", color = "black"),
          legend.text = element_text(size = 7, color = "black"),
          legend.key.size = unit(0.3, "cm")
        )

      if (!show_plot_title) {
        p <- p + theme(plot.title = element_blank())
      }

      # Generate output filename
      # Clean sheet name to be filesystem-safe
      safe_sheet_name <- gsub("[^A-Za-z0-9_]", "_", sheet_name)
      file_prefix <- paste0(xlsx_basename, "_", safe_sheet_name)

      # Create specific subdirectory for this palette
      palette_dir <- file.path(output_dir, palette_names[palette_choice])
      if (!dir.exists(palette_dir)) {
        dir.create(palette_dir, recursive = TRUE)
      }

      # Convert ggplot to gtable and set panel dimensions
      # Panel width is fixed; panel height scales with the number of terms,
      # with a minimum to prevent legend clipping for plots with few terms.
      dynamic_panel_height <- max(nrow(plot_df) * height_per_term,
                                  min_panel_height)
      g <- ggplotGrob(p)
      panel_pos <- g$layout[g$layout$name == "panel", ]
      g$widths[panel_pos$l]  <- unit(fixed_panel_width, "in")
      g$heights[panel_pos$t] <- unit(dynamic_panel_height, "in")

      # Measure total output dimensions accurately using a temporary PDF device.
      # Text-based grob sizes (axis labels, legend, title) can only be resolved
      # correctly inside an active graphics device context.
      tmp_pdf <- tempfile(fileext = ".pdf")
      pdf(tmp_pdf, width = 20, height = 20)
      grid.newpage()
      grid.draw(g)

      # Check if the legend is taller than the panel. If so, increase the panel
      # height to match the legend so nothing gets clipped.
      legend_rows <- g$layout[grepl("guide-box", g$layout$name), ]
      if (nrow(legend_rows) > 0) {
        legend_height <- convertHeight(
          sum(g$heights[seq(min(legend_rows$t), max(legend_rows$b))]),
          "in", valueOnly = TRUE
        )
        if (legend_height > dynamic_panel_height) {
          dynamic_panel_height <- legend_height
          g$heights[panel_pos$t] <- unit(dynamic_panel_height, "in")
        }
      }

      total_width  <- convertWidth(sum(g$widths), "in", valueOnly = TRUE)
      total_height <- convertHeight(sum(g$heights), "in", valueOnly = TRUE)
      dev.off()
      unlink(tmp_pdf)

      # Save as PDF with accurately measured dimensions
      pdf(file.path(palette_dir, paste0(file_prefix, "_bubble.pdf")),
          width = total_width, height = total_height)
      grid.newpage()
      grid.draw(g)
      dev.off()

      # Save as TIFF with accurately measured dimensions
      tiff(file.path(palette_dir, paste0(file_prefix, "_bubble.tiff")),
           width = total_width, height = total_height, units = "in", res = 300,
           compression = "lzw")
      grid.newpage()
      grid.draw(g)
      dev.off()

      cat("    -> Bubble plot saved:", file_prefix,
          "(panel:", fixed_panel_width, "x", round(dynamic_panel_height, 2),
          "in, total:", round(total_width, 2), "x",
          round(total_height, 2), "in)\n")
    }
  }

  cat("\n")
}

cat("All bubble plots have been generated successfully.\n")
cat("Output directory:", output_dir, "\n")

# ==============================================================================
# PART 2: Enrichment Map (emapplot)
# ==============================================================================
# For each .xlsx file and each worksheet, build an enrichment map (network) from
# the same green-highlighted terms used for bubble plots. Nodes represent enriched
# terms; node size reflects gene count, node color reflects -log10(Adjusted P-value).
# Edges connect terms that share genes, weighted by Jaccard similarity.
# Only edges above the similarity threshold are drawn.
#
# Uses igraph + ggraph for network layout and visualization (more flexible than
# clusterProfiler::emapplot since the data originates from external Enrichr results).
#
# Output:
#   prot_and_ub_annotated_enrichr_results/emapplot/
#     {xlsx_basename}_{sheet_name}_emapplot.pdf
#     {xlsx_basename}_{sheet_name}_emapplot.tiff

library(igraph)
library(ggraph)

# ==============================================================================
# USER CONFIGURATION: Enrichment Map Parameters
# ==============================================================================

# Jaccard similarity threshold: edges are drawn only between term pairs whose
# Jaccard index (shared genes / union of genes) exceeds this value.
# Lower values show more edges; higher values show only strongly related terms.
jaccard_threshold <- 0.1

# Enrichment map node color palette (same numbering as bubble palettes above)
# Provide a vector of numbers to select multiple emapplot palettes.
emapplot_palette_choices <- c(1,2,3,4,5,6,7,8,9,10)

# Enrichment map dynamic sizing parameters (inches)
# The plot area scales with sqrt(n_terms) so that nodes and their labels have
# enough room to spread out without overlapping. The total output size is then
# measured from a temporary render to accommodate legend and title.
emapplot_spacing_per_node <- 0.7  # inches of spacing per sqrt(n_terms)
emapplot_min_panel_size   <- 2.2  # minimum panel width or height (inches)
emapplot_label_font_size  <- 3  # pt (must match geom_node_text size below)

cat("\n")
cat("========================================\n")
cat("Part 2: Enrichment Map (emapplot)\n")
cat("========================================\n")
cat("Jaccard threshold:", jaccard_threshold, "\n")
cat("Emapplot palettes:", paste(emapplot_palette_choices, collapse = ", "), "\n")

# Setup output directory for emapplot
emapplot_output_dir <- file.path(input_dir, "emapplot")
if (!dir.exists(emapplot_output_dir)) {
  dir.create(emapplot_output_dir, recursive = TRUE)
}

# ==============================================================================
# Helper Function: Compute Jaccard similarity between two gene sets
# ==============================================================================
jaccard_similarity <- function(set_a, set_b) {
  intersection_size <- length(intersect(set_a, set_b))
  union_size <- length(union(set_a, set_b))
  if (union_size == 0) return(0)
  return(intersection_size / union_size)
}

# ==============================================================================
# Helper Function: Parse genes string into a character vector
# ==============================================================================
parse_genes <- function(genes_str) {
  if (is.na(genes_str) || genes_str == "") return(character(0))
  genes <- unlist(strsplit(genes_str, ";"))
  genes <- trimws(genes)
  genes <- genes[genes != ""]
  return(genes)
}

# ==============================================================================
# Enrichment Map Processing Loop
# ==============================================================================

for (xlsx_path in xlsx_files) {
  xlsx_basename <- tools::file_path_sans_ext(basename(xlsx_path))
  cat("\nProcessing (emapplot):", xlsx_basename, "\n")

  wb <- loadWorkbook(xlsx_path)
  sheet_names <- getSheetNames(xlsx_path)

  for (sheet_name in sheet_names) {
    cat("  Sheet:", sheet_name, "\n")

    df <- read.xlsx(xlsx_path, sheet = sheet_name)

    if (is.null(df) || nrow(df) == 0) {
      cat("    -> Empty sheet, skipping.\n")
      next
    }

    highlighted_rows <- get_highlighted_term_rows(wb, sheet_name)

    if (length(highlighted_rows) == 0) {
      cat("    -> No highlighted Term rows found, skipping.\n")
      next
    }

    emap_df <- df[highlighted_rows, , drop = FALSE]

    # Map column names
    col_names <- colnames(emap_df)
    term_col  <- "Term"
    padj_col  <- grep("^Adjusted[.]P", col_names, value = TRUE)[1]
    odds_col  <- grep("^Odds[.]Ratio", col_names, value = TRUE)[1]
    genes_col <- "Genes"

    if (is.na(padj_col) || is.na(odds_col)) {
      cat("    -> Required columns not found, skipping.\n")
      next
    }

    emap_df$Count <- sapply(emap_df[[genes_col]], count_genes)
    emap_df$neg_log10_padj <- -log10(as.numeric(emap_df[[padj_col]]))
    emap_df$OddsRatio <- as.numeric(emap_df[[odds_col]])

    emap_df <- emap_df[!is.na(emap_df$neg_log10_padj) &
                        !is.na(emap_df$OddsRatio) &
                        !is.na(emap_df$Term), ]

    if (nrow(emap_df) < 2) {
      cat("    -> Less than 2 valid terms, skipping emapplot.\n")
      next
    }

    # Clean Term labels (same rules as bubble plots)
    if (grepl("WikiPathways", sheet_name, ignore.case = TRUE)) {
      emap_df$Term <- trimws(gsub("\\s*WP\\d+$", "", emap_df$Term))
    }
    if (grepl("GO_Biological_Process", sheet_name, ignore.case = TRUE)) {
      emap_df$Term <- trimws(gsub("\\s*\\(GO:\\d+\\)$", "", emap_df$Term))
    }

    # Parse gene lists for each term
    gene_lists <- lapply(emap_df[[genes_col]], parse_genes)
    names(gene_lists) <- emap_df$Term

    # Compute pairwise Jaccard similarity matrix
    n_terms <- nrow(emap_df)
    terms <- emap_df$Term

    edge_list <- data.frame(from = character(0), to = character(0),
                            similarity = numeric(0),
                            stringsAsFactors = FALSE)

    for (i in seq_len(n_terms - 1)) {
      for (j in (i + 1):n_terms) {
        jac <- jaccard_similarity(gene_lists[[i]], gene_lists[[j]])
        if (jac >= jaccard_threshold) {
          edge_list <- rbind(edge_list, data.frame(
            from = terms[i], to = terms[j], similarity = jac,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    # Build igraph network
    # Create node data frame
    node_df <- data.frame(
      name = emap_df$Term,
      Count = emap_df$Count,
      neg_log10_padj = emap_df$neg_log10_padj,
      stringsAsFactors = FALSE
    )

    if (nrow(edge_list) > 0) {
      g_network <- graph_from_data_frame(edge_list, directed = FALSE,
                                          vertices = node_df)
    } else {
      # No edges above threshold: create graph with only nodes
      g_network <- graph_from_data_frame(
        data.frame(from = character(0), to = character(0)),
        directed = FALSE, vertices = node_df
      )
    }

    # Generate output filename
    safe_sheet_name <- gsub("[^A-Za-z0-9_]", "_", sheet_name)
    emap_file_prefix <- paste0(xlsx_basename, "_", safe_sheet_name)

    for (palette_choice in emapplot_palette_choices) {
      emap_color_low  <- bubble_palettes[[palette_choice]][1]
      emap_color_high <- bubble_palettes[[palette_choice]][2]

      # Create specific subdirectory for this palette
      palette_dir <- file.path(emapplot_output_dir, palette_names[palette_choice])
      if (!dir.exists(palette_dir)) {
        dir.create(palette_dir, recursive = TRUE)
      }

      # Calculate dynamic panel dimensions based on term count and label length.
      # Network nodes spread in 2D, so area scales with n_terms.
      # Extra width is added for the longest label text.
      max_label_nchar <- max(nchar(as.character(emap_df$Term)))
      # Estimate longest label width: ~0.07 inches per character at size 2.5
      label_width_est <- max_label_nchar * 0.084
      base_panel_size <- max(emapplot_min_panel_size,
                             sqrt(n_terms) * emapplot_spacing_per_node)
      dynamic_panel_width  <- base_panel_size + label_width_est
      dynamic_panel_height <- base_panel_size

      # Build emapplot using ggraph
      set.seed(42)  # Reproducible layout
      emap_plot <- ggraph(g_network, layout = "fr") +
        {if (nrow(edge_list) > 0)
          geom_edge_link(aes(width = similarity), alpha = 0.4,
                         color = "grey60", show.legend = FALSE)
        } +
        {if (nrow(edge_list) > 0)
          scale_edge_width(range = c(0.3, 2.5))
        } +
        geom_node_point(aes(size = Count, color = neg_log10_padj)) +
        geom_node_text(aes(label = name), repel = TRUE,
                       size = emapplot_label_font_size,
                       max.overlaps = 20) +
        scale_color_gradient(low = emap_color_high, high = emap_color_low,
                             name = expression(-log[10](Adjusted~P-value))) +
        scale_size_continuous(name = "Count", range = c(2, 6)) +
        labs(title = paste0(xlsx_basename, "\n", sheet_name)) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(size = 9, face = "bold", hjust = 0.5,
                                    color = "black"),
          legend.title = element_text(size = 7, face = "bold", color = "black"),
          legend.text = element_text(size = 7, color = "black"),
          legend.position = "right",
          legend.key.size = unit(0.4, "cm"),
          legend.spacing.y = unit(0.1, "cm"),
          plot.margin = margin(10, 10, 10, 10)
        )

      # Convert to gtable and set dynamic panel dimensions
      g_emap <- ggplotGrob(emap_plot)
      panel_pos_emap <- g_emap$layout[g_emap$layout$name == "panel", ]
      g_emap$widths[panel_pos_emap$l]  <- unit(dynamic_panel_width, "in")
      g_emap$heights[panel_pos_emap$t] <- unit(dynamic_panel_height, "in")

      # Measure total output dimensions using a temporary PDF device
      tmp_pdf <- tempfile(fileext = ".pdf")
      pdf(tmp_pdf, width = 20, height = 20)
      grid.newpage()
      grid.draw(g_emap)

      # Check if the legend is taller than the panel. If so, increase the panel
      # height to match the legend so nothing gets clipped.
      legend_rows_emap <- g_emap$layout[grepl("guide-box", g_emap$layout$name), ]
      if (nrow(legend_rows_emap) > 0) {
        legend_height_emap <- convertHeight(
          sum(g_emap$heights[seq(min(legend_rows_emap$t),
                                 max(legend_rows_emap$b))]),
          "in", valueOnly = TRUE
        )
        if (legend_height_emap > dynamic_panel_height) {
          dynamic_panel_height <- legend_height_emap
          g_emap$heights[panel_pos_emap$t] <- unit(dynamic_panel_height, "in")
        }
      }

      total_emap_width  <- convertWidth(sum(g_emap$widths), "in",
                                         valueOnly = TRUE)
      total_emap_height <- convertHeight(sum(g_emap$heights), "in",
                                          valueOnly = TRUE)
      dev.off()
      unlink(tmp_pdf)

      # Save as PDF with accurately measured dimensions
      pdf(file.path(palette_dir,
                    paste0(emap_file_prefix, "_emapplot.pdf")),
          width = total_emap_width, height = total_emap_height)
      grid.newpage()
      grid.draw(g_emap)
      dev.off()

      # Save as TIFF with accurately measured dimensions
      tiff(file.path(palette_dir,
                     paste0(emap_file_prefix, "_emapplot.tiff")),
           width = total_emap_width, height = total_emap_height,
           units = "in", res = 300, compression = "lzw")
      grid.newpage()
      grid.draw(g_emap)
      dev.off()

      cat("    -> Emapplot saved:", emap_file_prefix,
          "(palette", palette_choice, ", panel:",
          round(dynamic_panel_width, 2), "x",
          round(dynamic_panel_height, 2),
          "in, total:", round(total_emap_width, 2), "x",
          round(total_emap_height, 2), "in)\n")
    }
  }

  cat("\n")
}

cat("All enrichment maps have been generated successfully.\n")
cat("Output directory:", emapplot_output_dir, "\n")

# ==============================================================================
# PART 3: Gene-Concept Network (cnetplot)
# ==============================================================================
# For each .xlsx file and each worksheet, build a gene-concept network from the
# same green-highlighted terms used for bubble plots. This is a bipartite network:
#   - Term nodes: colored by -log10(Adjusted P-value), sized proportional to gene count
#   - Gene nodes: smaller, uniform color
#   - Edges: connect each term to its associated genes
# Genes shared by multiple terms create hub connections in the network.
#
# Uses igraph + ggraph (same rationale as emapplot: data is from external Enrichr
# results, not clusterProfiler enrichResult objects).
#
# Output:
#   prot_and_ub_annotated_enrichr_results/cnetplot/
#     {xlsx_basename}_{sheet_name}_cnetplot.pdf
#     {xlsx_basename}_{sheet_name}_cnetplot.tiff

# ==============================================================================
# USER CONFIGURATION: Gene-Concept Network Parameters
# ==============================================================================

# Cnetplot node color palette (same numbering as bubble palettes above)
# Provide a vector of numbers to select multiple cnetplot palettes.
cnetplot_palette_choices <- c(9, 10)

# Cnetplot dynamic sizing parameters (inches)
cnetplot_spacing_per_node <- 0.55  # inches of spacing per sqrt(total nodes)
cnetplot_min_panel_size   <- 4.0   # minimum panel width or height (inches)
cnetplot_term_font_size   <- 2.8   # pt for Term node labels
cnetplot_gene_font_size   <- 2.0   # pt for Gene node labels
cnetplot_gene_node_color  <- "grey70"  # color for gene nodes

cat("\n")
cat("========================================\n")
cat("Part 3: Gene-Concept Network (cnetplot)\n")
cat("========================================\n")
cat("Cnetplot palettes:", paste(cnetplot_palette_choices, collapse = ", "), "\n")

# Setup output directory for cnetplot
cnetplot_output_dir <- file.path(input_dir, "cnetplot")
if (!dir.exists(cnetplot_output_dir)) {
  dir.create(cnetplot_output_dir, recursive = TRUE)
}

# ==============================================================================
# Cnetplot Processing Loop
# ==============================================================================

for (xlsx_path in xlsx_files) {
  xlsx_basename <- tools::file_path_sans_ext(basename(xlsx_path))
  cat("\nProcessing (cnetplot):", xlsx_basename, "\n")

  wb <- loadWorkbook(xlsx_path)
  sheet_names <- getSheetNames(xlsx_path)

  for (sheet_name in sheet_names) {
    cat("  Sheet:", sheet_name, "\n")

    df <- read.xlsx(xlsx_path, sheet = sheet_name)

    if (is.null(df) || nrow(df) == 0) {
      cat("    -> Empty sheet, skipping.\n")
      next
    }

    highlighted_rows <- get_highlighted_term_rows(wb, sheet_name)

    if (length(highlighted_rows) == 0) {
      cat("    -> No highlighted Term rows found, skipping.\n")
      next
    }

    cnet_df <- df[highlighted_rows, , drop = FALSE]

    # Map column names
    col_names <- colnames(cnet_df)
    padj_col  <- grep("^Adjusted[.]P", col_names, value = TRUE)[1]
    odds_col  <- grep("^Odds[.]Ratio", col_names, value = TRUE)[1]
    genes_col <- "Genes"

    if (is.na(padj_col) || is.na(odds_col)) {
      cat("    -> Required columns not found, skipping.\n")
      next
    }

    cnet_df$Count <- sapply(cnet_df[[genes_col]], count_genes)
    cnet_df$neg_log10_padj <- -log10(as.numeric(cnet_df[[padj_col]]))

    cnet_df <- cnet_df[!is.na(cnet_df$neg_log10_padj) &
                        !is.na(cnet_df$Term), ]

    if (nrow(cnet_df) == 0) {
      cat("    -> No valid data after filtering, skipping.\n")
      next
    }

    # Clean Term labels (same rules as bubble plots)
    if (grepl("WikiPathways", sheet_name, ignore.case = TRUE)) {
      cnet_df$Term <- trimws(gsub("\\s*WP\\d+$", "", cnet_df$Term))
    }
    if (grepl("GO_Biological_Process", sheet_name, ignore.case = TRUE)) {
      cnet_df$Term <- trimws(gsub("\\s*\\(GO:\\d+\\)$", "", cnet_df$Term))
    }

    # Build bipartite edge list: Term -> Gene
    cnet_edges <- data.frame(from = character(0), to = character(0),
                              stringsAsFactors = FALSE)
    for (i in seq_len(nrow(cnet_df))) {
      genes <- parse_genes(cnet_df[[genes_col]][i])
      if (length(genes) > 0) {
        cnet_edges <- rbind(cnet_edges, data.frame(
          from = cnet_df$Term[i], to = genes,
          stringsAsFactors = FALSE
        ))
      }
    }

    if (nrow(cnet_edges) == 0) {
      cat("    -> No gene associations found, skipping.\n")
      next
    }

    # Collect all unique genes
    all_genes <- unique(cnet_edges$to)

    # Build node data frame with type info
    term_nodes <- data.frame(
      name = cnet_df$Term,
      node_type = "Term",
      node_size = cnet_df$Count,
      neg_log10_padj = cnet_df$neg_log10_padj,
      stringsAsFactors = FALSE
    )
    gene_nodes <- data.frame(
      name = all_genes,
      node_type = "Gene",
      node_size = 1,
      neg_log10_padj = NA_real_,
      stringsAsFactors = FALSE
    )
    all_nodes <- rbind(term_nodes, gene_nodes)

    # Build igraph network
    g_cnet <- graph_from_data_frame(cnet_edges, directed = FALSE,
                                     vertices = all_nodes)

    # Total node count for dynamic sizing
    total_nodes <- nrow(all_nodes)
    n_terms_cnet <- nrow(term_nodes)

    # Generate output filename
    safe_sheet_name <- gsub("[^A-Za-z0-9_]", "_", sheet_name)
    cnet_file_prefix <- paste0(xlsx_basename, "_", safe_sheet_name)

    for (palette_choice in cnetplot_palette_choices) {
      cnet_color_low  <- bubble_palettes[[palette_choice]][1]
      cnet_color_high <- bubble_palettes[[palette_choice]][2]

      # Create specific subdirectory for this palette
      palette_dir <- file.path(cnetplot_output_dir,
                                palette_names[palette_choice])
      if (!dir.exists(palette_dir)) {
        dir.create(palette_dir, recursive = TRUE)
      }

      # Calculate dynamic panel dimensions based on total node count
      max_term_nchar <- max(nchar(as.character(cnet_df$Term)))
      max_gene_nchar <- max(nchar(as.character(all_genes)))
      max_label_nchar <- max(max_term_nchar, max_gene_nchar)
      label_width_est <- max_label_nchar * 0.06
      base_panel_size <- max(cnetplot_min_panel_size,
                             sqrt(total_nodes) * cnetplot_spacing_per_node)
      dynamic_cnet_width  <- base_panel_size + label_width_est
      dynamic_cnet_height <- base_panel_size

      # Build cnetplot using ggraph
      set.seed(42)
      cnet_plot <- ggraph(g_cnet, layout = "fr") +
        geom_edge_link(alpha = 0.25, color = "grey70") +
        # Gene nodes (no color mapping, fixed color)
        geom_node_point(
          data = function(x) x[x$node_type == "Gene", ],
          aes(size = node_size),
          color = cnetplot_gene_node_color, show.legend = FALSE
        ) +
        # Term nodes (colored by -log10 padj)
        geom_node_point(
          data = function(x) x[x$node_type == "Term", ],
          aes(size = node_size, color = neg_log10_padj)
        ) +
        # Gene labels
        geom_node_text(
          data = function(x) x[x$node_type == "Gene", ],
          aes(label = name), repel = TRUE,
          size = cnetplot_gene_font_size, color = "black",
          max.overlaps = Inf,
          box.padding = unit(0.35, "lines"),
          point.padding = unit(0.3, "lines"),
          force = 2, force_pull = 0.5,
          segment.color = "grey80", segment.size = 0.2
        ) +
        # Term labels (bold)
        geom_node_text(
          data = function(x) x[x$node_type == "Term", ],
          aes(label = name), repel = TRUE,
          size = cnetplot_term_font_size, fontface = "bold",
          max.overlaps = Inf,
          box.padding = unit(0.5, "lines"),
          point.padding = unit(0.3, "lines"),
          force = 3, force_pull = 0.3,
          segment.color = "grey60", segment.size = 0.3
        ) +
        scale_color_gradient(low = cnet_color_high, high = cnet_color_low,
                             name = expression(-log[10](Adjusted~P-value))) +
        scale_size_continuous(name = "Count", range = c(1, 6),
                              guide = guide_legend(
                                override.aes = list(color = "black"))) +
        labs(title = paste0(xlsx_basename, "\n", sheet_name)) +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(size = 9, face = "bold", hjust = 0.5,
                                    color = "black"),
          legend.title = element_text(size = 7, face = "bold", color = "black"),
          legend.text = element_text(size = 7, color = "black"),
          legend.position = "right",
          legend.key.size = unit(0.4, "cm"),
          legend.spacing.y = unit(0.1, "cm"),
          plot.margin = margin(10, 10, 10, 10)
        )

      # Convert to gtable and set dynamic panel dimensions
      g_cnet_grob <- ggplotGrob(cnet_plot)
      panel_pos_cnet <- g_cnet_grob$layout[g_cnet_grob$layout$name == "panel", ]
      g_cnet_grob$widths[panel_pos_cnet$l]  <- unit(dynamic_cnet_width, "in")
      g_cnet_grob$heights[panel_pos_cnet$t] <- unit(dynamic_cnet_height, "in")

      # Measure total output dimensions using a temporary PDF device
      tmp_pdf <- tempfile(fileext = ".pdf")
      pdf(tmp_pdf, width = 20, height = 20)
      grid.newpage()
      grid.draw(g_cnet_grob)

      # Check if the legend is taller than the panel
      legend_rows_cnet <- g_cnet_grob$layout[
        grepl("guide-box", g_cnet_grob$layout$name), ]
      if (nrow(legend_rows_cnet) > 0) {
        legend_height_cnet <- convertHeight(
          sum(g_cnet_grob$heights[seq(min(legend_rows_cnet$t),
                                       max(legend_rows_cnet$b))]),
          "in", valueOnly = TRUE
        )
        if (legend_height_cnet > dynamic_cnet_height) {
          dynamic_cnet_height <- legend_height_cnet
          g_cnet_grob$heights[panel_pos_cnet$t] <- unit(dynamic_cnet_height,
                                                         "in")
        }
      }

      total_cnet_width  <- convertWidth(sum(g_cnet_grob$widths), "in",
                                         valueOnly = TRUE)
      total_cnet_height <- convertHeight(sum(g_cnet_grob$heights), "in",
                                          valueOnly = TRUE)
      dev.off()
      unlink(tmp_pdf)

      # Save as PDF
      pdf(file.path(palette_dir,
                    paste0(cnet_file_prefix, "_cnetplot.pdf")),
          width = total_cnet_width, height = total_cnet_height)
      grid.newpage()
      grid.draw(g_cnet_grob)
      dev.off()

      # Save as TIFF
      tiff(file.path(palette_dir,
                     paste0(cnet_file_prefix, "_cnetplot.tiff")),
           width = total_cnet_width, height = total_cnet_height,
           units = "in", res = 300, compression = "lzw")
      grid.newpage()
      grid.draw(g_cnet_grob)
      dev.off()

      cat("    -> Cnetplot saved:", cnet_file_prefix,
          "(palette", palette_choice, ", panel:",
          round(dynamic_cnet_width, 2), "x",
          round(dynamic_cnet_height, 2),
          "in, total:", round(total_cnet_width, 2), "x",
          round(total_cnet_height, 2), "in)\n")
    }
  }

  cat("\n")
}

cat("All gene-concept networks have been generated successfully.\n")
cat("Output directory:", cnetplot_output_dir, "\n")

# ==============================================================================
# PART 4: UpSet Plot
# ==============================================================================
# For each .xlsx file and each worksheet, build an UpSet plot from the same
# green-highlighted terms used for bubble/emapplot/cnetplot. The UpSet plot
# visualizes intersections of gene sets across enriched terms:
#   - Each "set" is one enriched Term
#   - Bars show the size of each intersection (shared genes between term combos)
#   - Bottom-left matrix dots show which terms participate in each intersection
#
# Uses UpSetR package (designed specifically for set intersection visualization,
# superior to Venn diagrams when there are more than 3-4 sets).
#
# Output:
#   prot_and_ub_annotated_enrichr_results/upsetplot/
#     {xlsx_basename}_{sheet_name}_upsetplot.pdf
#     {xlsx_basename}_{sheet_name}_upsetplot.tiff

# ==============================================================================
# USER CONFIGURATION: UpSet Plot Parameters
# ==============================================================================

# Load ComplexUpset (ggplot2-based, publication-quality UpSet plots)
if (!requireNamespace("ComplexUpset", quietly = TRUE)) {
  install.packages("ComplexUpset")
}
library(ComplexUpset)

# Maximum number of intersections to display (sorted by size, largest first)
upset_n_intersections <- 40

# Minimum intersection size to display
upset_min_size <- 1

# Define color palettes for UpSet plots
upset_palettes <- list(
  list(name = "palette_1", bar_color = "red",     set_bar_color = "#E8833A", dot_color = "#2D2D2D", inactive_dot = "#E0E0E0"),
  list(name = "palette_2", bar_color = "#4E79A7", set_bar_color = "#F28E2B", dot_color = "#4E79A7", inactive_dot = "#E0E0E0"),
  list(name = "palette_3", bar_color = "#59A14F", set_bar_color = "#8CD17D", dot_color = "#59A14F", inactive_dot = "#E0E0E0"),
  list(name = "palette_4", bar_color = "#B07AA1", set_bar_color = "#FF9DA7", dot_color = "#B07AA1", inactive_dot = "#E0E0E0"),
  list(name = "palette_5", bar_color = "#76B7B2", set_bar_color = "#EDC948", dot_color = "#76B7B2", inactive_dot = "#E0E0E0")
)

# Choose which palettes to use (1 to 5). You can select multiple, e.g., c(1, 2, 3, 4, 5)
upsetplot_palette_choices <- c(1, 2, 3, 4, 5)

# Dynamic sizing parameters
upset_width_per_intersection  <- 0.1  # inches per intersection column (fixed, minimal)
upset_height_per_set          <- 0.15  # inches per set row (fixed, minimal)
upset_min_width               <- 2.0   # minimum output width (inches)
upset_min_height              <- 2.0   # minimum output height (inches)

cat("\n")
cat("========================================\n")
cat("Part 4: UpSet Plot\n")
cat("========================================\n")
cat("Max intersections:", upset_n_intersections, "\n")

# Setup output directory for upsetplot
upsetplot_output_dir <- file.path(input_dir, "upsetplot")
if (!dir.exists(upsetplot_output_dir)) {
  dir.create(upsetplot_output_dir, recursive = TRUE)
}

# ==============================================================================
# UpSet Plot Processing Loop
# ==============================================================================

for (xlsx_path in xlsx_files) {
  xlsx_basename <- tools::file_path_sans_ext(basename(xlsx_path))
  cat("\nProcessing (upsetplot):", xlsx_basename, "\n")

  wb <- loadWorkbook(xlsx_path)
  sheet_names <- getSheetNames(xlsx_path)

  for (sheet_name in sheet_names) {
    cat("  Sheet:", sheet_name, "\n")

    df <- read.xlsx(xlsx_path, sheet = sheet_name)

    if (is.null(df) || nrow(df) == 0) {
      cat("    -> Empty sheet, skipping.\n")
      next
    }

    highlighted_rows <- get_highlighted_term_rows(wb, sheet_name)

    if (length(highlighted_rows) == 0) {
      cat("    -> No highlighted Term rows found, skipping.\n")
      next
    }

    upset_df <- df[highlighted_rows, , drop = FALSE]

    # Map column names
    col_names <- colnames(upset_df)
    padj_col  <- grep("^Adjusted[.]P", col_names, value = TRUE)[1]
    genes_col <- "Genes"

    if (is.na(padj_col)) {
      cat("    -> Required columns not found, skipping.\n")
      next
    }

    upset_df <- upset_df[!is.na(upset_df$Term), ]

    if (nrow(upset_df) < 2) {
      cat("    -> Less than 2 valid terms, skipping UpSet plot.\n")
      next
    }

    # Clean Term labels (same rules as bubble plots)
    if (grepl("WikiPathways", sheet_name, ignore.case = TRUE)) {
      upset_df$Term <- trimws(gsub("\\s*WP\\d+$", "", upset_df$Term))
    }
    if (grepl("GO_Biological_Process", sheet_name, ignore.case = TRUE)) {
      upset_df$Term <- trimws(gsub("\\s*\\(GO:\\d+\\)$", "", upset_df$Term))
    }

    # Parse gene lists for each term
    gene_lists_upset <- lapply(upset_df[[genes_col]], parse_genes)
    names(gene_lists_upset) <- upset_df$Term

    # Remove terms with zero genes
    gene_lists_upset <- gene_lists_upset[sapply(gene_lists_upset, length) > 0]

    if (length(gene_lists_upset) < 2) {
      cat("    -> Less than 2 terms with genes, skipping.\n")
      next
    }

    # Build binary membership data frame (rows = genes, cols = terms)
    all_genes_upset <- unique(unlist(gene_lists_upset))
    membership_matrix <- data.frame(
      gene = all_genes_upset,
      stringsAsFactors = FALSE
    )
    for (term_name in names(gene_lists_upset)) {
      membership_matrix[[term_name]] <- as.integer(
        all_genes_upset %in% gene_lists_upset[[term_name]]
      )
    }

    set_names <- setdiff(colnames(membership_matrix), "gene")
    n_sets <- length(set_names)
    n_genes_total <- nrow(membership_matrix)

    # Dynamically measure the longest term label width for Arial size 7
    longest_term_upset <- set_names[which.max(nchar(set_names))]
    max_label_width_in_upset <- grid::convertWidth(
      grid::grobWidth(grid::textGrob(longest_term_upset, gp = grid::gpar(fontsize = 7, fontfamily = "Arial"))),
      "inches", valueOnly = TRUE
    )
    
    # Set fixed panel dimensions for the Set Size bar plot and Intersection Matrix
    upset_set_size_width <- 1.0  # Fixed width for the Set Size bar plot
    n_display <- min(upset_n_intersections, 2^n_sets - 1)
    upset_matrix_width <- n_display * upset_width_per_intersection
    
    # Calculate width ratio as a proportion (ComplexUpset uses width_ratio as the fraction for the Set Size plot)
    calculated_width_ratio <- upset_set_size_width / (upset_matrix_width + upset_set_size_width)

    # Set fixed heights for the matrix and the top bar plot
    upset_matrix_height  <- n_sets * upset_height_per_set
    upset_top_bar_height <- 0.8  # Fixed to 0.8 inches (approx. 1/3 of previous size)
    
    # Calculate height ratio for ComplexUpset (matrix_height / top_bar_height)
    calculated_height_ratio <- upset_matrix_height / upset_top_bar_height

    # Calculate exact dynamic output dimensions WITHOUT max() limits to prevent panel stretching
    # We add 0.5 inches for margins to prevent ggsave from squishing the panels
    dynamic_upset_width  <- upset_matrix_width + upset_set_size_width + max_label_width_in_upset + 0.5
    dynamic_upset_height <- upset_matrix_height + upset_top_bar_height + 0.5

    # Base theme for Arial size 7
    upset_base_theme <- theme(
      text             = element_text(family = "Arial", size = 7, color = "black"),
      axis.title       = element_text(family = "Arial", size = 7, face = "bold", color = "black"),
      axis.text        = element_text(family = "Arial", size = 7, color = "black"),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.ticks       = element_line(color = "grey50", linewidth = 0.3)
    )

    # Loop over selected palettes
    for (palette_choice in upsetplot_palette_choices) {
      if (palette_choice < 1 || palette_choice > length(upset_palettes)) {
        cat("    -> Invalid palette choice:", palette_choice, "skipping.\n")
        next
      }
      
      current_palette <- upset_palettes[[palette_choice]]
      upset_bar_color     <- current_palette$bar_color
      upset_set_bar_color <- current_palette$set_bar_color
      upset_dot_color     <- current_palette$dot_color
      upset_inactive_dot  <- current_palette$inactive_dot
      
      palette_dir <- file.path(upsetplot_output_dir, paste0("palette_", palette_choice))
      if (!dir.exists(palette_dir)) {
        dir.create(palette_dir, recursive = TRUE)
      }

      # Generate output filename
      safe_sheet_name <- gsub("[^A-Za-z0-9_]", "_", sheet_name)
      upset_file_prefix <- paste0(xlsx_basename, "_", safe_sheet_name)

      # Build publication-quality UpSet plot using ComplexUpset
      upset_plot <- ComplexUpset::upset(
        data = membership_matrix,
        intersect = set_names,
        name = NULL,
        min_size = upset_min_size,
        n_intersections = upset_n_intersections,
        sort_sets = "descending",
        sort_intersections = "descending",
        sort_intersections_by = c("degree", "cardinality"),
        width_ratio = calculated_width_ratio,
        height_ratio = calculated_height_ratio,
        wrap = FALSE,
        set_sizes = (
          upset_set_size(
            geom = geom_bar(
              fill = upset_set_bar_color,
              width = 0.65
            ),
            position = "right"
          )
          + upset_base_theme
          + theme(plot.margin = margin(0, 2, 0, 2))
          + xlab("Set Size")
        ),
        base_annotations = list(
          "Intersection Size" = intersection_size(
            text = list(size = 7 / ggplot2::.pt, vjust = -0.3, family = "Arial"),
            bar_number_threshold = 1,
            mapping = aes(fill = "bars")
          )
          + scale_fill_manual(values = c("bars" = upset_bar_color),
                              guide = "none")
          + scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
          + upset_base_theme
          + theme(plot.margin = margin(2, 2, 0, 2))
          + ylab("Intersection\nSize")
        ),
        matrix = intersection_matrix(
          geom = geom_point(size = 1.8, shape = 16),
          segment = geom_segment(linewidth = 0.6),
          outline_color = list(active = upset_dot_color,
                                inactive = upset_inactive_dot)
        ),
        stripes = upset_stripes(
          colors = c("white", "grey97")
        ),
        themes = upset_modify_themes(list(
          "intersections_matrix" = upset_base_theme + theme(
            axis.text.y = element_text(size = 7, face = "bold", family = "Arial", color = "black", hjust = 1),
            plot.margin = margin(0, 0, 0, 0)
          )
        ))
      )

      # Save as PDF
      ggsave(
        filename = file.path(palette_dir,
                             paste0(upset_file_prefix, "_upsetplot.pdf")),
        plot = upset_plot,
        width = dynamic_upset_width,
        height = dynamic_upset_height,
        units = "in",
        device = "pdf"
      )

      # Save as TIFF
      ggsave(
        filename = file.path(palette_dir,
                             paste0(upset_file_prefix, "_upsetplot.tiff")),
        plot = upset_plot,
        width = dynamic_upset_width,
        height = dynamic_upset_height,
        units = "in",
        dpi = 300,
        device = "tiff",
        compression = "lzw"
      )

      cat("    -> UpSet plot saved (palette", palette_choice, "):", upset_file_prefix,
          "(", n_sets, "sets,", n_genes_total, "genes,",
          round(dynamic_upset_width, 2), "x",
          round(dynamic_upset_height, 2), "in)\n")
    }
  }

  cat("\n")
}

cat("All UpSet plots have been generated successfully.\n")
cat("Output directory:", upsetplot_output_dir, "\n")

