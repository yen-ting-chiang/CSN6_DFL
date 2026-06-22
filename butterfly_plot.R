setwd("C:/Users/danny/Documents/R_project/CSN6_DFL/butterfly_plot")
getwd()

# Set this variable to TRUE if you want to include phosphorylation data in the plots
show_phosphorylation <- FALSE

# ============================================================
# Basic Bar Plot
# ============================================================

# 1. Create example data
df <- data.frame(
  category = c("protein", "phosphorylation", "ubiquitination"),
  Up_regulated = c(247, 416, 390),
  Down_regulated = c(283, 426, 479)
)

if (!show_phosphorylation) {
  df <- df[df$category != "phosphorylation", ]
}

# 2. Convert to long format
library(tidyr)
df_long <- df %>%
  gather(key = "Regulation", value = "Count", -category)

# 3. Adjust factor levels (to show from top to bottom on y-axis: protein -> phosphorylation -> ubiquitination)
#   Note: When not using coord_flip() and category is on the y-axis,
#   the first level in factors appears at the bottom, and the last at the top.
#   Therefore, put the one you want at the top at the end of the levels vector.
cat_levels_basic <- c("ubiquitination", "phosphorylation", "protein")
if (!show_phosphorylation) {
  cat_levels_basic <- c("ubiquitination", "protein")
}

df_long$category <- factor(
  df_long$category,
  levels = cat_levels_basic
)
# The levels order above will put "protein" at the top and "ubiquitination" at the bottom

# Adjust Regulation order, legend will list Up_regulated first, then Down_regulated
df_long$Regulation <- factor(
  df_long$Regulation,
  levels = c("Down_regulated", "Up_regulated")
)

# 4. Draw Plot (without coord_flip, using x=Count, y=category)
library(ggplot2)

p <- ggplot(df_long, aes(x = Count, y = category, fill = Regulation)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_discrete(
    breaks = c("Up_regulated", "Down_regulated"), 
    labels = c("Up-regulated", "Down-regulated"))+
  # Show values
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.8),
    # For horizontal bars use hjust to adjust text position: hjust < 1 on the right, > 1 on the left
    hjust = -0.2,
    size = 3
  ) +
  labs(
    x = "Number of Differentially Expressed Proteins",
    y = "Category",
    fill = "Regulation",
    title = "CSN6 Knockdown (KD) vs Wild-Type (WT) in MCF7 Cell Lines"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

p_custom <- p + theme(
  axis.text.y = element_text(color = "black", size = 10),
  legend.text = element_text(size = 10)
)

# 5. Show final basic plot
print(p_custom)

# 6. Export image using ggsave() if needed
ggsave(
  filename = "example_plot_final.png",
  plot = p_custom,
  width = 8,
  height = 4,
  dpi = 300
)



# ============================================================
# Butterfly Plot (Tornado Plot)
# Up-regulated right, Down-regulated left
# Phosphorylation & ubiquitination have light (all) and dark (significant) layers
# ============================================================

# 1. Create data for light bars (all DEPs): only phosphorylation & ubiquitination
df_all <- data.frame(
  category = c("phosphorylation", "phosphorylation",
               "ubiquitination", "ubiquitination"),
  Regulation = c("Up_regulated", "Down_regulated",
                  "Up_regulated", "Down_regulated"),
  Count = c(590, -580, 572, -681)
)

# 2. Create data for dark bars (significant DEPs): all three categories
df_sig <- data.frame(
  category = c("protein", "protein",
               "phosphorylation", "phosphorylation",
               "ubiquitination", "ubiquitination"),
  Regulation = c("Up_regulated", "Down_regulated",
                  "Up_regulated", "Down_regulated",
                  "Up_regulated", "Down_regulated"),
  Count = c(247, -283, 416, -426, 390, -479)
)

# Filter out phosphorylation if show_phosphorylation is FALSE
if (!show_phosphorylation) {
  df_all <- df_all[df_all$category != "phosphorylation", ]
  df_sig <- df_sig[df_sig$category != "phosphorylation", ]
}

# 3. Set category factor levels (top: protein, bottom: ubiquitination)
cat_levels_butterfly <- c("ubiquitination", "phosphorylation", "protein")
if (!show_phosphorylation) {
  cat_levels_butterfly <- c("ubiquitination", "protein")
}
df_all$category  <- factor(df_all$category,  levels = cat_levels_butterfly)
df_sig$category  <- factor(df_sig$category,  levels = cat_levels_butterfly)

# 4. Create fill_group (combining Regulation and significance)
df_all$fill_group <- paste0(df_all$Regulation, ".all")
df_sig$fill_group <- paste0(df_sig$Regulation, ".sig")

# Set fill_group factor levels (controls legend display order)
fill_levels <- c("Up_regulated.all", "Up_regulated.sig",
                 "Down_regulated.all", "Down_regulated.sig")
df_all$fill_group <- factor(df_all$fill_group, levels = fill_levels)
df_sig$fill_group <- factor(df_sig$fill_group, levels = fill_levels)

# 5. Calculate x-axis range to make it symmetrical
x_max <- max(abs(df_all$Count), abs(df_sig$Count))
x_limit <- ceiling(x_max / 50) * 50

# 6. Draw Butterfly Plot
p_butterfly <- ggplot() +
  # Background layer: light bars (all DEPs)
  geom_bar(data = df_all,
           aes(x = Count, y = category, fill = fill_group),
           stat = "identity", position = "identity", width = 0.6) +
  # Foreground layer: dark bars (significant DEPs)
  geom_bar(data = df_sig,
           aes(x = Count, y = category, fill = fill_group),
           stat = "identity", position = "identity", width = 0.6) +
  # Central vertical line
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
  # Light bars numeric labels
  geom_text(data = df_all,
            aes(x = Count, y = category, label = abs(Count),
                hjust = ifelse(Count >= 0, -0.2, 1.2)),
            size = 3.2, color = "grey40") +
  # Dark bars numeric labels
  geom_text(data = df_sig,
            aes(x = Count, y = category, label = abs(Count),
                hjust = ifelse(Count >= 0, -0.2, 1.2)),
            size = 3.5, fontface = "bold") +
  # Custom colors: light / dark * red (Up) / blue (Down)
  scale_fill_manual(
    values = c(
      "Up_regulated.all"  = "#F1948A",   # Light red
      "Up_regulated.sig"  = "#E74C3C",   # Dark red
      "Down_regulated.all" = "#85C1E9",  # Light blue
      "Down_regulated.sig" = "#3498DB"   # Dark blue
    ),
    breaks = c("Up_regulated.all", "Up_regulated.sig",
               "Down_regulated.all", "Down_regulated.sig"),
    labels = c("Up-regulated (all)", "Up-regulated (significant)",
               "Down-regulated (all)", "Down-regulated (significant)")
  ) +
  # X-axis labels use absolute values, symmetrical
  scale_x_continuous(
    limits = c(-x_limit, x_limit),
    breaks = seq(-x_limit, x_limit, by = 100),
    labels = function(x) abs(x)
  ) +
  labs(
    x = "Number of Differentially Expressed Proteins",
    y = NULL,
    fill = "Regulation",
    title = "CSN6 Knockdown (KD) vs Wild-Type (WT) in MCF7 Cell Lines"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.text.y = element_text(color = "black", size = 11, face = "bold"),
    axis.text.x = element_text(color = "black", size = 9),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

# 7. Display plot
print(p_butterfly)

# 8. Export image (PNG)
ggsave(
  filename = "CSN6_MCF7_butterfly_plot.png",
  plot = p_butterfly,
  width = 7,
  height = 4.5,
  dpi = 300
)

# 9. Export publication quality TIFF (300 dpi, LZW compression)
ggsave(
  filename = "CSN6_MCF7_butterfly_plot.tiff",
  plot = p_butterfly,
  width = 7,
  height = 5,
  dpi = 300,
  compression = "lzw"
)

# 10. Export publication quality PDF (Vector image)
ggsave(
  filename = "CSN6_MCF7_butterfly_plot.pdf",
  plot = p_butterfly,
  width = 7,
  height = 5,
  device = cairo_pdf
)
