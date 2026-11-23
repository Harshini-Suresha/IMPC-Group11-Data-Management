# =============================================================================
# Shared figure theme (journal style) for the dashboard and report figures.
# Colour-vision-deficiency-safe categorical palette (validated: adjacent CVD
# dE >= 9.1); single-hue blue ramp for magnitudes.
# =============================================================================
suppressPackageStartupMessages(library(ggplot2))

PAL <- list(
  surface = "#ffffff", ink = "#000000", ink2 = "#333333", muted = "#808080",
  grid = "#e5e5e5", base = "#bdbdbd",
  cat = c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a", yellow = "#eda100",
          magenta = "#e87ba4", green = "#008300", violet = "#6250d6", red = "#e34948"),
  seq = c("#cde2fb", "#b7d3f6", "#9ec5f4", "#86b6ef", "#6da7ec", "#5598e7", "#3987e5",
          "#2a78d6", "#256abf", "#1c5cab", "#184f95", "#104281", "#0d366b")
)
SIG_COL <- PAL$cat[["blue"]]     # significant
NS_COL  <- "#c8c8c8"             # not significant
HL_COL  <- PAL$cat[["orange"]]   # highlighted genes
CAT     <- unname(PAL$cat)

# Classic publication theme: white background, black axis lines with ticks,
# no background grid, compact legends.
theme_pub <- function(base_size = 8, grid = "none") {
  t <- theme_classic(base_size = base_size, base_family = "Helvetica") +
    theme(
      axis.line        = element_line(colour = "black", linewidth = 0.3),
      axis.ticks       = element_line(colour = "black", linewidth = 0.3),
      axis.ticks.length = unit(1.5, "pt"),
      axis.text        = element_text(colour = "black", size = rel(0.9)),
      axis.title       = element_text(colour = "black", size = rel(1)),
      legend.title     = element_text(size = rel(0.9)),
      legend.text      = element_text(size = rel(0.85)),
      legend.key.size  = unit(0.8, "lines"),
      legend.background = element_blank(),
      strip.background = element_blank(),
      strip.text       = element_text(colour = "black", face = "bold", hjust = 0, size = rel(1)),
      plot.title       = element_text(colour = "black", face = "bold", size = rel(1.1), hjust = 0),
      plot.subtitle    = element_text(colour = "black", size = rel(0.9)),
      plot.tag         = element_text(face = "bold", size = rel(1.4)),
      plot.margin      = margin(4, 6, 4, 4)
    )
  if (grid %in% c("y", "both")) t <- t + theme(panel.grid.major.y = element_line(colour = PAL$grid, linewidth = 0.25))
  if (grid %in% c("x", "both")) t <- t + theme(panel.grid.major.x = element_line(colour = PAL$grid, linewidth = 0.25))
  t
}
# the dashboard uses a slightly larger base size
theme_impc <- function(base_size = 11, grid = "none") theme_pub(base_size, grid)
