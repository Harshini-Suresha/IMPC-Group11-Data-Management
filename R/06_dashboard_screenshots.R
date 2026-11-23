#!/usr/bin/env Rscript
# =============================================================================
# Group 11 - screenshots of the running RShiny dashboard for the report.
# Start the app first (shiny::runApp("shiny", port = 4011)), then run:
#   Rscript R/06_dashboard_screenshots.R [http://localhost:4011]
# Uses the dashboard's shareable links (?tab=...&gene=...).
# =============================================================================
suppressPackageStartupMessages(library(webshot2))
args <- commandArgs(trailingOnly = TRUE)
BASE <- if (length(args)) args[1] else "http://localhost:4011"
OUT  <- "results/figures/dashboard"; dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

shots <- list(
  list("dash_overview",  "tab=overview",                          1300),
  list("dash_gene_Tspyl2",  "tab=gene&gene=Tspyl2",               1220),
  list("dash_gene_Ifng",    "tab=gene&gene=Ifng",                 1220),
  list("dash_gene_Lpcat2b", "tab=gene&gene=Lpcat2b",              1220),
  list("dash_gene_Myof",    "tab=gene&gene=Myof",                 1220),
  list("dash_gene_Cog2",    "tab=gene&gene=Cog2",                 1000),
  list("dash_phenotype", "tab=phenotype&param=IMPC_DXA_007_001",  1250),
  list("dash_clusters",  "tab=clusters",                          1150),
  list("dash_groups",    "tab=groups",                            1150),
  list("dash_disease",   "tab=disease",                           1150),
  list("dash_interest",  "tab=interest",                          900),
  list("dash_quality",   "tab=quality",                           1000))

for (s in shots) {
  f <- file.path(OUT, paste0(s[[1]], ".png"))
  webshot(paste0(BASE, "/?", s[[2]]), file = f, vwidth = 1440, vheight = s[[3]], delay = 9, zoom = 2)
  cat("wrote", f, "\n")
}

# plot-only captures of the Gene -> phenotypes chart for each gene of interest
for (g in c("Tspyl2", "Ifng", "Lpcat2b", "Myof", "Cog2")) {
  f <- file.path(OUT, sprintf("plot_gene_%s.png", g))
  webshot(sprintf("%s/?tab=gene&gene=%s", BASE, g), file = f, selector = "#p_gene",
          vwidth = 1440, vheight = 1400, delay = 9, zoom = 2, expand = 6)
  cat("wrote", f, "\n")
}
