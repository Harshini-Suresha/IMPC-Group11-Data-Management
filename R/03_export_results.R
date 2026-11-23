#!/usr/bin/env Rscript
# =============================================================================
# Group 11 - export dashboard snapshot, collaborator query results and the
# report figures (same plotting code as the RShiny dashboard).
# Usage: Rscript R/03_export_results.R [db_config.cnf]
# =============================================================================
suppressPackageStartupMessages({ library(DBI); library(RMariaDB); library(data.table); library(ggplot2) })
source("shiny/R/data_access.R"); source("shiny/R/theme.R"); source("shiny/R/plots.R")

args <- commandArgs(trailingOnly = TRUE)
CNF  <- if (length(args)) args[1] else "db_config.cnf"
con  <- dbConnect(RMariaDB::MariaDB(), default.file = normalizePath(CNF), group = "impc", bigint = "integer")

ds <- list(type = "mysql", con = con, label = "MySQL")
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("results/queries", recursive = TRUE, showWarnings = FALSE)

# 1. offline snapshot used by the dashboard when MySQL is unavailable
export_snapshot(con, "shiny/data/dashboard_snapshot.rds")
cat("Snapshot written\n")

# 2. run every collaborator query and save its result
sql <- readLines("sql/03_collaborator_queries.sql", warn = FALSE)
sql <- sql[!grepl("^\\s*--", sql)]
stmts <- trimws(strsplit(paste(sql, collapse = "\n"), ";\\s*\n")[[1]])
stmts <- stmts[nzchar(stmts)]
qi <- 0
for (s in stmts) {
  if (grepl("^(SELECT)", s, ignore.case = TRUE)) {
    qi <- qi + 1
    res <- as.data.table(dbGetQuery(con, s))
    fwrite(res, sprintf("results/queries/Q%d.csv", qi - 1))
    cat(sprintf("Q%d: %d rows\n", qi - 1, nrow(res)))
  } else dbExecute(con, s)
}

# 3. figures for the report
QUERY <- c("Cog2", "Myof", "Lpcat2b", "Ifng", "Tspyl2")
genes <- list_genes(ds)
for (g in QUERY) {
  id <- genes[gene_symbol == g, mgi_gene_id]
  if (!length(id)) next
  x <- gene_results(ds, id)
  p <- plot_gene_phenotypes(x, 0.05, sprintf("%s knockout (%s)", g, id), label_top = 8)
  ggsave(sprintf("results/figures/fig_vis1_%s.png", g), p, width = 10, height = 5.5, dpi = 300, bg = "white")
}
# visualisation 2 example: body weight (most commonly tested weight parameter)
pid <- dbGetQuery(con, "SELECT a.parameter_stable_id, COUNT(*) n FROM analysis a
                        JOIN parameter p ON p.parameter_stable_id = a.parameter_stable_id
                        WHERE p.parameter_name = 'Body weight' GROUP BY a.parameter_stable_id ORDER BY n DESC LIMIT 1")$parameter_stable_id
x <- parameter_results(ds, pid)
p <- plot_parameter_genes(x, 0.05, sprintf("Body weight (%s): all knockout genes", pid), highlight = QUERY)
ggsave("results/figures/fig_vis2_body_weight.png", p, width = 10, height = 5.5, dpi = 300, bg = "white")
# visualisation 3: clusters on parameter groups
m <- build_matrix(score_long(ds, "group"), min_features = 4, top_features = 40)
res <- cluster_genes(m, k = 5, method = "hierarchical")
ggsave("results/figures/fig_vis3_cluster_pca.png", plot_cluster_pca(m, res, QUERY, "Gene clusters (Ward, k = 5) on parameter-group scores"),
       width = 8, height = 6, dpi = 300, bg = "white")
ggsave("results/figures/fig_vis3_cluster_heatmap.png", plot_cluster_heatmap(m, res, "Phenotype-score heatmap ordered by cluster"),
       width = 8, height = 7, dpi = 300, bg = "white")
fwrite(data.table(gene = rownames(m), cluster = res$cluster), "results/queries/gene_clusters_parameter_groups.csv")
# combined significant-phenotype figure for the genes of interest
qx <- rbindlist(lapply(genes[gene_symbol %in% QUERY, mgi_gene_id], function(i) gene_results(ds, i)), fill = TRUE)[p_value < 0.05]
qx[, score := neglog(p_value)]
p <- ggplot(qx, aes(score, reorder(procedure_name, score, max), colour = gene_symbol)) +
  geom_jitter(height = 0.2, width = 0, size = 2.2, alpha = 0.85) +
  geom_vline(xintercept = neglog(0.05), linetype = "dashed", colour = SIG_COL) +
  labs(x = "-log10(p)  (capped at 20)", y = NULL, colour = "Gene",
       title = "Significant phenotypes (p < 0.05) of the collaborator's genes") +
  theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))
ggsave("results/figures/fig_query_genes_significant.png", p, width = 10, height = 6, dpi = 300, bg = "white")
cat("Figures written to results/figures\n")
dbDisconnect(con)
