# =============================================================================
# Plotting functions shared by the dashboard and the report figures.
# Styling comes from theme.R (PAL, theme_impc, SIG_COL, NS_COL, HL_COL) - source
# theme.R before this file (Shiny sources R/ alphabetically: plots, then theme,
# which is fine because these functions only use the theme when called).
# =============================================================================
suppressPackageStartupMessages({ library(ggplot2); library(data.table) })

# p-values reported as exactly 0 (below numeric precision) and extreme scores
# are capped for display so they do not compress the rest of the plot.
SCORE_CAP <- 20
neglog <- function(p, cap = SCORE_CAP) pmin(-log10(pmax(p, 1e-300)), cap)
fmt_p  <- function(p) ifelse(p == 0, "0 (reported; below numeric precision)", sprintf("%.3g", p))
has_repel <- function() requireNamespace("ggrepel", quietly = TRUE)

# Visualisation 1 - Manhattan-style plot of every phenotype tested for one gene.
# Procedures sit on alternating bands; significant phenotypes are blue.
plot_gene_phenotypes <- function(x, threshold = 0.05, title = NULL, label_top = 10, cap = SCORE_CAP, repel = TRUE) {
  x <- copy(x)[order(procedure_name, parameter_name)]
  x[, idx := .I]
  x[, score := neglog(p_value, cap)]
  x[, significant := p_value < threshold]
  x[, capped := score >= cap]
  x[, band := as.integer(factor(procedure_name, levels = unique(procedure_name))) %% 2 == 0]
  x[, tone := fifelse(significant, "sig", fifelse(band, "a", "b"))]
  x[, tip := sprintf("%s (%s)<br>Procedure: %s<br>p = %s<br>Life stage: %s",
                     parameter_name, parameter_stable_id, procedure_name, fmt_p(p_value), life_stage_name)]
  centres <- x[, .(mid = mean(idx), lo = min(idx) - 0.5, hi = max(idx) + 0.5, band = band[1]), by = procedure_name]
  lab <- x[significant == TRUE][order(p_value)][, .SD[1], by = parameter_name][seq_len(min(.N, label_top))]
  p <- ggplot(x, aes(idx, score, text = tip)) +
    geom_rect(data = centres[band == TRUE], inherit.aes = FALSE,
              aes(xmin = lo, xmax = hi, ymin = -Inf, ymax = Inf), fill = "#f3f3f3") +
    geom_hline(yintercept = neglog(threshold), linetype = "22", colour = PAL$ink2, linewidth = 0.35) +
    geom_point(aes(colour = tone, size = significant, shape = capped), stroke = 0) +
    scale_colour_manual(values = c(sig = SIG_COL, a = "#8c8c8c", b = "#c8c8c8"), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 1.6, `TRUE` = 2.8), guide = "none") +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), guide = "none") +
    scale_x_continuous(breaks = centres$mid, labels = centres$procedure_name, expand = expansion(add = 1)) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.15))) +
    labs(x = NULL, y = "-log10 p", title = title,
         subtitle = sprintf("%d phenotypes tested, %d with p < %g%s", nrow(x), sum(x$significant),
                            threshold, if (any(x$capped)) sprintf("; triangles capped at %g", cap) else "")) +
    theme_impc() +
    theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 7.5))
  if (nrow(lab) && repel && has_repel())
    p <- p + ggrepel::geom_text_repel(data = lab, aes(label = parameter_name), size = 2.9, colour = PAL$ink,
                                      min.segment.length = 0, segment.colour = PAL$muted, segment.size = 0.25,
                                      box.padding = 0.4, max.overlaps = Inf, seed = 11)
  attr(p, "point_labels") <- lab[, .(x = idx, y = score, label = parameter_name)]   # for plotly annotations
  p
}

# Visualisation 2 - every knockout gene for one phenotype, ranked
plot_parameter_genes <- function(x, threshold = 0.05, title = NULL, highlight = character(0), label_top = 12,
                                 cap = SCORE_CAP, repel = TRUE) {
  x <- copy(x)[order(p_value)]
  x[, rank := .I]
  x[, score := neglog(p_value, cap)]
  x[, status := factor(fifelse(gene_symbol %in% highlight, "Highlighted gene",
                               fifelse(p_value < threshold, "Significant", "Not significant")),
                       levels = c("Significant", "Highlighted gene", "Not significant"))]
  x[, tip := sprintf("%s<br>Allele: %s<br>p = %s<br>Life stage: %s<br>Strain: %s",
                     gene_symbol, allele_symbol, fmt_p(p_value), life_stage_name, strain_name)]
  lab <- unique(rbind(x[status == "Significant"][seq_len(min(.N, label_top))], x[status == "Highlighted gene"]))
  p <- ggplot(x, aes(rank, score, text = tip)) +
    geom_segment(aes(xend = rank, y = 0, yend = score, colour = status), linewidth = 0.45) +
    geom_point(aes(colour = status), size = 2.1, stroke = 0) +
    geom_hline(yintercept = neglog(threshold), linetype = "22", colour = PAL$ink2, linewidth = 0.35) +
    scale_colour_manual(values = c("Significant" = SIG_COL, "Highlighted gene" = HL_COL, "Not significant" = NS_COL), drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Knockout genes, ranked by p-value", y = "-log10 p", colour = NULL, title = title,
         subtitle = sprintf("%d knockout genes tested; %d significant at p < %g",
                            uniqueN(x$gene_symbol), uniqueN(x[p_value < threshold, gene_symbol]), threshold)) +
    theme_impc() + theme(legend.position = "top", legend.justification = "left")
  if (nrow(lab) && repel && has_repel())
    p <- p + ggrepel::geom_text_repel(data = lab, aes(label = gene_symbol), size = 3, colour = PAL$ink,
                                      segment.colour = PAL$muted, segment.size = 0.25, max.overlaps = Inf,
                                      box.padding = 0.35, seed = 11)
  attr(p, "point_labels") <- lab[, .(x = rank, y = score, label = gene_symbol)]
  p
}

# Visualisation 3 - clustering helpers
build_matrix <- function(long, min_features = 3, top_features = 40) {
  long <- as.data.table(long)
  feat_cov <- long[, .N, by = feature][order(-N)]
  keep_feat <- head(feat_cov$feature, top_features)
  long <- long[feature %in% keep_feat]
  gcov <- long[, .N, by = .(mgi_gene_id, gene_symbol)][N >= min_features]
  long <- long[mgi_gene_id %in% gcov$mgi_gene_id]
  w <- dcast(long, mgi_gene_id + gene_symbol ~ feature, value.var = "score", fill = 0)
  m <- as.matrix(w[, -(1:2)]); rownames(m) <- make.unique(w$gene_symbol)
  m[m > 10] <- 10                                    # cap extreme scores for clustering
  m
}

cluster_genes <- function(m, k = 4, method = c("hierarchical", "kmeans"), distance = c("euclidean", "correlation")) {
  method <- match.arg(method); distance <- match.arg(distance)
  set.seed(11)
  if (method == "kmeans") {
    km <- kmeans(scale(m, scale = FALSE), centers = k, nstart = 25)
    cl <- km$cluster; ord <- order(cl, -rowSums(m)); hc <- NULL
  } else {
    d <- if (distance == "correlation") {
      cm <- suppressWarnings(cor(t(m))); cm[is.na(cm)] <- 0; as.dist(1 - cm)
    } else dist(m)
    hc <- hclust(d, method = "ward.D2"); cl <- cutree(hc, k); ord <- hc$order
  }
  # relabel clusters by size so colours are stable (C1 = largest cluster)
  rk <- rank(-tabulate(cl), ties.method = "first"); cl <- setNames(rk[cl], rownames(m))
  if (is.null(hc)) ord <- order(cl, -rowSums(m))
  pc <- prcomp(m, center = TRUE, scale. = FALSE)
  list(cluster = cl, order = ord, hc = hc, pca = pc)
}

plot_cluster_pca <- function(m, res, highlight = character(0), title = NULL, repel = TRUE) {
  pv <- round(100 * res$pca$sdev^2 / sum(res$pca$sdev^2), 1)
  df <- data.table(gene = rownames(m), PC1 = res$pca$x[, 1], PC2 = res$pca$x[, 2],
                   cluster = factor(paste0("C", res$cluster), levels = paste0("C", sort(unique(res$cluster)))),
                   n_sig = rowSums(m > -log10(0.05)))
  df[, tip := sprintf("%s<br>Cluster %s<br>%d features with p < 0.05", gene, cluster, n_sig)]
  cn <- df[, .N, by = cluster]
  levels(df$cluster) <- sprintf("%s; n=%d", levels(df$cluster), cn$N[match(levels(df$cluster), cn$cluster)])
  hl <- df[gene %in% highlight]
  p <- ggplot(df, aes(PC1, PC2, text = tip)) +
    geom_point(aes(colour = cluster), alpha = 0.7, size = 2, stroke = 0) +
    geom_point(data = hl, shape = 21, size = 4.2, stroke = 0.9, colour = PAL$ink, fill = NA) +
    scale_colour_manual(values = CAT[seq_len(nlevels(df$cluster))]) +
    labs(x = sprintf("PC1 (%.1f%% of variance)", pv[1]), y = sprintf("PC2 (%.1f%%)", pv[2]), colour = "Cluster",
         title = title, subtitle = sprintf("%d genes clustered on %d phenotype features", nrow(m), ncol(m))) +
    theme_impc()
  if (nrow(hl) && repel && has_repel())
    p <- p + ggrepel::geom_text_repel(data = hl, aes(label = gene), size = 3.2, colour = PAL$ink, fontface = "bold",
                                      box.padding = 0.6, segment.colour = PAL$ink2, segment.size = 0.3, seed = 11)
  attr(p, "point_labels") <- hl[, .(x = PC1, y = PC2, label = gene)]
  p
}

plot_cluster_heatmap <- function(m, res, title = NULL) {
  ord <- res$order
  mm <- m[ord, , drop = FALSE]
  fo <- if (ncol(mm) > 2) hclust(dist(t(mm)))$order else seq_len(ncol(mm))
  mm <- mm[, fo, drop = FALSE]
  df <- data.table(gene = factor(rep(rownames(mm), ncol(mm)), levels = rev(rownames(mm))),
                   feature = factor(rep(colnames(mm), each = nrow(mm)), levels = colnames(mm)),
                   score = as.vector(mm),
                   cluster = factor(rep(res$cluster[ord], ncol(mm))))
  df[, tip := sprintf("%s<br>%s<br>max -log10 p = %.2f<br>Cluster C%s", gene, feature, score, cluster)]
  ggplot(df, aes(feature, gene, fill = score, text = tip)) +
    geom_tile() +
    scale_fill_gradientn(colours = c(PAL$surface, PAL$seq[c(1, 4, 7, 10, 13)]),
                         values = c(0, 0.08, 0.2, 0.4, 0.7, 1), limits = c(0, 10), name = "max -log10 p") +
    labs(x = NULL, y = NULL, title = title) +
    theme_impc() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_blank())
}
