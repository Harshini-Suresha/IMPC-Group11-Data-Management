#!/usr/bin/env Rscript
# =============================================================================
# Group 11 - multi-panel report figures (journal style).
# Reads the cleaned tables (identical to the database contents, verified by
# R/02_build_database.R). Writes vector PDF and 600 dpi PNG to
# results/figures/report/. Figure legends: results/figures/report/FIGURE_LEGENDS.md
# Usage: Rscript R/05_report_figures.R
# =============================================================================
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(patchwork); library(ggrepel); library(scales)
})
source("shiny/R/theme.R"); source("shiny/R/plots.R")
OUT <- "results/figures/report"; dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(OUT, pattern = "^fig[0-9]+_.*\\.png$", full.names = TRUE))   # superseded single panels
MM <- 1 / 25.4; FULL <- 183 * MM                                              # double-column width (in)
TXT <- 2.9                                                                    # geom_text size (~7 pt)
th <- theme_pub(10) + theme(plot.title = element_text(face = "bold", size = 10.5, margin = margin(l = 14, b = 5)),
                        plot.title.position = "plot")
save_fig <- function(p, name, h_mm, w = FULL) {
  p <- p + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 11),
                                                       plot.tag.position = c(0, 1))
  ggsave(file.path(OUT, paste0(name, ".pdf")), p, width = w, height = h_mm * MM, device = pdf, useDingbats = FALSE)
  ggsave(file.path(OUT, paste0(name, ".png")), p, width = w, height = h_mm * MM, dpi = 600, bg = "white")
  cat("wrote", name, "\n")
}

t   <- readRDS("clean_data/clean_tables.rds")
raw <- fread("clean_data/raw_records_parsed.tsv", colClasses = "character", na.strings = "")
QUERY <- c("Cog2", "Myof", "Lpcat2b", "Ifng", "Tspyl2")

a <- t$analysis[is.na(content_duplicate_of)]
a <- merge(a, t$gene[, .(mgi_gene_id, gene_symbol)], by = "mgi_gene_id")
a <- merge(a, t$parameter[, .(parameter_stable_id, parameter_name, procedure_id)], by = "parameter_stable_id", all.x = TRUE)
a <- merge(a, t$procedure[, .(procedure_id, procedure_name)], by = "procedure_id", all.x = TRUE)
a[is.na(procedure_name), procedure_name := "No IMPC procedure"]
a[is.na(parameter_name), parameter_name := parameter_stable_id]
ap <- a[!is.na(p_value) & p_value <= 1]
gm <- merge(t$parameter_group_member, t$parameter_group[, .(group_id, group_name)], by = "group_id")
gm <- gm[parameter_stable_id %in% a$parameter_stable_id]

# =============================================================================
# Figure 1. Data cleaning and database
# =============================================================================
lg <- t$data_cleaning_log
f1 <- lg[, .N, by = field_name]
f1[, field := gsub("_", " ", sub("^lacz_", "lacZ ", field_name))]
f1[, field := reorder(field, N)]
p1a <- ggplot(f1, aes(N, field)) +
  geom_col(fill = SIG_COL, width = 0.7) +
  geom_text(aes(label = comma(N)), hjust = -0.15, size = TXT) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Corrections per field", x = "Number of values corrected", y = NULL) + th

STR <- t$strain$strain_name; STG <- t$life_stage$life_stage_name; LZ <- t$lacz_call$call_label
refp <- t$parameter[in_impc_reference == 1, parameter_stable_id]
valid_raw <- function(f, v) switch(f,
  strain_name = v %in% STR, mouse_life_stage = v %in% STG,
  parameter_stable_id = v %in% refp | grepl("^[A-Z]+(-[A-Z])*_[A-Z0-9]+_[0-9]{3}_[0-9]{3}$", v),
  gene_symbol = grepl("^[A-Za-z0-9.-]+$", v) & v != "NA",
  allele_symbol = grepl("^[A-Za-z0-9.-]+<[A-Za-z0-9().-]+>$", v),
  p_value = grepl("^[0-9.eE-]+$", v) & suppressWarnings(as.numeric(v)) <= 1,
  v %in% LZ)
fields <- c("gene_symbol", "allele_symbol", "strain_name", "parameter_stable_id", "p_value", "mouse_life_stage",
            grep("^lacz_", names(raw), value = TRUE))
cw <- merge(t$analysis[, .(analysis_id, allele_id, strain_id, life_stage_id, parameter_stable_id, p_value)],
            dcast(t$analysis_lacz, analysis_id ~ tissue_id, value.var = "call_id"), by = "analysis_id", all.x = TRUE)
rawk <- raw[!grepl("_[0-9]+\\.csv$", source_file)]
q2 <- rbindlist(lapply(fields, function(f) {
  v <- rawk[[f]]
  after <- switch(f, gene_symbol = 0, allele_symbol = mean(is.na(cw$allele_id)), strain_name = mean(is.na(cw$strain_id)),
                  parameter_stable_id = mean(is.na(cw$parameter_stable_id)),
                  p_value = mean(is.na(cw$p_value) | cw$p_value > 1), mouse_life_stage = mean(is.na(cw$life_stage_id)),
                  mean(is.na(cw[[as.character(t$lacz_tissue[tissue_name == sub("^lacz_", "", f), tissue_id])]])))
  data.table(field = gsub("_", " ", sub("^lacz_", "lacZ ", f)), Raw = mean(is.na(v) | !valid_raw(f, v)), Cleaned = after)
}))
q2[, field := reorder(field, Raw)]
q2l <- melt(q2, id.vars = "field", variable.name = "stage", value.name = "share")
p1b <- ggplot(q2, aes(y = field)) +
  geom_segment(aes(x = Cleaned, xend = Raw, yend = field), colour = "#bdbdbd", linewidth = 0.6) +
  geom_point(data = q2l, aes(x = share, colour = stage), size = 1.6) +
  scale_colour_manual(values = c(Raw = HL_COL, Cleaned = SIG_COL), labels = c(Raw = "Raw files", Cleaned = "After cleaning"),
                      name = NULL) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0.02, 0.05))) +
  labs(title = "Invalid or missing values before and after cleaning", x = "Records invalid or missing (%)", y = NULL) + th +
  theme(legend.position = c(0.75, 0.15))

dom <- c(gene = "Experimental", allele = "Experimental", strain = "Experimental", life_stage = "Experimental",
         procedure = "Experimental", parameter = "Experimental", parameter_version = "Experimental",
         lacz_tissue = "Experimental", lacz_call = "Experimental", analysis = "Experimental",
         analysis_lacz = "Experimental", parameter_group = "Parameter groups", parameter_group_member = "Parameter groups",
         human_gene = "Disease", disease = "Disease", human_gene_disease = "Disease", mouse_human_ortholog = "Disease",
         disease_model_genotype = "Disease", species = "Homology", ensembl_gene = "Homology", gene_ensembl_xref = "Homology",
         homology_type = "Homology", taxon = "Homology", homology = "Homology", duplicate_source_file = "Provenance",
         data_cleaning_log = "Provenance")
f3 <- data.table(table = names(t), rows = sapply(t, nrow))[, domain := factor(dom[table], levels = unique(dom))]
f3[, table := reorder(table, rows)]
p1c <- ggplot(f3, aes(rows, table, fill = domain)) +
  geom_col(width = 0.7) +
  geom_text(aes(x = rows * 1.25, label = comma(rows)), hjust = 0, size = TXT) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale()), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = CAT[1:5], name = "Schema domain") +
  labs(title = sprintf("IMPC_Group11 database: 26 tables, %s rows", comma(sum(f3$rows))),
       x = "Rows per table (log scale)", y = NULL) + th +
  theme(legend.position = c(0.78, 0.2), axis.text.y = element_text(family = "Courier", size = 6.5))
fig1 <- (p1a | p1b) / p1c + plot_layout(heights = c(1, 1.15))
save_fig(fig1, "Figure1_cleaning_and_database", 250)

# =============================================================================
# Figure 2. Phenotype landscape
# =============================================================================
exp_bin <- nrow(ap[p_value >= 0.5 & p_value < 1]) / 20
p2a <- ggplot(ap[p_value < 1], aes(p_value)) +
  geom_histogram(breaks = seq(0, 1, 0.025), fill = SIG_COL, colour = "white", linewidth = 0.2) +
  geom_hline(yintercept = exp_bin, linetype = "dashed", linewidth = 0.3) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(expand = c(0.01, 0)) +
  annotate("text", x = 0.98, y = exp_bin * 1.9, label = "Level expected\nunder the null", hjust = 1, size = TXT, lineheight = 0.9) +
  annotate("segment", x = 0.9, xend = 0.9, y = exp_bin * 1.55, yend = exp_bin * 1.05, linewidth = 0.25) +
  annotate("text", x = 0.08, y = 2400, label = sprintf("bold('%s analyses with')~bolditalic(P)<bold('0.05')", comma(nrow(ap[p_value < 0.05]))),
           parse = TRUE, hjust = 0, size = TXT) +
  labs(title = expression(bold("Distribution of")~bolditalic(P)~bold("values")), x = expression("Nominal"~italic(P)~"value"), y = "Number of analyses") + th

f5 <- ap[, .(n = .N, hit = mean(p_value < 0.05)), by = procedure_name][n >= 100]
f5[, procedure_name := reorder(procedure_name, hit)]
p2b <- ggplot(f5, aes(hit, procedure_name)) +
  geom_col(fill = SIG_COL, width = 0.7) +
  geom_text(aes(label = comma(n)), hjust = -0.2, size = TXT, colour = PAL$ink2) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Hit rate by phenotyping procedure (number of analyses)",
       x = expression("Analyses with"~italic(P) < 0.05~"(%)"), y = NULL) + th

f7 <- ap[, .(tests = .N, hits = sum(p_value < 0.05)), by = gene_symbol][, q := gene_symbol %in% QUERY]
set.seed(1)
p2c <- ggplot(f7, aes(tests, hits)) +
  geom_point(data = f7[q == FALSE], colour = NS_COL, size = 0.6, position = position_jitter(0.02, 0.08, seed = 1)) +
  geom_point(data = f7[q == TRUE], colour = HL_COL, size = 1.8) +
  geom_text_repel(data = f7[q == TRUE], aes(label = gene_symbol), size = TXT, fontface = "italic",
                  segment.size = 0.2, box.padding = 0.4, seed = 2) +
  scale_x_log10() + scale_y_continuous(trans = "log1p", breaks = c(0, 1, 3, 10, 30, 100)) +
  labs(title = "Breadth of phenotypes per knockout", x = "Phenotypes tested per gene (log scale)",
       y = expression("Phenotypes with"~italic(P) < 0.05~"per gene")) + th
fig2 <- (p2a / p2c) | p2b
fig2 <- fig2 + plot_layout(widths = c(1, 1.15))
save_fig(fig2, "Figure2_phenotype_landscape", 175)

# =============================================================================
# Figure 3. Parameter groups
# =============================================================================
gsz <- merge(gm[, .(params = .N), by = group_name],
             merge(gm, ap, by = "parameter_stable_id", allow.cartesian = TRUE)[, .(analyses = .N), by = group_name], by = "group_name")
gsz[, kind := fifelse(group_name %in% c("Weight", "Images", "Brain"), "Required", "Additional")]
lev <- gsz[order(analyses), group_name]
gsz[, group_name := factor(group_name, levels = lev)]
p3a <- ggplot(gsz, aes(analyses, group_name, fill = kind)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%s (%d)", comma(analyses), params)), hjust = -0.1, size = TXT) +
  scale_fill_manual(values = c(Required = HL_COL, Additional = SIG_COL),
                    labels = c(Required = "Required by brief", Additional = "Added by group"), name = NULL) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Size of the 11 parameter groups (3 required by the brief, 8 added by the group)",
       x = "Number of analyses (number of distinct parameters in brackets)", y = NULL) + th +
  theme(legend.position = c(0.8, 0.25))
pairs <- merge(gm[, .(parameter_stable_id, g1 = group_name)], gm[, .(parameter_stable_id, g2 = group_name)],
               by = "parameter_stable_id", allow.cartesian = TRUE)[, .N, by = .(g1, g2)]
pairs[, `:=`(g1 = factor(g1, levels = lev), g2 = factor(g2, levels = lev))]
full <- CJ(g1 = factor(lev, levels = lev), g2 = factor(lev, levels = lev))[as.integer(g1) > as.integer(g2)]
pairs <- merge(full, pairs, by = c("g1", "g2"), all.x = TRUE)[is.na(N), N := 0]
p3b <- ggplot(pairs, aes(g2, g1, fill = N)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(data = pairs[N > 0], aes(label = N, colour = N > 30), size = TXT) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
  scale_fill_gradientn(colours = c("#f5f5f5", PAL$seq[c(3, 7, 11, 13)]), values = c(0, 0.02, 0.3, 0.7, 1),
                       name = "Shared\nparameters") +
  coord_fixed() + labs(title = "Parameters shared between groups", x = NULL, y = NULL) + th +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.line = element_blank(), axis.ticks = element_blank())
p3a <- p3a + plot_annotation(tag_levels = NULL)
ggsave(file.path(OUT, "Figure3_parameter_groups.pdf"), p3a, width = FULL, height = 100 * MM, device = pdf, useDingbats = FALSE)
ggsave(file.path(OUT, "Figure3_parameter_groups.png"), p3a, width = FULL, height = 100 * MM, dpi = 600, bg = "white")
cat("wrote Figure3_parameter_groups\n")

# =============================================================================
# Figure 4. Collaborator's genes of interest
# =============================================================================
qg <- ap[gene_symbol %in% QUERY]
ord_q <- qg[, .(s = sum(p_value < 0.05)), by = gene_symbol][order(-s), gene_symbol]
qg[, gene_symbol := factor(gene_symbol, levels = ord_q)]
qg <- qg[order(gene_symbol, procedure_name, parameter_name)][, idx := seq_len(.N), by = gene_symbol]
qg[, `:=`(score = neglog(p_value), sig = p_value < 0.05, band = as.integer(factor(procedure_name)) %% 2 == 0)]
lab4 <- qg[sig == TRUE][order(p_value)][, .SD[!duplicated(parameter_name)][1:min(.N, 2)], by = gene_symbol]
nn <- qg[, .(lab = sprintf("italic(%s)~'(%d tested, %d with'~italic(P)<0.05*')'", gene_symbol[1], .N, sum(sig))), by = gene_symbol]
p4a <- ggplot(qg, aes(idx, score)) +
  geom_hline(yintercept = neglog(0.05), linetype = "dashed", linewidth = 0.25) +
  geom_point(aes(colour = fifelse(sig, "s", fifelse(band, "a", "b"))), size = 0.7) +
  geom_text_repel(data = lab4, aes(label = parameter_name), size = 2.5, max.overlaps = Inf, nudge_y = 7,
                  direction = "both", force = 4, segment.size = 0.15, box.padding = 0.35, seed = 4) +
  scale_colour_manual(values = c(s = SIG_COL, a = "#8c8c8c", b = "#c8c8c8"), guide = "none") +
  facet_wrap(~ gene_symbol, ncol = 1, scales = "free_x", labeller = as_labeller(setNames(nn$lab, nn$gene_symbol), default = label_parsed),
             strip.position = "top") +
  scale_y_continuous(limits = c(0, SCORE_CAP + 9), breaks = c(0, 10, 20)) +
  labs(title = "Phenotype scores per knockout", x = "Phenotypes tested, ordered by procedure",
       y = expression(-log[10]~italic(P))) + th +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.text = element_text(face = "plain", size = 7),
        panel.spacing.y = unit(2, "pt"))

f9 <- merge(qg, gm[, .(parameter_stable_id, group_name)], by = "parameter_stable_id", allow.cartesian = TRUE)[
  , .(n_sig = sum(sig), best = max(score), n = .N), by = .(gene_symbol, group_name)]
f9 <- merge(CJ(gene_symbol = ord_q, group_name = lev), f9, by = c("gene_symbol", "group_name"), all.x = TRUE)
f9[, `:=`(gene_symbol = factor(gene_symbol, levels = ord_q), group_name = factor(group_name, levels = lev))]
p4b <- ggplot(f9, aes(gene_symbol, group_name)) +
  geom_tile(aes(fill = best), colour = "white", linewidth = 0.5) +
  geom_text(data = f9[!is.na(n)], aes(label = sprintf("%d/%d", n_sig, n), colour = best > 8), size = TXT) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
  scale_fill_gradientn(colours = c("#ffffff", PAL$seq[c(2, 6, 10, 13)]), values = c(0, 0.065, 0.25, 0.6, 1),
                       limits = c(0, SCORE_CAP), na.value = "#eeeeee", name = expression("Strongest effect,"~-log[10]~italic(P))) +
  scale_x_discrete(position = "top") +
  labs(title = "Hits per parameter group", x = NULL, y = NULL) + th +
  theme(axis.text.x = element_text(face = "italic"), axis.line = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", legend.title.position = "top", legend.key.width = unit(1.6, "lines"),
        legend.key.height = unit(0.45, "lines"))
fig4 <- p4a | p4b
fig4 <- fig4 + plot_layout(widths = c(1.35, 1))
save_fig(fig4, "Figure4_genes_of_interest", 200)

# =============================================================================
# Figure 5. Gene clustering
# =============================================================================
sl <- merge(ap, gm[, .(parameter_stable_id, feature = group_name)], by = "parameter_stable_id", allow.cartesian = TRUE)[
  , .(score = max(neglog(p_value, 300))), by = .(mgi_gene_id, gene_symbol, feature)]
m <- build_matrix(sl, min_features = 4, top_features = 40)
res <- cluster_genes(m, k = 5, method = "hierarchical")
pv <- round(100 * res$pca$sdev^2 / sum(res$pca$sdev^2), 1)
cl <- res$cluster
pcd <- data.table(gene = rownames(m), PC1 = res$pca$x[, 1], PC2 = res$pca$x[, 2], cluster = factor(paste0("C", cl)))
cn <- pcd[, .N, by = cluster]; levels(pcd$cluster) <- sprintf("%s (n = %d)", levels(pcd$cluster), cn$N[match(levels(pcd$cluster), cn$cluster)])
p5a <- ggplot(pcd, aes(PC1, PC2, colour = cluster)) +
  geom_point(size = 0.7, alpha = 0.8) +
  geom_point(data = pcd[gene %in% QUERY], shape = 21, size = 2.6, stroke = 0.5, colour = "black") +
  geom_text_repel(data = pcd[gene %in% QUERY], aes(label = gene), colour = "black", size = TXT, fontface = "italic",
                  box.padding = 0.5, segment.size = 0.2, seed = 3) +
  scale_colour_manual(values = CAT[1:5], name = NULL) +
  labs(title = "Genes in phenotype space", colour = "Cluster",
       x = sprintf("Principal component 1 (%.1f%% of variance)", pv[1]),
       y = sprintf("Principal component 2 (%.1f%%)", pv[2])) + th +
  theme(legend.position = c(0.55, 0.3)) + guides(colour = guide_legend(override.aes = list(size = 2.2)))
prof <- rbindlist(lapply(sort(unique(cl)), function(k) data.table(
  cluster = sprintf("C%d", k), feature = colnames(m), share = colMeans(m[cl == k, , drop = FALSE] > -log10(0.05)))))
qin <- data.table(gene = names(cl), k = cl)[gene %in% QUERY][, .(g = paste(gene, collapse = ", ")), by = k]
clab <- sapply(sort(unique(cl)), function(kk) { x <- qin[k == kk, g]; if (length(x)) sprintf("C%d (%s)", kk, x) else sprintf("C%d", kk) })
prof[, cluster := factor(clab[as.integer(sub("C", "", cluster))], levels = rev(clab))]
prof[, feature := factor(feature, levels = prof[, .(v = max(share)), by = feature][order(-v), feature])]
p5b <- ggplot(prof, aes(feature, cluster, fill = share)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(100 * share), colour = share > 0.5), size = TXT) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none") +
  scale_fill_gradientn(colours = c("#ffffff", PAL$seq[c(2, 6, 10, 13)]), limits = c(0, 1), labels = percent,
                       name = expression("Genes with"~italic(P) < 0.05)) +
  labs(title = expression(bold("Cluster members with"~bolditalic(P) < 0.05~"(%)")), x = NULL, y = NULL) + th +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), axis.line = element_blank(), axis.ticks = element_blank(),
        legend.key.height = unit(0.9, "lines"), legend.key.width = unit(0.4, "lines"))
fig5 <- free(p5a) / p5b + plot_layout(heights = c(1.15, 1))
save_fig(fig5, "Figure5_gene_clusters", 225)

# =============================================================================
# Figure 6. Homology (bespoke task), disease links and lacZ expression
# =============================================================================
h <- merge(t$homology, t$ensembl_gene[, .(ensembl_gene_id, species_id)], by.x = "homolog_ensembl_gene_id", by.y = "ensembl_gene_id")
h <- merge(merge(h, t$species[, .(species_id, species_name)], by = "species_id"), t$homology_type, by = "homology_type_id")
h[, homology_type := factor(sub("ortholog_", "", homology_type), levels = c("one2one", "one2many", "many2many"),
                            labels = c("One-to-one", "One-to-many", "Many-to-many"))]
p6a <- ggplot(h, aes(homology_type, pct_identity, fill = species_name)) +
  geom_violin(scale = "width", width = 0.8, linewidth = 0.2, colour = "black", position = position_dodge(0.85)) +
  geom_boxplot(aes(group = interaction(species_name, homology_type)), width = 0.12, outlier.shape = NA,
               linewidth = 0.25, fill = "white", position = position_dodge(0.85)) +
  scale_fill_manual(values = c(Human = SIG_COL, `Vervet-AGM` = HL_COL), labels = c(Human = "Human", `Vervet-AGM` = "Vervet (African green monkey)"),
                    name = "Species") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Conservation of mouse homologs (bespoke task)", x = "Ensembl orthology type",
       y = "Sequence identity to mouse gene (%)") + th + theme(legend.position = "top")

dg <- unique(t$mouse_human_ortholog[hgnc_id %in% t$human_gene_disease$hgnc_id, .(mgi_gene_id)])
gstat <- ap[, .(tests = .N, hit_rate = mean(p_value < 0.05)), by = mgi_gene_id][tests >= 5]
gstat[, disease := factor(fifelse(mgi_gene_id %in% dg$mgi_gene_id, "DO-linked", "No DO link"), levels = c("No DO link", "DO-linked"))]
wt <- wilcox.test(hit_rate ~ disease, data = gstat)
p6b <- ggplot(gstat, aes(disease, hit_rate)) +
  geom_boxplot(aes(fill = disease), width = 0.55, outlier.size = 0.3, linewidth = 0.3) +
  scale_fill_manual(values = c(NS_COL, HL_COL), guide = "none") +
  annotate("text", x = 1.5, y = 1.02, label = sprintf("Wilcoxon~italic(P)==%.2g", wt$p.value), parse = TRUE, size = TXT) +
  scale_y_continuous(labels = percent, limits = c(0, 1.05)) +
  scale_x_discrete(labels = function(x) paste0(x, "\n(n = ", comma(as.numeric(table(gstat$disease)[x])), ")")) +
  labs(title = "Phenotype hit rate and human disease link", x = NULL,
       y = expression("Tests with"~italic(P) < 0.05~"per gene (%)")) + th

lz <- merge(merge(t$analysis_lacz[analysis_id %in% a$analysis_id], t$lacz_tissue, by = "tissue_id"), t$lacz_call, by = "call_id")
lz <- unique(merge(lz, t$analysis[, .(analysis_id, mgi_gene_id)], by = "analysis_id")[, .(mgi_gene_id, tissue_name, call_label)])
lz <- lz[, .(call_label = names(sort(table(call_label), decreasing = TRUE))[1]), by = .(mgi_gene_id, tissue_name)]
f14 <- lz[, .N, by = .(tissue_name, call_label)][, share := N / sum(N), by = tissue_name]
ord_calls <- c("expression", "ambiguous", "imageOnly", "no expression", "tissue not available", "No data")
f14[, call_label := factor(call_label, levels = rev(ord_calls))]
f14[, tissue := reorder(gsub("_", " ", tissue_name), share * (call_label == "expression"), sum)]
p6c <- ggplot(f14, aes(share, tissue, fill = call_label)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = setNames(c(SIG_COL, PAL$cat[["yellow"]], PAL$cat[["magenta"]], PAL$cat[["violet"]], "#9e9e9e", "#dcdcdc"),
                                      ord_calls), breaks = ord_calls, name = "lacZ call") +
  scale_x_continuous(labels = percent, expand = c(0, 0)) +
  labs(title = "lacZ reporter expression by tissue", x = "Knockout genes (%)", y = NULL, fill = "lacZ call") + th +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, keywidth = unit(0.6, "lines"), keyheight = unit(0.6, "lines")))
fig6 <- (p6a | p6b) / p6c + plot_layout(heights = c(1, 1), widths = c(1.6, 1))
save_fig(fig6, "Figure6_homology_disease_lacz", 195)


# =============================================================================
# Figure 7. Data processing flow (from TRE files to the database)
# =============================================================================
n_files <- nrow(raw); n_struct <- sum(!is.na(raw$import_issue) & raw$import_issue != "")
n_dupf <- nrow(t$duplicate_source_file); n_an <- nrow(t$analysis)
n_rep <- sum(!is.na(t$analysis$content_duplicate_of)); n_unique <- n_an - n_rep
n_p <- sum(!is.na(t$analysis[is.na(content_duplicate_of)]$p_value))
pd_raw <- nrow(fread("IMPC_parameter_description.txt", colClasses = "character"))
hml_rows <- length(readLines("bespokeTask_hml.txt", warn = FALSE)) - 1
mouse_ens <- t$ensembl_gene[species_id == 1, .N]
box <- function(x, y, label, w = 0.315, h = 0.09, fill = "#f2f6fc", bold = FALSE)
  list(annotate("rect", xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2,
                fill = fill, colour = "black", linewidth = 0.3),
       annotate("text", x = x, y = y, label = label, size = TXT * 0.92, lineheight = 0.95, fontface = if (bold) "bold" else "plain"))
arr <- function(x1, y1, x2, y2) annotate("segment", x = x1, y = y1, xend = x2, yend = y2, linewidth = 0.35,
                                        arrow = arrow(length = unit(1.6, "mm"), type = "closed"))
side <- function(x, y, label) list(annotate("segment", x = x, xend = x + 0.04, y = y, yend = y, linewidth = 0.3, linetype = "dashed"),
                                  annotate("text", x = x + 0.045, y = y, label = label, hjust = 0, size = TXT * 0.85, colour = PAL$ink2, lineheight = 0.9))
X1 <- 0.165; X2 <- 0.51; X3 <- 0.855
p7 <- ggplot() + xlim(0, 1.02) + ylim(0, 1) + theme_void() +
  annotate("text", x = c(X1, X2, X3), y = 0.985, label = c("Experimental analyses", "IMPC reference & disease", "Bespoke homology (Ensembl)"),
           fontface = "bold", size = TXT * 1.05) +
  box(X1, 0.90, sprintf("%s analysis files\n(key-value CSV, TRE egress)", comma(n_files))) +
  box(X1, 0.75, sprintf("Position-aware parsing\n%d malformed files repaired", n_struct)) +
  box(X1, 0.60, sprintf("Field cleaning (vocabulary, reference,\nconsensus): %s values corrected", comma(nrow(t$data_cleaning_log)))) +
  box(X1, 0.45, sprintf("%s analyses\n(%d duplicate files merged)", comma(n_an), n_dupf)) +
  box(X1, 0.30, sprintf("%s unique analyses\n(%s repeats flagged)", comma(n_unique), comma(n_rep))) +
  arr(X1, 0.857, X1, 0.793) + arr(X1, 0.707, X1, 0.643) + arr(X1, 0.557, X1, 0.493) + arr(X1, 0.407, X1, 0.343) +
  box(X2, 0.90, sprintf("Parameter reference\n%s rows", comma(pd_raw))) +
  box(X2, 0.75, sprintf("%s parameters, %d procedures\n(14 exact duplicates removed)", comma(nrow(t$parameter_version)), nrow(t$procedure))) +
  box(X2, 0.60, "11 parameter groups\n(3 required + 8 added)", fill = "#fdeee7") +
  box(X2, 0.45, sprintf("Disease Ontology report\n%s gene-disease rows", comma(nrow(t$human_gene_disease)))) +
  box(X2, 0.30, sprintf("%s human genes, %s diseases\n%s model genotypes", comma(nrow(t$human_gene)), comma(nrow(t$disease)),
                        comma(nrow(t$disease_model_genotype)))) +
  arr(X2, 0.857, X2, 0.793) + arr(X2, 0.707, X2, 0.643) + arr(X2, 0.407, X2, 0.343) +
  box(X3, 0.90, sprintf("BioMart export\n%s wide rows", comma(hml_rows))) +
  box(X3, 0.75, sprintf("Reshaped to long format\n%s species-pair rows", comma(110993))) +
  box(X3, 0.60, sprintf("%s unique homology pairs", comma(nrow(t$homology)))) +
  box(X3, 0.45, sprintf("%s mouse genes kept\n(%s without homolog excluded)", comma(mouse_ens), comma(57380))) +
  box(X3, 0.30, sprintf("%s linked to MGI genes\n(exact symbol match)", comma(nrow(t$gene_ensembl_xref)))) +
  arr(X3, 0.857, X3, 0.793) + arr(X3, 0.707, X3, 0.643) + arr(X3, 0.557, X3, 0.493) + arr(X3, 0.407, X3, 0.343) +
  box(0.5, 0.09, sprintf("IMPC_Group11 MySQL database\n26 normalised tables, %s rows, 4 views; dump restored and row counts verified",
                         comma(sum(sapply(t, nrow)))), w = 0.72, h = 0.1, fill = "#e3edf9", bold = TRUE) +
  arr(X1, 0.257, 0.36, 0.142) + arr(X2, 0.257, X2, 0.142) + arr(X3, 0.257, 0.70, 0.142)
ggsave(file.path(OUT, "Figure7_processing_flow.pdf"), p7, width = FULL, height = 150 * MM, device = pdf, useDingbats = FALSE)
ggsave(file.path(OUT, "Figure7_processing_flow.png"), p7, width = FULL, height = 150 * MM, dpi = 600, bg = "white")
cat("wrote Figure7_processing_flow\n")

# =============================================================================
# Figure 8. The parameter groupings in depth (3 required + 8 added)
# =============================================================================
memb <- gm[, .(combo = paste(sort(group_name), collapse = " + "), k = .N), by = parameter_stable_id]
top <- memb[, .N, by = combo][order(-N)][1:14]
top[, combo := factor(combo, levels = combo)]
grp_order <- gsz[order(-analyses), as.character(group_name)]
mat8 <- top[, .(group = unlist(strsplit(as.character(combo), " + ", fixed = TRUE))), by = combo]
dots <- CJ(combo = top$combo, group = factor(grp_order, levels = rev(grp_order)))
dots[, on := paste(combo, group) %in% paste(mat8$combo, mat8$group)]
p8a_top <- ggplot(top, aes(combo, N)) + geom_col(fill = "#4d4d4d", width = 0.7) +
  geom_text(aes(label = N), vjust = -0.4, size = TXT) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Parameter-group membership and overlaps (UpSet plot)", x = NULL, y = "Parameters") + th +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank())
lines8 <- dots[on == TRUE, .(ymin = min(as.integer(group)), ymax = max(as.integer(group))), by = combo]
p8a_mat <- ggplot(dots, aes(combo, group)) +
  geom_point(aes(colour = on), size = 2.4) +
  geom_segment(data = lines8, aes(x = combo, xend = combo, y = ymin, yend = ymax), inherit.aes = FALSE, linewidth = 0.6) +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "#e0e0e0"), guide = "none") +
  scale_y_discrete(labels = function(x) ifelse(x %in% c("Weight", "Images", "Brain"), paste0(x, " *"), x)) +
  labs(x = NULL, y = NULL) + th +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(), axis.line = element_blank())
wilson <- function(x, n, z = 1.96) { p <- x / n; d <- 1 + z^2 / n
  cbind(lo = (p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d,
        hi = (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d) }
gh <- merge(gm, ap, by = "parameter_stable_id", allow.cartesian = TRUE)[, .(x = sum(p_value < 0.05), n = .N), by = group_name]
gh[, c("lo", "hi") := as.data.table(wilson(x, n))][, rate := x / n]
gh[, kind := fifelse(group_name %in% c("Weight", "Images", "Brain"), "Required by brief", "Added by group")]
gh[, group_name := reorder(group_name, rate)]
p8b <- ggplot(gh, aes(rate, group_name, colour = kind)) +
  geom_vline(xintercept = mean(ap$p_value < 0.05), linetype = "dashed", linewidth = 0.3) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.3, linewidth = 0.4) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = c(`Required by brief` = HL_COL, `Added by group` = SIG_COL), name = NULL) +
  scale_x_continuous(labels = percent) +
  labs(title = "Hit rate per group (95% CI)", x = expression("Analyses with"~italic(P) < 0.05~"(%)"), y = NULL) + th +
  theme(legend.position = "bottom")
gg <- merge(gm, ap, by = "parameter_stable_id", allow.cartesian = TRUE)[, .(tested = uniqueN(group_name),
        hit = uniqueN(group_name[p_value < 0.05])), by = gene_symbol][tested >= 5]
p8c <- ggplot(gg, aes(factor(hit))) + geom_bar(fill = SIG_COL, width = 0.7) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4, size = TXT) +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.3, size = TXT, lineheight = 1.05,
           label = paste0("Genes of interest:\n", paste(gg[gene_symbol %in% QUERY][order(hit), sprintf("%s: %d groups", gene_symbol, hit)],
                                                       collapse = "\n"))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Pleiotropy per knockout",
       x = expression("Parameter groups with"~italic(P) < 0.05), y = "Knockout genes (tested in ≥ 5 groups)") + th
fig8 <- wrap_elements(full = (p8a_top / p8a_mat + plot_layout(heights = c(1, 1.1)))) / (p8b | p8c) + plot_layout(heights = c(2.1, 1.3))
save_fig(fig8, "Figure8_parameter_groups_in_depth", 235)

# =============================================================================
# Figure 9. Clustered heatmap: knockouts x procedures (pheatmap)
# =============================================================================
suppressPackageStartupMessages(library(pheatmap))
gp <- ap[procedure_name != "No IMPC procedure", .(score = min(max(neglog(p_value, 300)), 10)), by = .(gene_symbol, procedure_name)]
gcount <- gp[, .N, by = gene_symbol][order(-N)]
genes9 <- unique(c(head(gcount$gene_symbol, 60), intersect(QUERY, gp$gene_symbol)))
gp9 <- gp[gene_symbol %in% genes9]
pcov <- gp9[, .N, by = procedure_name][N >= 0.4 * length(genes9), procedure_name]
w9 <- dcast(gp9[procedure_name %in% pcov], gene_symbol ~ procedure_name, value.var = "score", fill = 0)
m9 <- as.matrix(w9[, -1]); rownames(m9) <- w9$gene_symbol
ann <- data.frame(`Gene of interest` = ifelse(rownames(m9) %in% QUERY, "Yes", "No"), check.names = FALSE, row.names = rownames(m9))
lab_row <- ifelse(rownames(m9) %in% QUERY, paste0(rownames(m9), "  *"), rownames(m9))
ph <- pheatmap(m9, color = colorRampPalette(c("#ffffff", PAL$seq[c(2, 5, 8, 11, 13)]))(100), breaks = seq(0, 10, length.out = 101),
               border_color = NA, clustering_method = "ward.D2", fontsize = 10, fontsize_row = 7.5, fontsize_col = 9,
               angle_col = 90, treeheight_row = 40, treeheight_col = 30, annotation_row = ann, annotation_names_row = FALSE,
               annotation_colors = list(`Gene of interest` = c(Yes = HL_COL, No = "#eeeeee")), labels_row = lab_row,
               legend_breaks = c(0, 2.5, 5, 7.5, 10), legend_labels = c("0", "2.5", "5", "7.5", "10"), silent = TRUE)
pdf(file.path(OUT, "Figure9_clustered_heatmap.pdf"), width = FULL, height = 250 * MM, useDingbats = FALSE)
grid::grid.draw(ph$gtable); dev.off()
png(file.path(OUT, "Figure9_clustered_heatmap.png"), width = FULL, height = 250 * MM, units = "in", res = 600)
grid::grid.draw(ph$gtable); dev.off()
cat("wrote Figure9_clustered_heatmap\n")

# =============================================================================
# Figure 10. Conservation in two primates; hit rates by life stage and strain
# =============================================================================
h1 <- dcast(h[homology_type == "One-to-one"], mouse_ensembl_gene_id ~ species_name, value.var = "pct_identity", fun.aggregate = max)
h1 <- h1[is.finite(Human) & is.finite(`Vervet-AGM`)]
rho <- cor(h1$Human, h1$`Vervet-AGM`, method = "spearman")
p10a <- ggplot(h1, aes(Human, `Vervet-AGM`)) +
  geom_bin2d(bins = 60) +
  geom_abline(linetype = "dashed", linewidth = 0.3) +
  scale_fill_gradientn(colours = PAL$seq[c(1, 4, 8, 11, 13)], trans = "log10", name = "Genes") +
  annotate("text", x = 100, y = 3, label = sprintf("Spearman~rho==%.2f~~(italic(n)=='%s')", rho, format(nrow(h1), big.mark = ",")),
           parse = TRUE, hjust = 1, size = TXT) +
  theme(aspect.ratio = 1) + labs(title = "One-to-one orthologs: human vs vervet",
                       x = "Identity of human ortholog (%)", y = "Identity of vervet ortholog (%)") + th
a2 <- merge(merge(ap, t$strain, by = "strain_id"), t$life_stage[, .(life_stage_id, life_stage_name)], by = "life_stage_id", all.x = TRUE)
ls_ <- a2[!is.na(life_stage_name), .(x = sum(p_value < 0.05), n = .N), by = life_stage_name]
ls_[, c("lo", "hi") := as.data.table(wilson(x, n))][, rate := x / n]
ls_[, life_stage_name := factor(life_stage_name, levels = c("E9.5", "E12.5", "E15.5", "E18.5", "Early adult", "Middle aged adult", "Late adult"))]
p10b <- ggplot(ls_, aes(life_stage_name, rate)) +
  geom_col(fill = SIG_COL, width = 0.65) + geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, linewidth = 0.35) +
  geom_text(aes(y = hi, label = comma(n)), vjust = -0.5, size = TXT * 0.9) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(labels = c("E9.5", "E12.5", "E15.5", "E18.5", "Early\nadult", "Middle-\naged", "Late\nadult")) +
  labs(title = "Hit rate by life stage", x = NULL, y = expression("Analyses with"~italic(P) < 0.05~"(%)")) + th
st <- a2[, .(x = sum(p_value < 0.05), n = .N), by = strain_name][n >= 50]
st[, c("lo", "hi") := as.data.table(wilson(x, n))][, rate := x / n][, strain_name := reorder(strain_name, rate)]
p10c <- ggplot(st, aes(rate, strain_name)) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.3, linewidth = 0.4) + geom_point(size = 2.4, colour = SIG_COL) +
  geom_text(aes(x = hi, label = comma(n)), hjust = -0.3, size = TXT * 0.9) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0.05, 0.2))) +
  labs(title = "Hit rate by genetic background", x = expression("Analyses with"~italic(P) < 0.05~"(%)"), y = NULL) + th
fig10 <- (p10a | p10b) / free(p10c) + plot_layout(heights = c(1.25, 0.8))
save_fig(fig10, "Figure10_conservation_stage_strain", 185)

writeLines(capture.output(print(wt)), file.path(OUT, "Figure6B_wilcoxon.txt"))
cat("Done.\n")
