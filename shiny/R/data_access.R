# =============================================================================
# Data access layer for the IMPC Group 11 dashboard.
# Uses parameterised queries against the MySQL database when available and
# falls back to a bundled snapshot (data/dashboard_snapshot.rds) otherwise, so
# the app can always be launched from the GitHub repository.
# =============================================================================
suppressPackageStartupMessages({ library(DBI); library(data.table) })

find_cnf <- function() {
  cands <- c(Sys.getenv("IMPC_DB_CNF"), "db_config.cnf", "../db_config.cnf", "~/.my.cnf")
  cands <- cands[nzchar(cands)]
  hit <- cands[file.exists(cands)]
  if (length(hit)) normalizePath(hit[1]) else NA_character_
}

open_data_source <- function() {
  cnf <- find_cnf()
  if (!is.na(cnf) && requireNamespace("RMariaDB", quietly = TRUE)) {
    con <- tryCatch(dbConnect(RMariaDB::MariaDB(), default.file = cnf, group = "impc", bigint = "integer"),
                    error = function(e) NULL)
    if (!is.null(con) && dbExistsTable(con, "analysis"))
      return(list(type = "mysql", con = con,
                  label = paste0("MySQL: ", dbGetQuery(con, "SELECT DATABASE() d")$d)))
  }
  snap <- c("data/dashboard_snapshot.rds", "shiny/data/dashboard_snapshot.rds")
  snap <- snap[file.exists(snap)][1]
  if (is.na(snap)) stop("No database connection and no snapshot found.")
  s <- readRDS(snap)
  list(type = "snapshot", snap = s, label = "Offline snapshot of IMPC_Group11")
}

q <- function(ds, sql, params = NULL) {
  as.data.table(if (is.null(params)) dbGetQuery(ds$con, sql) else dbGetQuery(ds$con, sql, params = params))
}

# ---- lookup lists -----------------------------------------------------------
list_genes <- function(ds) {
  if (ds$type == "mysql")
    q(ds, "SELECT g.mgi_gene_id, g.gene_symbol, COUNT(*) n_tests
           FROM analysis a JOIN gene g ON g.mgi_gene_id = a.mgi_gene_id
           GROUP BY g.mgi_gene_id, g.gene_symbol ORDER BY g.gene_symbol")
  else ds$snap$analysis[, .(n_tests = .N), by = .(mgi_gene_id, gene_symbol)][order(gene_symbol)]
}

list_parameters <- function(ds) {
  if (ds$type == "mysql")
    q(ds, "SELECT a.parameter_stable_id, p.parameter_name, COALESCE(pr.procedure_name,'(unknown)') procedure_name,
                  COUNT(DISTINCT a.mgi_gene_id) n_genes
           FROM analysis a JOIN parameter p ON p.parameter_stable_id = a.parameter_stable_id
           LEFT JOIN `procedure` pr ON pr.procedure_id = p.procedure_id
           GROUP BY a.parameter_stable_id, p.parameter_name, pr.procedure_name
           ORDER BY n_genes DESC")
  else ds$snap$analysis[!is.na(parameter_stable_id),
                        .(n_genes = uniqueN(mgi_gene_id)),
                        by = .(parameter_stable_id, parameter_name,
                               procedure_name = fifelse(is.na(procedure_name), "(unknown)", procedure_name))][order(-n_genes)]
}

list_groups <- function(ds) {
  if (ds$type == "mysql") q(ds, "SELECT group_id, group_name FROM parameter_group ORDER BY group_name")
  else ds$snap$parameter_group[order(group_name), .(group_id, group_name)]
}

# ---- visualisation 1: one gene, all phenotypes --------------------------------
gene_results <- function(ds, mgi_id) {
  if (ds$type == "mysql") {
    x <- q(ds, "SELECT v.analysis_id, v.gene_symbol, v.allele_symbol, v.life_stage_name, v.strain_name,
                       v.parameter_stable_id, v.parameter_name,
                       COALESCE(v.procedure_name,'(unknown)') procedure_name, v.p_value, f.q_value_bh, v.p_value_flag
                FROM v_analysis_full v LEFT JOIN v_analysis_fdr f ON f.analysis_id = v.analysis_id
                WHERE v.mgi_gene_id = ? AND v.p_value IS NOT NULL AND v.content_duplicate_of IS NULL", list(mgi_id))
    grp <- q(ds, "SELECT m.parameter_stable_id, GROUP_CONCAT(pg.group_name ORDER BY pg.group_name SEPARATOR '; ') parameter_groups
                  FROM parameter_group_member m JOIN parameter_group pg ON pg.group_id = m.group_id
                  WHERE m.parameter_stable_id IN (SELECT parameter_stable_id FROM analysis WHERE mgi_gene_id = ?)
                  GROUP BY m.parameter_stable_id", list(mgi_id))
    x <- merge(x, grp, by = "parameter_stable_id", all.x = TRUE)
  } else {
    x <- ds$snap$analysis[mgi_gene_id == mgi_id & !is.na(p_value) & is.na(content_duplicate_of)]
    x[, mgi_gene_id := mgi_id]
    x[is.na(procedure_name), procedure_name := "(unknown)"]
  }
  x[is.na(parameter_name), parameter_name := parameter_stable_id]
  x[]
}

gene_disease <- function(ds, mgi_id) {
  if (ds$type == "mysql")
    q(ds, "SELECT human_symbol, doid, disease_name, n_mouse_model_genotypes FROM v_gene_disease
           WHERE mgi_gene_id = ? ORDER BY disease_name", list(mgi_id))
  else ds$snap$gene_disease[mgi_gene_id == mgi_id, .(human_symbol, doid, disease_name, n_mouse_model_genotypes)]
}

gene_homology <- function(ds, mgi_id) {
  if (ds$type == "mysql")
    q(ds, "SELECT species_name, homolog_ensembl_gene_id, homolog_gene_name, homology_type,
                  last_common_ancestor, pct_identity
           FROM v_gene_homology WHERE mgi_gene_id = ? ORDER BY species_name, pct_identity DESC", list(mgi_id))
  else ds$snap$gene_homology[mgi_gene_id == mgi_id, .(species_name, homolog_ensembl_gene_id, homolog_gene_name,
                                                      homology_type, last_common_ancestor, pct_identity)]
}

# ---- visualisation 2: one phenotype, all genes -----------------------------------
parameter_results <- function(ds, pid) {
  if (ds$type == "mysql")
    q(ds, "SELECT analysis_id, mgi_gene_id, gene_symbol, allele_symbol, life_stage_name, strain_name,
                  parameter_name, COALESCE(procedure_name,'(unknown)') procedure_name, p_value, p_value_flag
           FROM v_analysis_full WHERE parameter_stable_id = ? AND p_value IS NOT NULL AND content_duplicate_of IS NULL", list(pid))
  else ds$snap$analysis[parameter_stable_id == pid & !is.na(p_value) & is.na(content_duplicate_of)]
}

# ---- visualisation 3: gene x feature score matrix --------------------------------
# value = max(-log10 p) of a gene within a feature (parameter group / procedure /
# parameter). Untested gene-feature combinations are set to 0 (= p of 1, i.e.
# no evidence of an effect); genes must be tested in >= min_features features.
score_long <- function(ds, level = c("group", "procedure", "parameter")) {
  level <- match.arg(level)
  if (ds$type == "mysql") {
    sql <- switch(level,
      group = "SELECT a.mgi_gene_id, g.gene_symbol, pg.group_name feature,
                      MAX(-LOG10(GREATEST(a.p_value,1e-300))) score
               FROM analysis a JOIN gene g ON g.mgi_gene_id = a.mgi_gene_id
               JOIN parameter_group_member m ON m.parameter_stable_id = a.parameter_stable_id
               JOIN parameter_group pg ON pg.group_id = m.group_id
               WHERE a.p_value IS NOT NULL AND a.p_value <= 1
               GROUP BY a.mgi_gene_id, g.gene_symbol, pg.group_name",
      procedure = "SELECT a.mgi_gene_id, g.gene_symbol, pr.procedure_name feature,
                      MAX(-LOG10(GREATEST(a.p_value,1e-300))) score
               FROM analysis a JOIN gene g ON g.mgi_gene_id = a.mgi_gene_id
               JOIN parameter p ON p.parameter_stable_id = a.parameter_stable_id
               JOIN `procedure` pr ON pr.procedure_id = p.procedure_id
               WHERE a.p_value IS NOT NULL AND a.p_value <= 1
               GROUP BY a.mgi_gene_id, g.gene_symbol, pr.procedure_name",
      parameter = "SELECT a.mgi_gene_id, g.gene_symbol, a.parameter_stable_id feature,
                      MAX(-LOG10(GREATEST(a.p_value,1e-300))) score
               FROM analysis a JOIN gene g ON g.mgi_gene_id = a.mgi_gene_id
               WHERE a.p_value IS NOT NULL AND a.p_value <= 1 AND a.parameter_stable_id IS NOT NULL
               GROUP BY a.mgi_gene_id, g.gene_symbol, a.parameter_stable_id")
    q(ds, sql)
  } else {
    a <- ds$snap$analysis[!is.na(p_value) & p_value <= 1]
    a[, s := -log10(pmax(p_value, 1e-300))]
    switch(level,
      group = merge(a, ds$snap$group_member, by = "parameter_stable_id", allow.cartesian = TRUE)[
        , .(score = max(s)), by = .(mgi_gene_id, gene_symbol, feature = group_name)],
      procedure = a[!is.na(procedure_name), .(score = max(s)), by = .(mgi_gene_id, gene_symbol, feature = procedure_name)],
      parameter = a[!is.na(parameter_stable_id), .(score = max(s)), by = .(mgi_gene_id, gene_symbol, feature = parameter_stable_id)])
  }
}

# ---- extra datasets for the overview / groups / disease / quality tabs -------
EXTRA_SQL <- list(
  cleaning = "SELECT field_name, rule, COUNT(*) n FROM data_cleaning_log GROUP BY field_name, rule",
  cleaning_examples = "SELECT field_name, raw_value, clean_value, rule, COUNT(*) n FROM data_cleaning_log
                       WHERE raw_value IS NOT NULL GROUP BY field_name, raw_value, clean_value, rule ORDER BY n DESC LIMIT 3000",
  table_rows = "SELECT table_name AS tn FROM information_schema.tables
                WHERE table_schema = DATABASE() AND table_type = 'BASE TABLE'",
  disease_all = "SELECT o.mgi_gene_id, g.gene_symbol, g.in_impc_data, d.doid, d.disease_name, h.human_symbol
                 FROM mouse_human_ortholog o JOIN gene g ON g.mgi_gene_id = o.mgi_gene_id
                 JOIN human_gene h ON h.hgnc_id = o.hgnc_id
                 JOIN human_gene_disease hd ON hd.hgnc_id = o.hgnc_id JOIN disease d ON d.doid = hd.doid",
  homology_all = "SELECT hm.mouse_ensembl_gene_id, sp.species_name, ht.homology_type, hm.pct_identity
                  FROM homology hm JOIN ensembl_gene eg ON eg.ensembl_gene_id = hm.homolog_ensembl_gene_id
                  JOIN species sp ON sp.species_id = eg.species_id
                  JOIN homology_type ht ON ht.homology_type_id = hm.homology_type_id",
  quality = "SELECT (SELECT COUNT(*) FROM analysis) analyses, (SELECT COUNT(*) FROM duplicate_source_file) dup_files,
             (SELECT COUNT(*) FROM analysis WHERE content_duplicate_of IS NOT NULL) repeats,
             (SELECT COUNT(*) FROM analysis WHERE import_issue IS NOT NULL) import_repairs,
             (SELECT COUNT(*) FROM analysis WHERE p_value IS NULL) p_missing,
             (SELECT COUNT(*) FROM analysis WHERE parameter_id_source = 'recovered_from_unique_parameter_name') pid_recovered,
             (SELECT COUNT(*) FROM data_cleaning_log) log_rows")
load_extras <- function(ds) {
  if (ds$type == "snapshot") return(ds$snap[names(EXTRA_SQL)])
  out <- lapply(EXTRA_SQL, function(sql) q(ds, sql))
  out$table_rows <- rbindlist(lapply(out$table_rows$tn, function(tn)
    data.table(table_name = tn, table_rows = q(ds, sprintf("SELECT COUNT(*) n FROM `%s`", tn))$n)))
  out
}

# ---- snapshot export (run once after building the database) -----------------
export_snapshot <- function(con, path = "shiny/data/dashboard_snapshot.rds") {
  ds <- list(type = "mysql", con = con)
  snap <- list(
    analysis = q(ds, "SELECT analysis_id, mgi_gene_id, gene_symbol, allele_symbol, life_stage_name, strain_name,
                             parameter_stable_id, parameter_name, procedure_name, p_value, p_value_flag, content_duplicate_of
                      FROM v_analysis_full"),
    parameter_group = q(ds, "SELECT * FROM parameter_group"),
    group_member = q(ds, "SELECT m.parameter_stable_id, pg.group_name FROM parameter_group_member m
                          JOIN parameter_group pg ON pg.group_id = m.group_id"),
    gene_disease = q(ds, "SELECT * FROM v_gene_disease WHERE mgi_gene_id IN (SELECT DISTINCT mgi_gene_id FROM analysis)"),
    gene_homology = q(ds, "SELECT * FROM v_gene_homology WHERE mgi_gene_id IN (SELECT DISTINCT mgi_gene_id FROM analysis)"))
  snap <- c(snap, load_extras(ds))
  u <- snap$analysis[is.na(content_duplicate_of) & !is.na(p_value) & p_value <= 1, .(analysis_id, q_value_bh = p.adjust(p_value, "BH"))]
  snap$analysis <- merge(snap$analysis, u, by = "analysis_id", all.x = TRUE)
  grp <- snap$group_member[, .(parameter_groups = paste(sort(group_name), collapse = "; ")), by = parameter_stable_id]
  snap$analysis <- merge(snap$analysis, grp, by = "parameter_stable_id", all.x = TRUE)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(snap, path, compress = "xz")
  invisible(path)
}
