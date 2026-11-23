#!/usr/bin/env Rscript
# =============================================================================
# Group 11 - build and populate the IMPC_Group11 MySQL database
# Requires: clean_data/clean_tables.rds (from R/01_clean_data.R) and a MySQL
# option file with credentials (default: db_config.cnf, section [client]).
# Usage:  Rscript R/02_build_database.R [path/to/option_file.cnf]
# =============================================================================
suppressPackageStartupMessages({ library(DBI); library(RMariaDB); library(data.table) })

args <- commandArgs(trailingOnly = TRUE)
CNF  <- if (length(args)) args[1] else "db_config.cnf"
msg  <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

con <- dbConnect(RMariaDB::MariaDB(), default.file = normalizePath(CNF), group = "impc", bigint = "integer")

msg("Connected to", dbGetQuery(con, "SELECT DATABASE() db, VERSION() v")$db)

run_sql_file <- function(path) {
  txt <- readLines(path, warn = FALSE)
  txt <- txt[!grepl("^\\s*--", txt)]
  stmts <- strsplit(paste(txt, collapse = "\n"), ";\\s*\n")[[1]]
  stmts <- trimws(stmts[nzchar(trimws(stmts))])
  for (s in stmts) dbExecute(con, s)
  msg("Executed", length(stmts), "statements from", path)
}

run_sql_file("sql/01_schema.sql")

tabs <- readRDS("clean_data/clean_tables.rds")
LOAD_ORDER <- c("gene", "allele", "strain", "life_stage", "procedure", "parameter",
                "parameter_version", "lacz_tissue", "lacz_call", "analysis", "analysis_lacz",
                "parameter_group", "parameter_group_member", "human_gene", "disease",
                "human_gene_disease", "mouse_human_ortholog", "disease_model_genotype",
                "species", "ensembl_gene", "gene_ensembl_xref", "homology_type", "taxon",
                "homology", "duplicate_source_file", "data_cleaning_log")
stopifnot(setequal(LOAD_ORDER, names(tabs)))

dbExecute(con, "SET unique_checks = 0")
for (t in LOAD_ORDER) {
  df <- as.data.frame(tabs[[t]])
  cols <- dbListFields(con, t)
  df <- df[, cols, drop = FALSE]                       # column order as in schema
  dbBegin(con)
  chunk <- 20000L
  for (i in seq(1, nrow(df), by = chunk))
    dbAppendTable(con, DBI::Id(table = t), df[i:min(nrow(df), i + chunk - 1), , drop = FALSE])
  dbCommit(con)
  msg(sprintf("Loaded %-24s %8d rows", t, nrow(df)))
}
dbExecute(con, "SET unique_checks = 1")

run_sql_file("sql/02_views.sql")

# ---- verification: database row counts must equal the cleaned tables ----
chk <- rbindlist(lapply(LOAD_ORDER, function(t) data.table(
  table = t, expected = nrow(tabs[[t]]),
  in_database = as.integer(dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM `%s`", t))$n))))
chk[, ok := expected == in_database]
print(chk)
fwrite(chk, "clean_data/database_row_count_check.tsv", sep = "\t")
stopifnot(all(chk$ok))
# referential sanity: every analysis resolves through the full view
n_view <- as.integer(dbGetQuery(con, "SELECT COUNT(*) n FROM v_analysis_full")$n)
stopifnot(n_view == nrow(tabs$analysis))
msg("All row counts verified; v_analysis_full rows:", n_view)
dbDisconnect(con)
