#!/usr/bin/env Rscript
# =============================================================================
# Group 11 - IMPC data cleaning pipeline
# -----------------------------------------------------------------------------
# Reads the raw IMPC analysis files (one key/value CSV per analysis), the IMPC
# parameter / procedure references, the Disease Ontology (MGI) report and the
# bespoke Ensembl homology export, cleans them and writes normalised,
# database-ready tables to clean_data/.
#
# Principles
#   * Raw files are never modified; every change to a value is written to an
#     audit log (clean_data/cleaning_log.tsv -> table data_cleaning_log).
#   * Controlled-vocabulary fields are repaired by matching a normalised key
#     (lower-case, alphanumerics only) against the SOP vocabulary.
#   * Identifier / symbol fields are repaired against an authoritative
#     reference (IMPC parameter list) or, when none exists, against the
#     consensus (most frequent) spelling within the same normalised-key group.
#   * Values that cannot be repaired with evidence are set to NULL and flagged,
#     never guessed.
#
# Usage:  Rscript R/01_clean_data.R            (run from the project root)
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

RAW_DIR   <- "group11_data"
OUT_DIR   <- "clean_data"
dir.create(OUT_DIR, showWarnings = FALSE)
if (!dir.exists(RAW_DIR) && file.exists("group11_data.zip")) unzip("group11_data.zip")   # raw files as egressed
set.seed(11)

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

# ---------------------------------------------------------------------------
# 0. Helpers
# ---------------------------------------------------------------------------
# characters that were randomly injected into values
NOISE_CHARS <- "[!@#$%^&*()_+.\\- ]"
norm_key  <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))
trim      <- function(x) gsub("^\\s+|\\s+$", "", x)
na_if_lit <- function(x) { x <- trim(x); x[x %in% c("", "NA", "N/A", "NULL", "na")] <- NA; x }

log_env <- new.env(); log_env$rows <- list()
log_change <- function(analysis_id, field, raw, clean, rule) {
  same <- (is.na(raw) & is.na(clean)) | (!is.na(raw) & !is.na(clean) & raw == clean)
  ch <- !same
  if (any(ch)) log_env$rows[[length(log_env$rows) + 1]] <- data.table(
    analysis_id = analysis_id[ch], field_name = field,
    raw_value = raw[ch], clean_value = clean[ch], rule = rule)
}

# match against a controlled vocabulary through a normalised key
vocab_clean <- function(x, vocab) {
  k <- norm_key(x); vk <- norm_key(vocab)
  out <- vocab[match(k, vk)]
  out[is.na(x)] <- NA
  out
}

# most frequent spelling within a grouping; ties broken by `prefer` regex
consensus <- function(value, group, prefer = NULL) {
  dt <- data.table(value, group)[!is.na(value) & !is.na(group), .N, by = .(group, value)]
  if (!is.null(prefer)) dt[, ok := grepl(prefer, value)] else dt[, ok := TRUE]
  setorder(dt, group, -ok, -N)
  dt[, .SD[1], by = group][, .(group, consensus = value)]
}

# ---------------------------------------------------------------------------
# 1. Import raw analysis files (robust, position-aware key/value parser)
# ---------------------------------------------------------------------------
FIELDS <- c("gene_accession_id", "gene_symbol", "allele_symbol", "strain_name",
            "parameter_stable_id", "p_value", "parameter_name", "mouse_life_stage",
            "lacz_blood", "lacz_brain", "lacz_colon", "lacz_eye", "lacz_heart",
            "lacz_kidney", "lacz_liver", "lacz_lung", "lacz_skeletal_muscle",
            "lacz_skin", "lacz_vascular_system", "analysis_id")
LACZ <- grep("^lacz_", FIELDS, value = TRUE)

files <- sort(list.files(RAW_DIR, pattern = "\\.csv$", full.names = TRUE))
msg("Importing", length(files), "raw analysis files")

parse_file <- function(f) {
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  rec <- setNames(rep(NA_character_, length(FIELDS)), FIELDS)
  issues <- character(0)
  for (i in seq_along(lines)) {
    ln <- lines[i]
    if (trim(ln) == "") {                     # blank line -> positional field missing
      if (i <= length(FIELDS)) issues <- c(issues, paste0("blank_line:", FIELDS[i]))
      next
    }
    key_raw <- sub(",.*$", "", ln)
    val_raw <- if (grepl(",", ln)) sub("^[^,]*,", "", ln) else NA_character_
    val <- if (is.na(val_raw)) NA_character_ else gsub('^"|"$', "", trim(val_raw))
    if (!is.na(val_raw) && trim(val_raw) == "NA") val <- "NA"   # unquoted NA
    keys <- strsplit(gsub('^"|"$', "", key_raw), '""', fixed = TRUE)[[1]]
    keys <- keys[keys != ""]
    if (length(keys) > 1) {                     # merged labels, e.g. "lacz_blood""lacz_brain"
      issues <- c(issues, paste0("merged_labels:", paste(keys, collapse = "+")))
      if (keys[length(keys)] %in% FIELDS) { rec[keys[length(keys)]] <- val; next }
      keys <- ""
    }
    key <- if (length(keys) == 1) keys else ""
    if (!key %in% FIELDS) {                     # malformed label -> use position
      if (i <= length(FIELDS)) {
        issues <- c(issues, paste0("label_repaired_by_position:", FIELDS[i]))
        key <- FIELDS[i]
      } else next
    }
    rec[key] <- val
  }
  c(rec, source_file = basename(f), import_issue = paste(issues, collapse = ";"))
}

raw <- rbindlist(lapply(files, function(f) as.list(parse_file(f))))
raw[import_issue == "", import_issue := NA]
msg("Parsed", nrow(raw), "records;", sum(!is.na(raw$import_issue)), "with structural issues")
fwrite(raw, file.path(OUT_DIR, "raw_records_parsed.tsv"), sep = "\t")

d <- copy(raw)
d[, file_analysis_id := sub("(_\\d+)?\\.csv$", "", source_file)]
d[, row_uid := sprintf("%s", source_file)]

# ---------------------------------------------------------------------------
# 2. Field-level cleaning
# ---------------------------------------------------------------------------
# 2.1 analysis_id : file name is authoritative (SOP: 'an' + 10 digits)
aid_raw <- d$analysis_id
d[, analysis_id := na_if_lit(analysis_id)]
d[is.na(analysis_id) | analysis_id != file_analysis_id, analysis_id_note :=
    fifelse(is.na(analysis_id), "missing_in_file_restored_from_filename",
            "mismatch_with_filename_restored_from_filename")]
d[, analysis_id := file_analysis_id]
log_change(d$analysis_id, "analysis_id", aid_raw, d$analysis_id, "restore_from_filename")

# 2.2 gene_accession_id
acc_raw <- d$gene_accession_id
d[, gene_accession_id := toupper(gsub("[^A-Za-z0-9:]", "", na_if_lit(gene_accession_id)))]
d[!grepl("^MGI:[0-9]+$", gene_accession_id), gene_accession_id := NA]
log_change(d$analysis_id, "gene_accession_id", acc_raw, d$gene_accession_id, "strip_noise_validate_MGI")

# 2.3 gene_symbol : evidence-ranked spelling per MGI accession (case + noise repair)
#     authority order: (1) MGI symbol in the Disease Ontology report when its
#     normalised key agrees, (2) the spelling that equals an allele-symbol prefix,
#     (3) the most frequent spelling with standard nomenclature form.
gs_raw <- d$gene_symbol
gs <- gsub("[!@#$%^&*()_+ ]", "", na_if_lit(gs_raw))     # '.' and '-' are legal in symbols
d[, gs_tmp := gs]
mgi_sym <- local({
  dl <- readLines("Disease_information.txt", warn = FALSE); dl <- dl[!grepl("^#", dl) & trim(dl) != ""]
  x <- fread(text = dl, sep = "\t", header = FALSE, colClasses = "character", quote = "", fill = TRUE)
  unique(x[, .(acc = trim(V8), sym = trim(V7))])[, .SD[1], by = acc]
})
al_prefix <- trim(sub("<.*$", "", gsub("[!@#$%^&*()_+ ]", "", na_if_lit(d$allele_symbol))))
cand <- d[!is.na(gs_tmp) & !is.na(gene_accession_id), .N, by = .(acc = gene_accession_id, v = gs_tmp)]
pref <- unique(data.table(acc = d$gene_accession_id, v = al_prefix)[!is.na(v)])[, is_prefix := TRUE]
cand <- merge(cand, pref, by = c("acc", "v"), all.x = TRUE)
cand[is.na(is_prefix), is_prefix := FALSE]
cand[, nomen := grepl("^[A-Z][a-z0-9]*([.-][A-Za-z0-9]+)*$|^[A-Z0-9]+[a-z0-9]*$", v)]
setorder(cand, acc, -is_prefix, -nomen, -N)
best <- cand[, .SD[1], by = acc]
best <- merge(best, mgi_sym, by = "acc", all.x = TRUE)
best[!is.na(sym) & norm_key(sym) == norm_key(v), v := sym]
cg <- best$v[match(d$gene_accession_id, best$acc)]
d[, gene_symbol := fifelse(!is.na(cg), cg, gs_tmp)][, gs_tmp := NULL]
# genes whose symbol is missing in every record: recover from the allele prefix
ap <- data.table(acc = d$gene_accession_id, v = al_prefix)[!is.na(v) & !is.na(acc), .N, by = .(acc, v)][order(acc, -N)][, .SD[1], by = acc]
need <- is.na(d$gene_symbol) & d$gene_accession_id %in% ap$acc
d[which(need), gene_symbol := ap$v[match(gene_accession_id, ap$acc)]]
msg("Gene symbols recovered from allele prefix:", sum(need), "records;", sum(is.na(d$gene_symbol)), "still missing")
log_change(d$analysis_id, "gene_symbol", gs_raw, d$gene_symbol, "evidence_ranked_symbol_per_MGI_accession")

# 2.4 allele_symbol : remove noise, then consensus within (accession, normalised key)
al_raw <- d$allele_symbol
al <- gsub("[!@#$%^&*_+ ]", "", na_if_lit(al_raw))
# allele prefix must be the gene symbol: repair when normalised keys agree
apfx <- sub("<.*$", "", al); fix <- !is.na(al) & grepl("<", al) & norm_key(apfx) == norm_key(d$gene_symbol) & apfx != d$gene_symbol
al[which(fix)] <- paste0(d$gene_symbol[which(fix)], substring(al[which(fix)], nchar(apfx[which(fix)]) + 1))
d[, al_tmp := al][, al_key := paste(gene_accession_id, norm_key(al_tmp))]
cons_al <- consensus(d$al_tmp, fifelse(is.na(d$al_tmp), NA_character_, d$al_key),
                     prefer = "^[A-Za-z0-9.-]+<[^<>]+>$")
ca <- cons_al$consensus[match(d$al_key, cons_al$group)]
d[, allele_symbol := fifelse(!is.na(ca) & !is.na(al_tmp), ca, al_tmp)]
d[, c("al_tmp", "al_key") := NULL]
# validation: brackets balanced and allele prefix equals gene symbol
d[, allele_valid := !is.na(allele_symbol) &
     grepl("^[^<>]+<[^<>]+>$", allele_symbol) &
     sub("<.*$", "", allele_symbol) == gene_symbol]
# where the allele is missing but the gene has exactly one observed allele, impute it
one_allele <- d[!is.na(allele_symbol), .(n = uniqueN(allele_symbol), a = allele_symbol[1]), by = gene_accession_id][n == 1]
d[, a := one_allele$a[match(gene_accession_id, one_allele$gene_accession_id)]]
d[is.na(allele_symbol) & !is.na(a), `:=`(allele_symbol = a, allele_note = "imputed_single_allele_for_gene")]
d[, a := NULL]
log_change(d$analysis_id, "allele_symbol", al_raw, d$allele_symbol,
           "noise_strip_consensus_or_single_allele_imputation")

# 2.5 strain / life stage / lacZ : controlled vocabularies from the SOP
STRAINS <- c("B6Brd;B6Dnk;B6N-Tyr<c-Brd>", "C57BL/6Brd-Tyr<c-Brd>", "C57BL/6Dnk",
             "C57BL/6JTyr;C57BL/6N", "C57BL/6N", "C57BL/6NCrl", "C57BL/6NJ",
             "C57BL/6NTac", "C57BL/6NTacDen")
STAGES  <- c("E9.5", "E12.5", "E15.5", "E18.5", "Early adult", "Middle aged adult", "Late adult")
LACZ_V  <- c("No data", "ambiguous", "expression", "imageOnly", "no expression", "tissue not available")

for (f in c("strain_name", "mouse_life_stage", LACZ)) {
  vocab <- switch(f, strain_name = STRAINS, mouse_life_stage = STAGES, LACZ_V)
  r <- d[[f]]
  v <- vocab_clean(na_if_lit(r), vocab)
  set(d, j = f, value = v)
  log_change(d$analysis_id, f, r, v, "controlled_vocabulary_match")
}

# 2.6 p_value
pv_raw <- d$p_value
d[, p_value := suppressWarnings(as.numeric(na_if_lit(p_value)))]
d[, p_value_flag := fcase(is.na(p_value), "missing",
                          p_value < 0 | p_value > 1, "out_of_range_[0,1]",
                          default = NA_character_)]
log_change(d$analysis_id, "p_value", pv_raw, as.character(d$p_value), "trim_to_numeric")

# ---------------------------------------------------------------------------
# 3. Reference data: parameters and procedures
# ---------------------------------------------------------------------------
pdesc <- fread("IMPC_parameter_description.txt", colClasses = "character", strip.white = TRUE)
proc  <- fread("IMPC_procedure.txt",             colClasses = "character", strip.white = TRUE)
pdesc[, names(pdesc) := lapply(.SD, trim)]
proc[,  names(proc)  := lapply(.SD, trim)]
n0 <- nrow(pdesc); pdesc <- unique(pdesc); msg("Parameter reference: removed", n0 - nrow(pdesc), "exact duplicates")
stopifnot(!anyDuplicated(pdesc$impcParameterOrigId), !anyDuplicated(proc$impcParameterOrigId))

proc_ref <- merge(pdesc, proc, by = "impcParameterOrigId", suffixes = c("_param", "_proc"))
stopifnot(nrow(proc_ref) == nrow(pdesc))
# procedure table: one row per procedure name; keep the most informative description
proc_ref[description_proc %in% c("Housing_and_husbandry", "experimental_design"), description_proc := ""]
procedure <- proc_ref[, .(description = { x <- unique(description_proc); x <- x[x != ""]; if (length(x)) x[1] else NA_character_ },
                          procedure_code = paste(unique(sub("^IMPC_([A-Z]+)_.*$", "\\1", parameterId)), collapse = ",")),
                      by = .(procedure_name = name_proc)]
setorder(procedure, procedure_name)
procedure[, procedure_id := .I]

ref_params <- proc_ref[, .(parameter_name_ref = name_param[1],
                           parameter_description = description_param[1],
                           procedure_name = name_proc[1],
                           is_mandatory = as.integer(toupper(isMandatory[1]) == "TRUE")),
                       by = .(parameter_stable_id = parameterId)]
# individual IMPC parameter versions (orig ids), kept for completeness
param_version <- proc_ref[, .(impc_parameter_orig_id = as.integer(impcParameterOrigId),
                              parameter_stable_id = parameterId,
                              name = name_param, description = description_param,
                              is_mandatory = as.integer(toupper(isMandatory) == "TRUE"))]

# 3.1 parameter_stable_id repair
pid_raw <- d$parameter_stable_id
pid <- na_if_lit(pid_raw)
ref_key <- setNames(ref_params$parameter_stable_id, norm_key(ref_params$parameter_stable_id))
k <- norm_key(pid)
pid_ref <- unname(ref_key[k])
# non-reference IDs (centre-specific legacy procedures): consensus within key group
d[, pid_tmp := toupper(pid)][, pid_k := k]
cons_pid <- consensus(d$pid_tmp[is.na(pid_ref)], d$pid_k[is.na(pid_ref)],
                      prefer = "^[A-Z]+(-[A-Z])*(-[A-Z]+)?_[A-Z0-9]+_[0-9]{3}_[0-9]{3}$")
d[, pid_clean := fifelse(!is.na(pid_ref), pid_ref,
                         cons_pid$consensus[match(pid_k, cons_pid$group)])]
d[is.na(pid), pid_clean := NA]
d[, parameter_id_source := fifelse(!is.na(pid_ref), "IMPC_reference",
                                   fifelse(!is.na(pid_clean), "consensus_spelling", NA_character_))]

# 3.2 parameter_name repair (trim, collapse spaces) and recovery of missing IDs
pn_raw <- d$parameter_name
d[, parameter_name := gsub("\\s+", " ", na_if_lit(parameter_name))]
name2id <- ref_params[, .(ids = list(unique(parameter_stable_id))), by = .(nk = norm_key(parameter_name_ref))]
name2id <- name2id[lengths(ids) == 1, .(nk, id = unlist(ids))]
miss <- is.na(d$pid_clean) & !is.na(d$parameter_name)
rec  <- name2id$id[match(norm_key(d$parameter_name[miss]), name2id$nk)]
d[which(miss)[!is.na(rec)], `:=`(pid_clean = rec[!is.na(rec)], parameter_id_source = "recovered_from_unique_parameter_name")]
d[, parameter_stable_id := pid_clean][, c("pid_tmp", "pid_k", "pid_clean") := NULL]
log_change(d$analysis_id, "parameter_stable_id", pid_raw, d$parameter_stable_id,
           "reference_key_match_or_consensus_or_unique_name_recovery")

# canonical parameter name: reference name where available, else consensus experimental name
cons_pn <- consensus(d$parameter_name, d$parameter_stable_id)
pn_canon <- merge(cons_pn, ref_params[, .(parameter_stable_id, parameter_name_ref)],
                  by.x = "group", by.y = "parameter_stable_id", all.x = TRUE)
pn_canon[, canonical := fifelse(!is.na(parameter_name_ref), parameter_name_ref, consensus)]
d[, parameter_name_clean := pn_canon$canonical[match(parameter_stable_id, pn_canon$group)]]
d[is.na(parameter_name_clean), parameter_name_clean := parameter_name]
log_change(d$analysis_id, "parameter_name", pn_raw, d$parameter_name_clean, "canonical_parameter_name")

# ---------------------------------------------------------------------------
# 4. Duplicate analyses (files suffixed _1)
# ---------------------------------------------------------------------------
cmp_cols <- c("gene_accession_id", "gene_symbol", "allele_symbol", "strain_name",
              "parameter_stable_id", "p_value", "mouse_life_stage", LACZ)
d[, n_changes := 0L]
lg <- rbindlist(log_env$rows)
chg <- lg[, .N, by = analysis_id]
dups <- d[analysis_id %in% d[duplicated(analysis_id), analysis_id]]
dup_report <- dups[, .(files = paste(source_file, collapse = ";"),
                       identical_after_cleaning = uniqueN(.SD) == 1),
                   by = analysis_id, .SDcols = cmp_cols]
msg("Duplicate analysis ids:", nrow(dup_report), "; identical after cleaning:",
    sum(dup_report$identical_after_cleaning))
fwrite(dup_report, file.path(OUT_DIR, "duplicate_analysis_report.tsv"), sep = "\t")
# keep the original (un-suffixed) file; fill its missing values from the duplicate
d[, is_duplicate_file := grepl("_\\d+\\.csv$", source_file)]
conflicts <- list()
for (a in dup_report[identical_after_cleaning == FALSE, analysis_id]) {
  io <- which(d$analysis_id == a & !d$is_duplicate_file); id <- which(d$analysis_id == a & d$is_duplicate_file)
  for (cc in cmp_cols) {
    vo <- d[[cc]][io]; vd <- d[[cc]][id[1]]
    if (is.na(vo) && !is.na(vd)) {
      set(d, io, cc, vd)
      log_env$rows[[length(log_env$rows) + 1]] <- data.table(analysis_id = a, field_name = cc, raw_value = NA_character_,
                                                             clean_value = as.character(vd), rule = "filled_from_duplicate_source_file")
    } else if (!is.na(vo) && !is.na(vd) && vo != vd) conflicts[[length(conflicts) + 1]] <- data.table(analysis_id = a, field = cc, original = as.character(vo), duplicate = as.character(vd))
  }
}
msg("Duplicate pairs merged; value conflicts:", length(conflicts))
if (length(conflicts)) fwrite(rbindlist(conflicts), file.path(OUT_DIR, "duplicate_value_conflicts.tsv"), sep = "\t")
dup_files <- d[is_duplicate_file == TRUE, .(source_file, analysis_id)]
dup_files <- merge(dup_files, dup_report[, .(analysis_id, identical_after_cleaning)], by = "analysis_id")
d <- d[is_duplicate_file == FALSE]
stopifnot(!anyDuplicated(d$analysis_id))

# ---------------------------------------------------------------------------
# 5. Build normalised entity tables
# ---------------------------------------------------------------------------
# 5.1 Disease Ontology report (MGI)
# header comment lines start with '#'
dl <- readLines("Disease_information.txt", warn = FALSE)
dl <- dl[!grepl("^#", dl) & trim(dl) != ""]
dis_all <- fread(text = dl, sep = "\t", header = FALSE, colClasses = "character", quote = "", fill = TRUE)
setnames(dis_all, c("human_symbol", "human_name", "hgnc_id", "do_name", "doid",
                    "mouse_genotype_ids", "mouse_symbol", "mgi_gene_id"))
dis <- dis_all[, lapply(.SD, trim)]
n0 <- nrow(dis); dis <- unique(dis); msg("Disease file:", n0, "rows,", n0 - nrow(dis), "exact duplicates removed")
stopifnot(all(grepl("^DOID:[0-9]+$", dis$doid)), all(grepl("^HGNC:[0-9]+$", dis$hgnc_id)),
          all(grepl("^MGI:[0-9]+$", dis$mgi_gene_id)))

human_gene <- unique(dis[, .(hgnc_id, human_symbol, human_name)])
stopifnot(!anyDuplicated(human_gene$hgnc_id))
disease <- dis[, .(disease_name = do_name[1]), by = doid]
human_gene_disease <- unique(dis[, .(hgnc_id, doid)])
mouse_human_ortholog <- unique(dis[, .(mgi_gene_id, hgnc_id)])
disease_model_genotype <- dis[mouse_genotype_ids != "",
                              .(mgi_genotype_id = unlist(strsplit(mouse_genotype_ids, "|", fixed = TRUE))),
                              by = .(hgnc_id, doid)]
disease_model_genotype <- unique(disease_model_genotype[, mgi_genotype_id := trim(mgi_genotype_id)])

# 5.2 Mouse genes: IMPC genes plus disease-report genes
g_impc <- unique(d[!is.na(gene_accession_id), .(mgi_gene_id = gene_accession_id, gene_symbol)])
stopifnot(!anyDuplicated(g_impc$mgi_gene_id))
g_dis  <- unique(dis[, .(mgi_gene_id, gene_symbol = mouse_symbol)])
gene <- rbind(g_impc[, .(mgi_gene_id, gene_symbol, in_impc_data = 1L)],
              g_dis[!mgi_gene_id %in% g_impc$mgi_gene_id, .(mgi_gene_id, gene_symbol, in_impc_data = 0L)])
gene <- gene[, .SD[1], by = mgi_gene_id]
# cross-check: IMPC symbol vs MGI symbol in the disease report
sym_check <- merge(g_impc, g_dis, by = "mgi_gene_id", suffixes = c("_impc", "_mgi"))
msg("Gene symbols cross-checked with MGI disease report:", nrow(sym_check),
    "; disagreements:", sum(sym_check$gene_symbol_impc != sym_check$gene_symbol_mgi))
fwrite(sym_check[gene_symbol_impc != gene_symbol_mgi], file.path(OUT_DIR, "gene_symbol_disagreements.tsv"), sep = "\t")

allele <- unique(d[!is.na(allele_symbol), .(allele_symbol, mgi_gene_id = gene_accession_id, allele_valid)])
allele <- allele[, .(allele_valid = as.integer(any(allele_valid))), by = .(allele_symbol, mgi_gene_id)]
setorder(allele, allele_symbol); allele[, allele_id := .I]

strain     <- data.table(strain_id = seq_along(STRAINS), strain_name = STRAINS)
life_stage <- data.table(life_stage_id = seq_along(STAGES), life_stage_name = STAGES,
                         is_embryonic = as.integer(grepl("^E", STAGES)))
lacz_tissue <- data.table(tissue_id = seq_along(LACZ), tissue_name = sub("^lacz_", "", LACZ))
lacz_call   <- data.table(call_id = seq_along(LACZ_V), call_label = LACZ_V)

# 5.3 Parameters: every ID used in the experiments, enriched with the reference
exp_ids <- unique(d[!is.na(parameter_stable_id), .(parameter_stable_id)])
parameter <- merge(exp_ids, ref_params, by = "parameter_stable_id", all.x = TRUE)
parameter <- rbind(parameter, ref_params[!parameter_stable_id %in% exp_ids$parameter_stable_id], fill = TRUE)
parameter[, parameter_name := pn_canon$canonical[match(parameter_stable_id, pn_canon$group)]]
parameter[is.na(parameter_name), parameter_name := parameter_name_ref]
parameter[, in_impc_reference := as.integer(!is.na(procedure_name))]
# infer procedure for centre-specific IDs from the procedure code (e.g. JAX_OFD_* -> Open Field)
code2proc <- procedure[!grepl(",", procedure_code), .(procedure_code, procedure_name)]
parameter[, code := fifelse(grepl("^[A-Z-]+_[A-Z]{3}_[0-9]{3}_[0-9]{3}$", parameter_stable_id),
                            sub("^[A-Z-]+_([A-Z]{3})_.*$", "\\1", parameter_stable_id), NA_character_)]
parameter[, procedure_link := fifelse(in_impc_reference == 1, "IMPC_reference", NA_character_)]
parameter[is.na(procedure_name) & code %in% code2proc$procedure_code,
          `:=`(procedure_name = code2proc$procedure_name[match(code, code2proc$procedure_code)],
               procedure_link = "inferred_from_procedure_code")]
parameter[, procedure_id := procedure$procedure_id[match(procedure_name, procedure$procedure_name)]]
parameter <- parameter[, .(parameter_stable_id, parameter_name, parameter_description,
                           procedure_id, procedure_link, is_mandatory, in_impc_reference)]
stopifnot(!anyDuplicated(parameter$parameter_stable_id))

# 5.4 Parameter groupings (many-to-many). Rules use the parameter name,
#     description and procedure; documented in parameter_group.rule_description.
pg_src <- merge(parameter, procedure[, .(procedure_id, procedure_name)], by = "procedure_id", all.x = TRUE)
pg_src[, txt := tolower(paste(parameter_name, parameter_description, sep = " | "))]
pg_src[, pr  := tolower(fifelse(is.na(procedure_name), "", procedure_name))]
GROUPS <- list(
  list("Weight", "Body, organ and tissue weights",
       "name/description contains 'weight' or 'mass'; procedures Body Weight, Heart Weight, Organ Weight",
       quote(grepl("weight|\\bmass\\b", txt) | pr %in% c("body weight", "heart weight", "organ weight"))),
  list("Images", "Parameters that are images or derived from imaging",
       "name/description contains 'image'/'imaging'/'x-ray'/'microct'/'opt'; procedures X-ray, MicroCT*, OPT*",
       quote(grepl("imag|x-ray|xray|micro ?ct|\\bopt\\b", txt) | grepl("^x-ray|^microct|^opt", pr))),
  list("Brain", "Brain and central nervous system",
       "name/description contains brain, cerebr*, cerebell*, hippocamp*, cortex, ventricle(s) of brain, neur*, spinal cord; procedure Auditory Brain Stem Response",
       quote(grepl("brain|cerebr|cerebell|hippocamp|cortex|neur|spinal cord|hypothalam|thalam|pituitar|medulla", txt) |
               pr == "auditory brain stem response")),
  list("Blood & clinical chemistry", "Haematology, clinical chemistry and circulating analytes",
       "procedures Hematology, Clinical Chemistry, Insulin Blood Level; or name contains blood/plasma/serum",
       quote(pr %in% c("hematology", "clinical chemistry", "insulin blood level") | grepl("blood|plasma|serum", txt))),
  list("Cardiovascular", "Heart and vascular function / morphology",
       "procedures Echo, Electrocardiogram (ECG), Heart Weight; or name contains heart/cardi*/aort*/ventric*/atri*/vascular",
       quote(pr %in% c("echo", "electrocardiogram (ecg)", "heart weight") |
               grepl("heart|cardi|aort|vascular|\\bqrs\\b|\\bqt|\\bpr interval|ejection|stroke volume", txt))),
  list("Eye & vision", "Eye morphology and vision",
       "procedure Eye Morphology; or name contains eye/retina/lens/cornea/iris/pupil/optic",
       quote(pr == "eye morphology" | grepl("\\beye|retina|\\blens\\b|cornea|\\biris\\b|pupil|optic|vitreous|conjunctiv", txt))),
  list("Skeleton & bone", "Skeletal morphology and bone density",
       "procedure X-ray; or name contains bone/skull/femur/tibia/vertebra/rib/digit/BMD/skeletal",
       quote(pr == "x-ray" | grepl("bone|skull|femur|tibia|vertebra|\\bribs?\\b|digit|\\bbmd\\b|skelet|pelvis|scapula|clavicle|mandible|teeth|tooth", txt))),
  list("Behaviour & neurological", "Behavioural and neurological screens",
       "procedures Open Field, Light-Dark Test, Fear Conditioning, Y-maze, Acoustic Startle/PPI, Grip Strength, Combined SHIRPA, Formalin Test",
       quote(pr %in% c("open field", "light-dark test", "fear conditioning", "y-maze",
                       "acoustic startle and pre-pulse inhibition (ppi)", "grip strength",
                       "combined shirpa and dysmorphology", "formalin test") |
               grepl("startle|locomot|freezing|grip|anxiety|rotarod|behavio", txt))),
  list("Metabolism", "Energy balance, glucose handling and body composition",
       "procedures Indirect Calorimetry, IPGTT, Insulin Blood Level, Body Composition (DEXA); or name contains glucose/insulin/fat/lean/oxygen consumption/respiratory exchange",
       quote(pr %in% c("indirect calorimetry", "intraperitoneal glucose tolerance test (ipgtt)",
                       "insulin blood level", "body composition (dexa lean/fat)") |
               grepl("glucose|insulin|\\bfat\\b|lean|oxygen consumption|respiratory exchange|energy expenditure|triglycer|cholesterol", txt))),
  list("Immune system", "Immunophenotyping and immune cell populations",
       "procedure Immunophenotyping; or name contains T cell/B cell/NK/CD4/CD8/CD45/lymph*/monocyte/neutrophil/macrophage/granulocyte",
       quote(pr == "immunophenotyping" | grepl("t cell|b cell|\\bnk\\b|cd4|cd8|cd45|cd19|lymph|monocyte|neutrophil|macrophage|granulocyte|eosinophil|basophil|dendritic|immun", txt))),
  list("Embryo & development", "Embryonic / placental morphology and viability",
       "procedures Gross Morphology Embryo/Placenta*, Viability*, Embryo LacZ, MicroCT/OPT of embryos",
       quote(grepl("embryo|placenta|viability|microct|^opt", pr) | grepl("embryo|placent|yolk sac|somite", txt)))
)
parameter_group <- rbindlist(lapply(seq_along(GROUPS), function(i)
  data.table(group_id = i, group_name = GROUPS[[i]][[1]], group_description = GROUPS[[i]][[2]],
             rule_description = GROUPS[[i]][[3]])))
parameter_group_member <- rbindlist(lapply(seq_along(GROUPS), function(i)
  pg_src[eval(GROUPS[[i]][[4]]), .(group_id = i, parameter_stable_id)]))
msg("Parameter group sizes:"); print(parameter_group_member[, .N, by = group_id][parameter_group, on = "group_id"][, .(group_name, N)])

# 5.5 Analysis fact table + lacZ (long format)
analysis <- d[, .(analysis_id, mgi_gene_id = gene_accession_id,
                  allele_id = allele$allele_id[match(paste(allele_symbol, gene_accession_id),
                                                     paste(allele$allele_symbol, allele$mgi_gene_id))],
                  strain_id = strain$strain_id[match(strain_name, strain$strain_name)],
                  life_stage_id = life_stage$life_stage_id[match(mouse_life_stage, life_stage$life_stage_name)],
                  parameter_stable_id, p_value, p_value_flag, parameter_id_source,
                  source_file, import_issue,
                  analysis_id_note)]
# analyses with identical content (gene, allele, strain, stage, parameter, p-value)
# but different ids are retained; later ones point to the first of their group
analysis[, dup_key := fifelse(is.na(p_value) | is.na(parameter_stable_id), NA_character_,
                              paste(mgi_gene_id, allele_id, strain_id, life_stage_id, parameter_stable_id, p_value))]
setorder(analysis, analysis_id)
analysis[!is.na(dup_key), first_id := analysis_id[1], by = dup_key]
analysis[, content_duplicate_of := fifelse(!is.na(first_id) & first_id != analysis_id, first_id, NA_character_)]
analysis[, c("dup_key", "first_id") := NULL]
msg("Content-identical repeat analyses flagged:", sum(!is.na(analysis$content_duplicate_of)))
lacz <- melt(d[, c("analysis_id", LACZ), with = FALSE], id.vars = "analysis_id",
             variable.name = "tissue", value.name = "call", variable.factor = FALSE)
lacz <- lacz[!is.na(call), .(analysis_id,
                             tissue_id = lacz_tissue$tissue_id[match(sub("^lacz_", "", tissue), lacz_tissue$tissue_name)],
                             call_id = lacz_call$call_id[match(call, lacz_call$call_label)])]

# 5.6 Bespoke task: Ensembl homology (wide BioMart export -> long, species-generic)
hml <- fread("bespokeTask_hml.txt", sep = "\t", colClasses = "character", quote = "", na.strings = NULL)
hml[, names(hml) := lapply(.SD, trim)]
n_hml_rows <- nrow(hml)
setnames(hml, 1, "mouse_ensembl_gene_id")
setnames(hml, "Gene name", "mouse_gene_name")
species_cols <- function(sp) {
  cols <- grep(paste0("^", sp, " gene stable ID$|^", sp, " gene name$|^Last common ancestor with ", sp,
                      "$|^", sp, " homology type$|^%id. target ", sp), names(hml), value = TRUE)
  stopifnot(length(cols) == 5)
  x <- hml[, c("mouse_ensembl_gene_id", cols), with = FALSE]
  setnames(x, c("mouse_ensembl_gene_id", "homolog_ensembl_gene_id", "homolog_gene_name",
                "last_common_ancestor", "homology_type", "pct_identity"))
  x[, species := sp]
  x[homolog_ensembl_gene_id != ""]
}
SPECIES <- sub(" gene stable ID$", "", grep(" gene stable ID$", names(hml), value = TRUE))
SPECIES <- setdiff(SPECIES, "Gene")
hom_long <- rbindlist(lapply(SPECIES, species_cols))
n_long <- nrow(hom_long)
hom_long <- unique(hom_long)       # wide export is a cartesian product of species -> duplicates
msg("Homology: wide rows", n_hml_rows, "| long rows", n_long, "| unique pairs", nrow(hom_long))
stopifnot(!anyDuplicated(hom_long[, .(mouse_ensembl_gene_id, homolog_ensembl_gene_id)]))
hom_long[, pct_identity := as.numeric(pct_identity)]

sp_tbl <- data.table(species_id = seq_len(length(SPECIES) + 1),
                      species_name = c("Mouse", SPECIES),
                      ensembl_gene_prefix = c("ENSMUSG", sapply(SPECIES, function(s)
                        sub("[0-9]+$", "", hom_long[species == s, homolog_ensembl_gene_id][1]))))
mouse_ens <- unique(hml[mouse_ensembl_gene_id %in% hom_long$mouse_ensembl_gene_id,
                        .(ensembl_gene_id = mouse_ensembl_gene_id, gene_name = mouse_gene_name)])
mouse_ens <- mouse_ens[, .(gene_name = { x <- unique(gene_name[gene_name != ""]); if (length(x)) x[1] else NA_character_ }),
                       by = ensembl_gene_id]
homolog_ens <- unique(hom_long[, .(ensembl_gene_id = homolog_ensembl_gene_id, species)])
homolog_ens <- merge(homolog_ens,
                     hom_long[, .(gene_name = { x <- unique(homolog_gene_name[homolog_gene_name != ""]); if (length(x)) x[1] else NA_character_ }),
                              by = .(ensembl_gene_id = homolog_ensembl_gene_id)], by = "ensembl_gene_id")
ensembl_gene <- rbind(mouse_ens[, .(ensembl_gene_id, species_id = 1L, gene_name)],
                      homolog_ens[, .(ensembl_gene_id, species_id = sp_tbl$species_id[match(species, sp_tbl$species_name)], gene_name)])
stopifnot(!anyDuplicated(ensembl_gene$ensembl_gene_id))
# link mouse Ensembl genes to MGI genes through the official symbol (exact, case-sensitive)
gene_ensembl_xref <- merge(mouse_ens[!is.na(gene_name)], gene[, .(mgi_gene_id, gene_symbol)],
                           by.x = "gene_name", by.y = "gene_symbol")[, .(mgi_gene_id, ensembl_gene_id, match_method = "exact_symbol")]
ht_tbl <- data.table(homology_type_id = seq_along(unique(hom_long$homology_type)),
                            homology_type = sort(unique(hom_long$homology_type)))
tx_tbl <- data.table(taxon_id = seq_along(unique(hom_long$last_common_ancestor)),
                    taxon_name = sort(unique(hom_long$last_common_ancestor)))
homology <- hom_long[, .(mouse_ensembl_gene_id, homolog_ensembl_gene_id,
                         homology_type_id = ht_tbl$homology_type_id[match(homology_type, ht_tbl$homology_type)],
                         lca_taxon_id = tx_tbl$taxon_id[match(last_common_ancestor, tx_tbl$taxon_name)],
                         pct_identity)]
msg("Bespoke homology: mouse genes with >=1 homolog:", nrow(mouse_ens),
    "| linked to MGI genes:", nrow(gene_ensembl_xref),
    "| mouse genes without any homolog excluded:", uniqueN(hml$mouse_ensembl_gene_id) - nrow(mouse_ens))

# ---------------------------------------------------------------------------
# 6. Write outputs
# ---------------------------------------------------------------------------
cleaning_log <- rbindlist(log_env$rows)
cleaning_log <- cleaning_log[analysis_id %in% analysis$analysis_id | TRUE]
cleaning_log[, log_id := .I]
dup_files[, note := "duplicate source file of an existing analysis; excluded from analysis table"]

out <- list(procedure = procedure[, .(procedure_id, procedure_name, procedure_code, description)],
            parameter = parameter, parameter_version = param_version,
            parameter_group = parameter_group, parameter_group_member = parameter_group_member,
            gene = gene, allele = allele[, .(allele_id, allele_symbol, mgi_gene_id, allele_valid)],
            strain = strain, life_stage = life_stage,
            lacz_tissue = lacz_tissue, lacz_call = lacz_call,
            analysis = analysis, analysis_lacz = lacz,
            duplicate_source_file = dup_files,
            human_gene = human_gene, disease = disease, human_gene_disease = human_gene_disease,
            mouse_human_ortholog = mouse_human_ortholog, disease_model_genotype = disease_model_genotype,
            species = sp_tbl, ensembl_gene = ensembl_gene, gene_ensembl_xref = gene_ensembl_xref,
            homology_type = ht_tbl, taxon = tx_tbl, homology = homology,
            data_cleaning_log = cleaning_log[, .(log_id, analysis_id, field_name, raw_value, clean_value, rule)])
for (n in names(out)) fwrite(out[[n]], file.path(OUT_DIR, paste0(n, ".tsv")), sep = "\t", na = "\\N", quote = FALSE)
saveRDS(out, file.path(OUT_DIR, "clean_tables.rds"))

# summary report
summ <- data.table(table = names(out), rows = sapply(out, nrow))
fwrite(summ, file.path(OUT_DIR, "table_row_counts.tsv"), sep = "\t")
print(summ)
fwrite(cleaning_log[, .N, by = .(field_name, rule)][order(-N)], file.path(OUT_DIR, "cleaning_summary.tsv"), sep = "\t")
print(cleaning_log[, .N, by = .(field_name)][order(-N)])
msg("Remaining missing values in analysis table:")
print(analysis[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = c("mgi_gene_id", "allele_id", "strain_id", "life_stage_id", "parameter_stable_id", "p_value")])
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "R_sessionInfo.txt"))
msg("Done.")
