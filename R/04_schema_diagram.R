#!/usr/bin/env Rscript
# Draws the IMPC_Group11 entity-relationship diagram (docs/ER_diagram.png/.svg)
suppressPackageStartupMessages({ library(DiagrammeR); library(DiagrammeRsvg); library(rsvg) })

col_port <- function(x) gsub("[^A-Za-z0-9_]", "", sub("^(PK|FK|PK,FK) ", "", x))
tbl <- function(name, cols, colour) {
  rows <- paste0('<tr><td bgcolor="#ffffff" align="left" port="', col_port(cols), '">', cols, "</td></tr>", collapse = "")
  sprintf('%s [label=<<table border="0" cellborder="1" cellspacing="0" cellpadding="5"><tr><td bgcolor="%s"><b>%s</b></td></tr>%s</table>>];',
          name, colour, name, rows)
}
A <- "#D6EAF8"; B <- "#FCF3CF"; C <- "#FADBD8"; D <- "#D5F5E3"; E <- "#EAEDED"
nodes <- c(
  tbl("analysis", c("PK analysis_id", "FK mgi_gene_id", "FK allele_id", "FK strain_id", "FK life_stage_id",
                    "FK parameter_stable_id", "p_value", "p_value_flag", "parameter_id_source", "source_file",
                    "import_issue", "FK content_duplicate_of"), A),
  tbl("gene", c("PK mgi_gene_id", "gene_symbol", "in_impc_data"), A),
  tbl("allele", c("PK allele_id", "allele_symbol", "FK mgi_gene_id", "allele_valid"), A),
  tbl("strain", c("PK strain_id", "strain_name"), A),
  tbl("life_stage", c("PK life_stage_id", "life_stage_name", "is_embryonic"), A),
  tbl("parameter", c("PK parameter_stable_id", "parameter_name", "parameter_description", "FK procedure_id",
                     "procedure_link", "is_mandatory", "in_impc_reference"), A),
  tbl("parameter_version", c("PK impc_parameter_orig_id", "FK parameter_stable_id", "name", "description", "is_mandatory"), A),
  tbl("procedure", c("PK procedure_id", "procedure_name", "procedure_code", "description"), A),
  tbl("analysis_lacz", c("PK,FK analysis_id", "PK,FK tissue_id", "FK call_id"), A),
  tbl("lacz_tissue", c("PK tissue_id", "tissue_name"), A),
  tbl("lacz_call", c("PK call_id", "call_label"), A),
  tbl("parameter_group", c("PK group_id", "group_name", "group_description", "rule_description"), B),
  tbl("parameter_group_member", c("PK,FK group_id", "PK,FK parameter_stable_id"), B),
  tbl("human_gene", c("PK hgnc_id", "human_symbol", "human_name"), C),
  tbl("disease", c("PK doid", "disease_name"), C),
  tbl("human_gene_disease", c("PK,FK hgnc_id", "PK,FK doid"), C),
  tbl("mouse_human_ortholog", c("PK,FK mgi_gene_id", "PK,FK hgnc_id"), C),
  tbl("disease_model_genotype", c("PK,FK hgnc_id", "PK,FK doid", "PK mgi_genotype_id"), C),
  tbl("species", c("PK species_id", "species_name", "ensembl_gene_prefix"), D),
  tbl("ensembl_gene", c("PK ensembl_gene_id", "FK species_id", "gene_name"), D),
  tbl("gene_ensembl_xref", c("PK,FK mgi_gene_id", "PK,FK ensembl_gene_id", "match_method"), D),
  tbl("homology", c("PK,FK mouse_ensembl_gene_id", "PK,FK homolog_ensembl_gene_id", "FK homology_type_id",
                    "FK lca_taxon_id", "pct_identity"), D),
  tbl("homology_type", c("PK homology_type_id", "homology_type"), D),
  tbl("taxon", c("PK taxon_id", "taxon_name"), D),
  tbl("duplicate_source_file", c("PK source_file", "FK analysis_id", "identical_after_cleaning", "note"), E),
  tbl("data_cleaning_log", c("PK log_id", "analysis_id", "field_name", "raw_value", "clean_value", "rule"), E))
# edges: child FK column -> parent PK column ("many" end at the child)
edges <- c("analysis:mgi_gene_id -> gene:mgi_gene_id", "analysis:allele_id -> allele:allele_id",
           "analysis:strain_id -> strain:strain_id", "analysis:life_stage_id -> life_stage:life_stage_id",
           "analysis:parameter_stable_id -> parameter:parameter_stable_id",
           "analysis:content_duplicate_of -> analysis:analysis_id", "allele:mgi_gene_id -> gene:mgi_gene_id",
           "parameter:procedure_id -> procedure:procedure_id",
           "parameter_version:parameter_stable_id -> parameter:parameter_stable_id",
           "analysis_lacz:analysis_id -> analysis:analysis_id",
           "analysis_lacz:tissue_id -> lacz_tissue:tissue_id",
           "analysis_lacz:call_id -> lacz_call:call_id",
           "parameter_group_member:group_id -> parameter_group:group_id",
           "parameter_group_member:parameter_stable_id -> parameter:parameter_stable_id",
           "human_gene_disease:hgnc_id -> human_gene:hgnc_id",
           "human_gene_disease:doid -> disease:doid",
           "mouse_human_ortholog:mgi_gene_id -> gene:mgi_gene_id",
           "mouse_human_ortholog:hgnc_id -> human_gene:hgnc_id",
           "disease_model_genotype:doid -> human_gene_disease:doid",
           "ensembl_gene:species_id -> species:species_id",
           "gene_ensembl_xref:mgi_gene_id -> gene:mgi_gene_id",
           "gene_ensembl_xref:ensembl_gene_id -> ensembl_gene:ensembl_gene_id",
           "homology:mouse_ensembl_gene_id -> ensembl_gene:ensembl_gene_id",
           "homology:homology_type_id -> homology_type:homology_type_id",
           "homology:lca_taxon_id -> taxon:taxon_id",
           "duplicate_source_file:analysis_id -> analysis:analysis_id")
dot <- paste0('digraph ER { graph [rankdir=LR, splines=true, nodesep=0.8, ranksep=1.3, fontname="Helvetica",
  label="IMPC_Group11 schema   (blue: experimental core | yellow: parameter groups | red: disease | green: bespoke homology | grey: provenance)\\nArrows point from the FK column to the referenced PK column; crow foot = many side", labelloc=t, fontsize=16];
  node [shape=plaintext, fontname="Helvetica", fontsize=11];
  edge [arrowhead=normal, arrowtail=crow, dir=both, color="#566573", arrowsize=0.7, penwidth=0.8];\n',
  paste(nodes, collapse = "\n"), "\n", paste(edges, collapse = ";\n"), ";\n}")
dir.create("docs", showWarnings = FALSE)
svg <- export_svg(grViz(dot))
groups <- gregexpr('(?s)<g id="(node|edge)[0-9]+" class="(node|edge)">.*?</g>\\n?', svg, perl = TRUE)
if (groups[[1]][1] > 0) {
  blocks <- regmatches(svg, groups)[[1]]
  g <- groups[[1]]
  svg <- paste0(substr(svg, 1, g[1] - 1),
                paste(blocks[grepl('class="edge"', blocks)], collapse = "\n"), "\n",
                paste(blocks[grepl('class="node"', blocks)], collapse = "\n"), "\n",
                substr(svg, g[length(g)] + attr(g, "match.length")[length(g)], nchar(svg)))
}
writeLines(svg, "docs/ER_diagram.svg")
svg_width <- as.numeric(sub('(?s).*<svg width="([0-9.]+)pt".*', '\\1', svg, perl = TRUE))
rsvg_png(charToRaw(svg), "docs/ER_diagram.png", width = ceiling(svg_width * 3.2))
cat("ER diagram written to docs/\n")
