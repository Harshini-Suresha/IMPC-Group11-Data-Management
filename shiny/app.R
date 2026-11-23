# =============================================================================
# IMPC Group 11 - Phenotype Explorer (RShiny dashboard)
#   1. Gene -> phenotypes : every phenotype score for one knockout (PheWAS-style)
#   2. Phenotype -> genes : every knockout score for one phenotype
#   3. Interactive clusters: genes with similar phenotype profiles
#   4. Genes of interest  : the collaborator's genes side by side
# Run from the repository root:  shiny::runApp("shiny")
# =============================================================================
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(plotly); library(DT); library(data.table)
})
for (f in c("R/data_access.R", "R/plots.R")) source(f, local = TRUE)   # data layer + clustering helpers

`%||%` <- function(a, b) if (is.null(a)) b else a
ds <- open_data_source()
onStop(function() if (ds$type == "mysql") try(DBI::dbDisconnect(ds$con), silent = TRUE))

GENES  <- list_genes(ds)
PARAMS <- list_parameters(ds)
GROUPS <- list_groups(ds)
EX     <- load_extras(ds)
QUERY_GENES <- c("Cog2", "Myof", "Lpcat2b", "Ifng", "Tspyl2")
gene_choices  <- setNames(GENES$mgi_gene_id, sprintf("%s  ·  %s", GENES$gene_symbol, GENES$mgi_gene_id))
param_choices <- setNames(PARAMS$parameter_stable_id,
                          sprintf("%s — %s (%d genes)", PARAMS$parameter_name, PARAMS$procedure_name, PARAMS$n_genes))
default_gene <- GENES[gene_symbol == "Ifng", mgi_gene_id][1]

search_suggestions <- c(
  setNames(GENES$gene_symbol, sprintf("Gene · %s", GENES$gene_symbol)),
  setNames(PARAMS$parameter_name, sprintf("Phenotype · %s", PARAMS$parameter_name)),
  setNames(unique(EX$disease_all$disease_name),
           sprintf("Disease · %s", unique(EX$disease_all$disease_name))))
search_suggestions <- search_suggestions[!is.na(search_suggestions) & nzchar(search_suggestions)]
search_suggestions <- search_suggestions[order(names(search_suggestions))]

# ---- visual identity: saturated, publication-grade scientific palette --------
# Categorical: Okabe-Ito (colour-vision safe); sequential: viridis; accent
# gradient: plasma. Deep blue + red brand accents on flat white surfaces.
COL <- list(ink = "#14213d", ink2 = "#3f4f66", navy = "#0039a6", sig = "#d55e00", ns = "#0072b2",
            hl = "#cc79a7", grid = "#e6ebf2", page = "#f2f5f9", teal = "#009e73", green = "#009e73",
            amber = "#e69f00", coral = "#d55e00", pink = "#cc79a7", violet = "#7b3294", sky = "#56b4e9",
            line = "#c9d3e0", red = "#cc0000")
OI <- c("#0072b2", "#d55e00", "#009e73", "#cc79a7", "#e69f00", "#56b4e9", "#f0e442")
SEQ <- list(c(0, "#440154"), c(0.14, "#3e4a89"), c(0.28, "#31688e"), c(0.43, "#26828e"),
            c(0.57, "#1f9e89"), c(0.71, "#35b779"), c(0.86, "#b4de2c"), c(1, "#fde725"))
HOT <- list(c(0, "#0d0887"), c(0.4, "#7e03a8"), c(0.75, "#cc4778"), c(1, "#f89441"))
FONT <- "'Source Sans 3', 'Helvetica Neue', Arial, sans-serif"
ramp_cols <- function(v, pal = OI) {
  if (!length(v)) return(character(0))
  r <- colorRampPalette(pal)(101); span <- max(v) - min(v)
  idx <- if (span <= 0) rep(51L, length(v)) else round(1 + 100 * (v - min(v)) / span)
  r[pmax(1L, pmin(101L, idx))]
}
fmt_p <- function(p) ifelse(p == 0, "< 1e-300", formatC(p, format = "g", digits = 3))
nlp <- function(p, cap = 1e6) pmin(-log10(pmax(p, 1e-300)), cap)

axis_style <- function(title = NULL, ...) list(title = list(text = title, font = list(size = 13, color = COL$ink2)),
  showline = TRUE, linecolor = COL$line, linewidth = 1, ticks = "outside", tickcolor = COL$line, ticklen = 4,
  gridcolor = COL$grid, zeroline = FALSE, tickfont = list(size = 12, color = COL$ink), automargin = TRUE, ...)
finish <- function(p, ...) p |>
  layout(font = list(family = FONT, size = 12, color = COL$ink), paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff",
         hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#c9d6f2", font = list(family = FONT, size = 12, color = COL$ink)),
         margin = list(l = 10, r = 20, t = 10, b = 10), ...) |>
  config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
         toImageButtonOptions = list(format = "svg", filename = "impc_group11_plot"))

stat <- function(label, value_id, note = NULL) div(class = "kpi",
  div(class = "kpi-label", label), div(class = "kpi-value", textOutput(value_id, inline = TRUE)),
  if (!is.null(note)) div(class = "kpi-note", note))

thr_input <- function(id, fdr = FALSE) selectInput(id, "Significance threshold",
  c(if (fdr) c("FDR q < 0.05 (Benjamini-Hochberg)" = "fdr"), "P < 0.05" = 0.05, "P < 0.01" = 0.01, "P < 0.001" = 0.001, "P < 1e-4" = 1e-4, "P < 1e-6" = 1e-6), selected = 0.05)

CSS <- "
body{background:#f2f5f9;font-size:15px;color:#14213d}
.bslib-page-navbar,.tab-content,.tab-pane{background:transparent}
a{color:#0039a6}
.navbar{box-shadow:0 1px 3px rgba(20,33,61,.06)!important;background:#ffffff!important;border-bottom:1px solid #d9e1ec!important;border-top:4px solid #0039a6;padding-top:0;padding-bottom:0}
.navbar-brand{font-weight:700;letter-spacing:.01em;color:#14213d!important;font-size:1.15rem}
.navbar .nav-link{color:#3f4f66!important;font-weight:600;font-size:.92rem;padding:1rem .9rem!important;border-bottom:3px solid transparent}
.navbar .nav-link:hover{color:#0039a6!important}
.navbar .nav-link.active{color:#14213d!important;border-bottom:3px solid #cc0000}
.hero{background:#ffffff;border:1px solid #d9e1ec;border-left:6px solid #0039a6;border-radius:8px;padding:1.4rem 1.6rem;margin-bottom:1rem;box-shadow:0 1px 3px rgba(20,33,61,.05)}
.hero h2{font-size:1.5rem;font-weight:800;color:#14213d;margin:0 0 .4rem}
.hero p{color:#3f4f66;margin:0;max-width:880px}
.kpi-row.six{grid-template-columns:repeat(6,1fr)}
.page{padding:1rem 1.2rem}
.card{background:#fff;border:1px solid #d9e1ec;border-top:3px solid #0039a6;border-radius:8px;box-shadow:0 1px 3px rgba(20,33,61,.05)}
.card:nth-child(even){border-top-color:#cc0000}
.card-header{background:#fff;font-weight:700;font-size:1rem;color:#14213d;border-bottom:1px solid #e6ebf2;padding:.75rem 1rem}
.card-header .sub{font-weight:500;color:#5b6b80;font-size:.88rem;margin-left:.5rem}
.bslib-sidebar-layout>.sidebar{background:#fff;border-right:1px solid #d9e1ec}
.sidebar .control-label,.sidebar label{font-weight:600;font-size:.85rem;color:#14213d}
.form-control:focus,.form-select:focus{border-color:#0039a6;box-shadow:0 0 0 .2rem rgba(0,57,166,.14)}
.section-title{font-size:.72rem;letter-spacing:.08em;text-transform:uppercase;color:#0039a6;font-weight:700;margin:.4rem 0 .6rem}
.kpi-row{display:grid;grid-template-columns:repeat(4,1fr);background:#fff;border:1px solid #d9e1ec;border-radius:8px;overflow:hidden;margin-bottom:1rem;box-shadow:0 1px 3px rgba(20,33,61,.05)}
.kpi{padding:.9rem 1.1rem;border-right:1px solid #e6ebf2;border-top:4px solid #0039a6}
.kpi:last-child{border-right:none}
.kpi:nth-child(6n+1){border-top-color:#0039a6}
.kpi:nth-child(6n+2){border-top-color:#d55e00}
.kpi:nth-child(6n+3){border-top-color:#009e73}
.kpi:nth-child(6n+4){border-top-color:#e69f00}
.kpi:nth-child(6n+5){border-top-color:#cc79a7}
.kpi:nth-child(6n){border-top-color:#cc0000}
.kpi-label{font-size:.75rem;letter-spacing:.06em;text-transform:uppercase;color:#5b6b80;font-weight:600}
.kpi-value{font-size:1.65rem;font-weight:700;color:#14213d;line-height:1.25;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.kpi:nth-child(6n+1) .kpi-value{color:#0039a6}
.kpi:nth-child(6n+2) .kpi-value{color:#d55e00}
.kpi:nth-child(6n+3) .kpi-value{color:#009e73}
.kpi:nth-child(6n+4) .kpi-value{color:#b07d00}
.kpi:nth-child(6n+5) .kpi-value{color:#cc79a7}
.kpi:nth-child(6n) .kpi-value{color:#cc0000}
.kpi-note{font-size:.8rem;color:#5b6b80}
.source-badge{font-size:.78rem;color:#3f4f66;padding:.35rem 1rem;background:#fff;border-bottom:1px solid #d9e1ec}
.legend-chip{display:inline-flex;align-items:center;margin-right:1rem;font-size:.85rem;color:#3f4f66}
.legend-chip i{width:10px;height:10px;border-radius:50%;display:inline-block;margin-right:.35rem}
table.dataTable{font-size:.88rem}
table.dataTable thead th{background:#e8eef7!important;color:#14213d!important;border-bottom:2px solid #c9d3e0!important;font-weight:700}
table.dataTable.stripe>tbody>tr.odd>td,table.dataTable.stripe>tbody>tr.odd>th{background-color:#f6f8fb!important}
table.dataTable tbody>tr:hover>td,table.dataTable tbody>tr:hover>th{background-color:#eef3fa!important}
.dataTables_wrapper .dataTables_paginate .paginate_button.current{background:#0039a6!important;border:none!important;color:#fff!important;border-radius:4px}
.btn-vibrant{background:#0039a6!important;border:1px solid #0039a6!important;color:#fff!important;font-weight:600;border-radius:6px!important}
.btn-vibrant:hover{background:#002b7f!important;border-color:#002b7f!important}
input[type=checkbox],input[type=radio]{accent-color:#0039a6}
.selectize-control.multi .selectize-input>div{background:#0039a6;border:none;color:#fff}
.irs--shiny .irs-bar{background:#0039a6;border:none}
.irs--shiny .irs-handle{border-color:#0039a6!important}
.irs--shiny .irs-single,.irs--shiny .irs-from,.irs--shiny .irs-to{background:#0039a6!important}
.help{font-size:.82rem;color:#5b6b80;line-height:1.4}
#shiny-disconnected-overlay{background:rgba(255,255,255,.58)!important;opacity:1!important}
#shiny-disconnected-overlay::after{content:'Connection to the server was lost - reload the page (Cmd R / Ctrl R) to reconnect.';
  position:fixed;top:72px;left:50%;transform:translateX(-50%);background:#cc0000;color:#fff;padding:.7rem 1.2rem;border-radius:6px;
  font-weight:600;box-shadow:0 4px 14px rgba(20,33,61,.18);z-index:99999}
.about-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:1rem;margin-top:1rem}
.about-card{background:#fff;border:1px solid #d9e1ec;border-radius:8px;padding:1.1rem 1.2rem;border-top:4px solid #0039a6;box-shadow:0 1px 3px rgba(20,33,61,.05)}
.about-grid .about-card:nth-child(2){border-top-color:#cc0000}
.about-grid .about-card:nth-child(3){border-top-color:#009e73}
.about-grid .about-card:nth-child(4){border-top-color:#cc79a7}
.about-grid .about-card:nth-child(5){border-top-color:#e69f00}
.about-grid .about-card:nth-child(6){border-top-color:#14213d}
.about-card.red{border-top-color:#cc0000}.about-card.teal{border-top-color:#009e73}
.about-card h4{font-size:1rem;font-weight:700;margin:0 0 .4rem;color:#14213d}
.about-card p,.about-card li{font-size:.9rem;color:#3f4f66;margin:0}
.about-card ul{padding-left:1.1rem;margin:0}
.steps{display:flex;gap:.5rem;flex-wrap:wrap;margin-top:.8rem}
.step{background:#0039a6;border:1px solid #0039a6;border-radius:6px;padding:.3rem .9rem;font-size:.85rem;font-weight:700;color:#fff}
.steps .step:nth-child(3){background:#009e73;border-color:#009e73}
.steps .step:nth-child(5){background:#e69f00;border-color:#e69f00;color:#14213d}
.steps .step:nth-child(7){background:#cc0000;border-color:#cc0000}
.steps .step:nth-child(9){background:#cc79a7;border-color:#cc79a7;color:#14213d}
.steps .step:nth-child(11){background:#14213d;border-color:#14213d}
.step.arrow{background:none;border:none;color:#0039a6;padding:.3rem 0}
"

legend_chips <- div(style = "padding:.2rem 1rem 0",
  span(class = "legend-chip", tags$i(style = paste0("background:", COL$sig)), "Significant"),
  span(class = "legend-chip", tags$i(style = paste0("background:", COL$ns)), "Not significant"),
  span(class = "legend-chip", "\u25B6 drawn at the display cap"))

ui <- page_navbar(id = "nav",
  title = tags$span(tags$span("IMPC", style = "color:#cc0000;font-weight:700"), " Group 11 · Phenotype Explorer"),
  window_title = "IMPC Group 11 · Phenotype Explorer", fillable = FALSE, navbar_options = navbar_options(bg = "#ffffff", theme = "light"),
  theme = bs_theme(version = 5, bg = "#ffffff", fg = COL$ink, primary = "#0039a6", secondary = COL$ink2,
                   danger = "#cc0000", base_font = font_collection("Source Sans 3", "Helvetica Neue", "Arial", "sans-serif"),
                   "border-radius" = "6px", "input-border-color" = "#c9d3e0") |> bs_add_rules(CSS),
  header = tagList(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600;700&display=swap"),
    div(class = "source-badge", textOutput("source_label", inline = TRUE))),


  # --------------------------------------------------------------- overview --
  nav_panel("Overview",
    div(class = "page",
      div(class = "hero", h2("Mouse knockout phenotypes at a glance"),
          p("Pre-analysed IMPC phenotyping results for Group 11, cleaned and stored in the normalised MySQL database IMPC_Group11.
             Each analysis tests whether knocking out a gene changes a phenotype relative to wild-type mice.")),
      div(class = "kpi-row six", stat("Knockout genes", "o_genes"), stat("Phenotypes", "o_params"), stat("Analyses", "o_an"),
          stat("P < 0.05", "o_sig"), stat("Procedures", "o_proc"), stat("Disease-linked genes", "o_dis")),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Distribution of P values", span(class = "sub", "excess near zero = real phenotypic signal")),
             plotlyOutput("o_hist", height = "360px")),
        card(card_header("Phenotyping breadth vs. hits per knockout", span(class = "sub", "hover a point; genes of interest outlined")),
             plotlyOutput("o_scatter", height = "360px"))),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Hit rate by phenotyping procedure", span(class = "sub", "share of analyses with P < 0.05; ≥ 100 analyses")),
             plotlyOutput("o_proc_plot", height = "auto")),
        card(card_header("Life stage of analyses"), plotlyOutput("o_stage", height = "300px"),
             card_header("Genetic background"), plotlyOutput("o_strain", height = "300px"))))),
  # ------------------------------------------------------------------ tab 1 --
  nav_panel("Gene → phenotypes",
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(width = 300,
        div(class = "section-title", "Knockout"),
        selectizeInput("gene", NULL, choices = NULL, options = list(placeholder = "Type a gene symbol")),
        div(class = "section-title", "Filters"),
        thr_input("thr1", fdr = TRUE),
        selectInput("grp1", "Parameter group", c("All groups" = "", setNames(GROUPS$group_name, GROUPS$group_name))),
        checkboxInput("adult1", "Adult life stages only", FALSE),
        sliderInput("cap1", "Cap −log10 P for display", 5, 60, 20, step = 5),
        div(class = "help", "Each point is one statistical test (knockout vs. wild-type). P values reported as 0 are drawn at the cap."),
        hr(), downloadButton("dl1", "Download results (CSV)", class = "btn-vibrant btn-sm w-100")),
      div(class = "kpi-row", stat("Phenotypes tested", "k_tested"), stat("Significant", "k_sig"),
          stat("Strongest phenotype", "k_top"), stat("Human disease links", "k_dis")),
      layout_columns(col_widths = c(8, 4),
        card(card_header("Phenotype scores by procedure", span(class = "sub", "−log10 P; dashed line = threshold")),
             legend_chips, plotlyOutput("p_gene", height = "auto")),
        card(card_header("Top significant phenotypes"), DTOutput("t_top"))),
      navset_card_underline(
        nav_panel("All results", DTOutput("t_gene_all")),
        nav_panel("Disease Ontology links", DTOutput("t_gene_dis")),
        nav_panel("Homologs (bespoke task)", DTOutput("t_gene_hom"))))),

  # ------------------------------------------------------------------ tab 2 --
  nav_panel("Phenotype → genes",
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(width = 300,
        div(class = "section-title", "Phenotype"),
        selectizeInput("param", NULL, choices = NULL, options = list(placeholder = "Type a phenotype")),
        div(class = "section-title", "Display"),
        thr_input("thr2"),
        sliderInput("topn", "Genes shown (ranked)", 10, 80, 35, step = 5),
        selectizeInput("hl2", "Highlight genes", choices = NULL, multiple = TRUE),
        hr(), downloadButton("dl2", "Download results (CSV)", class = "btn-vibrant btn-sm w-100")),
      div(class = "kpi-row", stat("Knockouts tested", "k2_n"), stat("Significant", "k2_sig"),
          stat("Procedure", "k2_proc"), stat("Strongest knockout", "k2_top")),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Most affected knockouts", span(class = "sub", "ranked by P value")), plotlyOutput("p_param", height = "auto")),
        card(card_header("Distribution across all knockouts"), plotlyOutput("p_param_hist", height = "360px"),
             div(class = "help", style = "padding:0 1rem 1rem", "Histogram of −log10 P for every knockout tested; the red line marks the threshold."))),
      card(card_header("All knockouts tested for this phenotype"), DTOutput("t_param")))),

  # ------------------------------------------------------------------ tab 3 --
  nav_panel("Interactive cluster explorer",
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(width = 300,
        div(class = "section-title", "Features"),
        radioButtons("lvl", NULL, c("Parameter groups" = "group", "Procedures" = "procedure", "Most-tested parameters" = "parameter")),
        sliderInput("topf", "Maximum features", 5, 60, 40),
        sliderInput("minf", "Gene tested in at least", 1, 20, 4, post = " features"),
        div(class = "section-title", "Clustering"),
        radioButtons("meth", NULL, c("Hierarchical (Ward)" = "hierarchical", "k-means" = "kmeans")),
        conditionalPanel("input.meth == 'hierarchical'",
          radioButtons("dist", "Distance", c("Euclidean" = "euclidean", "1 − Pearson r" = "correlation"))),
        sliderInput("k", "Number of clusters", 2, 8, 5),
        selectizeInput("hl3", "Highlight genes", choices = NULL, multiple = TRUE),
        div(class = "help", "Each gene is described by its strongest −log10 P in each feature; untested combinations score 0.")),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Genes in phenotype space", span(class = "sub", "PCA of feature scores")), plotlyOutput("p_pca", height = "520px")),
        card(card_header("What defines each cluster", span(class = "sub", "% of members with P < 0.05")), plotlyOutput("p_prof", height = "520px"))),
      card(card_header("Cluster heatmap", span(class = "sub", "genes ordered by cluster; colour = −log10 P")), plotlyOutput("p_heat", height = "520px")),
      card(card_header("Cluster membership"), DTOutput("t_clu")))),


  # ------------------------------------------------------- parameter groups --
  nav_panel("Parameter groups",
    div(class = "page",
      div(class = "hero", h2("Eleven parameter groups"),
          p("Weight, Images and Brain were required by the brief; eight further groups were added by the team.
             Membership is many-to-many, so one parameter (e.g. heart weight) can belong to several groups.")),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Hit rate per group", span(class = "sub", "95% Wilson interval; dashed = overall rate")),
             plotlyOutput("g_rate", height = "420px")),
        card(card_header("Parameters shared between groups"), plotlyOutput("g_overlap", height = "420px"))),
      layout_columns(col_widths = c(4, 8),
        card(card_header("Explore a group"),
             card_body(selectInput("g_sel", "Parameter group", choices = setNames(GROUPS$group_name, GROUPS$group_name)),
                       div(class = "help", "Shows the knockouts with the most significant phenotypes in this group."),
                       tableOutput("g_rule"))),
        card(card_header(textOutput("g_title", inline = TRUE)), plotlyOutput("g_top", height = "auto"))))),

  # ------------------------------------------------------- disease & homology --
  nav_panel("Disease & homology",
    div(class = "page",
      div(class = "hero", h2("Links to human disease and cross-species conservation"),
          p("Disease Ontology annotations come from the MGI report via one-to-one human orthologs; homology comes from the bespoke
             Ensembl BioMart task (human and vervet).")),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Diseases with the most IMPC knockouts", span(class = "sub", "genes with phenotype data")),
             plotlyOutput("d_top", height = "460px")),
        card(card_header("Do disease genes show more phenotypes?", span(class = "sub", "per-gene hit rate, ≥ 5 tests")),
             plotlyOutput("d_box", height = "400px"), div(class = "help", style = "padding:0 1rem 1rem", textOutput("d_test")))),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Sequence identity of mouse homologs", span(class = "sub", "by orthology type and species")),
             plotlyOutput("h_violin", height = "420px")),
        card(card_header("Search a disease"),
             card_body(selectizeInput("d_sel", NULL, choices = NULL, options = list(placeholder = "Type a disease name")),
                       DTOutput("d_genes")))))),
  # ------------------------------------------------------------------ tab 4 --
  nav_panel("Genes of interest",
    div(style = "padding:1rem",
      div(class = "kpi-row", stat("Genes requested", "k4_n"), stat("Phenotypes tested", "k4_tests"),
          stat("Significant (P < 0.05)", "k4_sig"), stat("Disease-linked genes", "k4_dis")),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Strongest effect per procedure", span(class = "sub", "cell text = significant / tested")),
             plotlyOutput("p_query", height = "620px")),
        card(card_header("Summary per gene"), DTOutput("t_query"))))),

  # ------------------------------------------------------- gene comparison --
  nav_panel("Gene comparison",
    div(class = "page",
      layout_sidebar(fillable = FALSE,
        sidebar = sidebar(width = 300,
          div(class = "section-title", "Genes"),
          selectizeInput("compare_genes", NULL, choices = NULL, multiple = TRUE,
                         options = list(maxItems = 8, placeholder = "Choose 2-8 genes")),
          radioButtons("compare_level", "Comparison feature",
                       c("Procedures" = "procedure", "Parameter groups" = "group")),
          div(class = "help", "Cell values are the strongest effect for each gene and feature. Text shows significant / tested."),
          downloadButton("dl_compare", "Download comparison (CSV)", class = "btn-vibrant btn-sm w-100")),
        div(class = "kpi-row",
          stat("Genes compared", "kc_n"), stat("Features", "kc_features"),
          stat("Shared significant hits", "kc_shared"), stat("Strongest gene-feature pair", "kc_top")),
        card(card_header("Side-by-side phenotype profile"),
             plotlyOutput("p_compare", height = "620px")),
        card(card_header("Comparison table"), DTOutput("t_compare"))))),

  # ------------------------------------------------ phenotype enrichment --
  nav_panel("Phenotype enrichment",
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(width = 300,
        div(class = "section-title", "Gene"),
        selectizeInput("enrich_gene", NULL, choices = NULL, options = list(placeholder = "Choose a knockout")),
        radioButtons("enrich_threshold", "Significance threshold",
                     c("P < 0.05" = "0.05", "P < 0.01" = "0.01", "P < 0.001" = "0.001")),
        div(class = "help", "Two-sided Fisher exact tests compare significant analyses for the selected gene with the rest of the dataset in each parameter group.")),
      div(class = "kpi-row",
        stat("Parameter groups", "ke_groups"), stat("Nominal enrichments", "ke_nom"),
        stat("FDR enrichments", "ke_fdr"), stat("Strongest enrichment", "ke_top")),
      card(card_header("Parameter-group enrichment",
                       span(class = "sub", "odds ratios and Benjamini-Hochberg adjusted P")),
           plotlyOutput("p_enrich", height = "460px")),
      card(card_header("Enrichment test results"), DTOutput("t_enrich")))),

  # --------------------------------------------- global search & network --
  nav_panel("Search & network",
    div(class = "page",
      div(class = "hero", h2("Search & network"),
          p("Search across knockouts, phenotypes and diseases with live suggestions and filters. Click a result to open
             its detail tab, or use the gene's network button to redraw the gene–phenotype–disease graph.")),
      div(class = "kpi-row",
          stat("Matching records", "ks_total"), stat("Genes", "ks_genes"),
          stat("Phenotypes", "ks_params"), stat("Diseases", "ks_diseases")),
      layout_columns(col_widths = c(5, 7),
        card(card_header("Global search", span(class = "sub", "suggestions · filters · clickable results")),
             card_body(
               selectizeInput("global_search", NULL, choices = NULL,
                              options = list(create = TRUE, maxItems = 1,
                                             placeholder = "Type a gene, phenotype or disease")),
               selectInput("search_category", "Result type",
                           c("All" = "all", "Genes" = "gene", "Phenotypes" = "phenotype", "Diseases" = "disease")),
               sliderInput("search_limit", "Maximum results", 10, 200, 50),
               div(class = "help", "Search matches gene symbols and MGI IDs, phenotype names and stable IDs, and disease names.
                    Results are capped so the table stays responsive."),
               DTOutput("t_search"),
               downloadButton("dl_search", "Download results (CSV)", class = "btn-vibrant btn-sm mt-2"))),
        card(card_header("Phenotype network",
                         span(class = "sub", "gene → phenotypes → diseases")),
             card_body(
               selectizeInput("network_gene", "Centre on gene", choices = NULL,
                              options = list(placeholder = "Choose a knockout")),
               sliderInput("network_limit", "Strongest phenotypes", 5, 40, 15),
               checkboxInput("network_diseases", "Include disease links", TRUE),
               downloadButton("dl_network", "Export network (CSV)", class = "btn-vibrant btn-sm mb-2"),
               plotlyOutput("p_network", height = "560px"),
               div(class = "help", "Node colour is −log10 P (viridis); red nodes are Disease Ontology annotations.
                    Hover a phenotype for its procedure, parameter group, allele, stage, strain, P and BH q value."),
               DTOutput("t_network"))))),


  # ------------------------------------------------- one-click HTML report --
  nav_panel("Gene HTML report",
    div(class = "page",
      div(class = "hero", h2("One-click gene report"),
          p("Generate a standalone HTML report for a knockout, including phenotype results, disease links and homologous genes.")),
      layout_columns(col_widths = c(4, 8),
        card(card_header("Report options"),
             card_body(selectizeInput("report_gene", NULL, choices = NULL,
                                      options = list(placeholder = "Choose a knockout")),
                       downloadButton("dl_report", "Generate HTML report", class = "btn-vibrant w-100"))),
        card(card_header("Report preview"),
             card_body(uiOutput("report_preview"))))),

  # ------------------------------------------------------------ data quality --
  nav_panel("Data quality",
    div(class = "page",
      div(class = "hero", h2("What the cleaning pipeline changed"),
          p("Every corrected value is stored in the table data_cleaning_log with its raw value, clean value and the rule applied,
             so each decision is auditable.")),
      div(class = "kpi-row six", stat("Values corrected", "q_log"), stat("Malformed files repaired", "q_imp"),
          stat("Duplicate files merged", "q_dup"), stat("Repeat analyses flagged", "q_rep"),
          stat("Parameter IDs recovered", "q_pid"), stat("P values missing", "q_pm")),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Corrections per field"), plotlyOutput("q_fields", height = "auto")),
        card(card_header("Database contents", span(class = "sub", "rows per table, log scale")), plotlyOutput("q_tables", height = "auto"))),
      card(card_header("Most frequent corrections", span(class = "sub", "raw value → cleaned value")), DTOutput("q_examples")))),
  nav_panel("About",
    div(class = "page",
      div(class = "hero", h2("About the Phenotype Explorer"),
          p("An interactive view of the IMPC_Group11 MySQL database, built for the collaborator to explore which phenotypes
             each knockout affects, which knockouts affect each phenotype, and how genes group by phenotype profile."),
          div(class = "steps", span(class = "step", "28,670 TRE files"), span(class = "step arrow", "\u2192"),
              span(class = "step", "R cleaning pipeline"), span(class = "step arrow", "\u2192"),
              span(class = "step", "MySQL IMPC_Group11"), span(class = "step arrow", "\u2192"),
              span(class = "step", "Parameterised SQL"), span(class = "step arrow", "\u2192"), span(class = "step", "This dashboard"))),
      div(class = "about-grid",
        div(class = "about-card red", h4("Reading the plots"),
            p("Each point is one statistical analysis comparing a knockout line with wild-type controls. Axes show \u2212log10 P:
               larger values mean stronger evidence of an effect. Vermillion = significant at the chosen threshold, blue = not significant.")),
        div(class = "about-card", h4("Database"),
            tags$ul(tags$li("26 normalised tables, 4 views"), tags$li("11 parameter groups (3 required + 8 added)"),
                    tags$li("Disease Ontology links via human orthologs"), tags$li("Bespoke Ensembl homology (human, vervet)"))),
        div(class = "about-card teal", h4("Clustering"),
            p("Genes are described by their strongest \u2212log10 P per parameter group, procedure or parameter, then clustered with
               Ward hierarchical clustering or k-means. PCA is used for display only.")),
        div(class = "about-card teal", h4("Data access"),
            p("Queries run against MySQL when db_config.cnf connects; otherwise a bundled snapshot of the same database is used,
               so the app always launches from the GitHub repository.")),
        div(class = "about-card red", h4("Caveats"),
            tags$ul(tags$li("P values are nominal (no multiple-testing correction)"), tags$li("254 P values reported as 0 are capped for display"),
                    tags$li("Untested gene\u2013feature pairs score 0 when clustering"))),
        div(class = "about-card", h4("Source code"),
            p(tags$a(href = "https://github.com/Harshini-Suresha/IMPC-Group11-Data-Management", target = "_blank",
                     "github.com/Harshini-Suresha/IMPC-Group11-Data-Management")),
            p(style = "margin-top:.4rem", "Run with shiny::runApp(\"shiny\") from the repository root."))))))))

# =============================================================================
server <- function(input, output, session) {
  session$allowReconnect(TRUE)
  output$source_label <- renderText(paste("Data source:", ds$label, " ·  ", format(nrow(GENES), big.mark = ","),
                                          "knockout genes  · ", format(nrow(PARAMS), big.mark = ","), "phenotypes"))
  # shareable links: ?tab=gene&gene=Tspyl2 , ?tab=phenotype&param=IMPC_DXA_007_001 , ?tab=clusters ...
  qs <- isolate(parseQueryString(session$clientData$url_search))
  start_gene <- if (!is.null(qs$gene)) GENES[gene_symbol == qs$gene | mgi_gene_id == qs$gene, mgi_gene_id][1] else default_gene
  if (is.na(start_gene)) start_gene <- default_gene
  start_param <- if (!is.null(qs$param) && qs$param %in% PARAMS$parameter_stable_id) qs$param else PARAMS$parameter_stable_id[1]
  updateSelectizeInput(session, "gene", choices = gene_choices, selected = start_gene, server = TRUE)
  updateSelectizeInput(session, "param", choices = param_choices, selected = start_param, server = TRUE)
  TABS <- c(overview = "Overview", gene = "Gene \u2192 phenotypes", phenotype = "Phenotype \u2192 genes", clusters = "Interactive cluster explorer",
            groups = "Parameter groups", disease = "Disease & homology", interest = "Genes of interest",
            compare = "Gene comparison", enrich = "Phenotype enrichment", search = "Search & network",
            report = "Gene HTML report", quality = "Data quality", about = "About")
  if (!is.null(qs$tab) && qs$tab %in% names(TABS)) nav_select("nav", TABS[[qs$tab]])
  else if (!is.null(qs$gene)) nav_select("nav", TABS[["gene"]])
  for (id in c("hl2", "hl3")) updateSelectizeInput(session, id, choices = sort(GENES$gene_symbol), selected = QUERY_GENES, server = TRUE)
  updateSelectizeInput(session, "compare_genes", choices = gene_choices, selected = head(GENES[gene_symbol %in% QUERY_GENES, mgi_gene_id], 3), server = TRUE)
  updateSelectizeInput(session, "enrich_gene", choices = gene_choices, selected = default_gene, server = TRUE)
  updateSelectizeInput(session, "network_gene", choices = gene_choices, selected = default_gene, server = TRUE)
  updateSelectizeInput(session, "global_search", choices = search_suggestions, server = TRUE)
  updateSelectizeInput(session, "report_gene", choices = gene_choices, selected = default_gene, server = TRUE)

  dt <- function(x, page = 10, dom = "ftip", escape = TRUE) datatable(x, rownames = FALSE,
    class = "compact stripe hover", escape = escape,
    options = list(pageLength = page, scrollX = TRUE, dom = dom, autoWidth = FALSE))


  # ============================================================ new sections
  AN <- if (ds$type == "mysql") q(ds, "SELECT a.analysis_id, a.mgi_gene_id, g.gene_symbol, a.parameter_stable_id,
          COALESCE(pr.procedure_name, 'Not in IMPC reference') procedure_name, ls.life_stage_name, s.strain_name, a.p_value
          FROM analysis a JOIN gene g ON g.mgi_gene_id = a.mgi_gene_id JOIN strain s ON s.strain_id = a.strain_id
          LEFT JOIN life_stage ls ON ls.life_stage_id = a.life_stage_id
          LEFT JOIN parameter p ON p.parameter_stable_id = a.parameter_stable_id
          LEFT JOIN `procedure` pr ON pr.procedure_id = p.procedure_id
          WHERE a.content_duplicate_of IS NULL AND a.p_value IS NOT NULL AND a.p_value <= 1")
        else ds$snap$analysis[is.na(content_duplicate_of) & !is.na(p_value) & p_value <= 1][
          , procedure_name := fifelse(is.na(procedure_name), "Not in IMPC reference", procedure_name)][]
  GM <- if (ds$type == "mysql") q(ds, "SELECT m.parameter_stable_id, pg.group_name, pg.rule_description FROM parameter_group_member m
          JOIN parameter_group pg ON pg.group_id = m.group_id") else merge(ds$snap$group_member,
          ds$snap$parameter_group[, .(group_name, rule_description)], by = "group_name")
  wilson <- function(x, n, z = 1.96) { p <- x / n; d <- 1 + z^2 / n; c <- (p + z^2 / (2 * n)) / d
    h <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d; list(c - h, c + h) }
  fmtn <- function(x) format(x, big.mark = ",")
  dis_genes <- unique(EX$disease_all$mgi_gene_id)

  # ---- overview
  output$o_genes  <- renderText(fmtn(uniqueN(AN$mgi_gene_id)))
  output$o_params <- renderText(fmtn(uniqueN(AN$parameter_stable_id)))
  output$o_an     <- renderText(fmtn(nrow(AN)))
  output$o_sig    <- renderText(sprintf("%s (%.1f%%)", fmtn(sum(AN$p_value < 0.05)), 100 * mean(AN$p_value < 0.05)))
  output$o_proc   <- renderText(uniqueN(AN$procedure_name))
  output$o_dis    <- renderText(fmtn(sum(unique(AN$mgi_gene_id) %in% dis_genes)))
  output$o_hist <- renderPlotly({
    x <- AN[p_value < 1]; flat <- nrow(x[p_value >= 0.5]) / 20
    br <- seq(0, 1, by = 0.025); cnt <- hist(x$p_value, breaks = br, plot = FALSE)$counts
    h <- data.table(mid = head(br, -1) + 0.0125, n = cnt)
    h[, col := ramp_cols(mid, c("#440154", "#31688e", "#35b779", "#fde725"))]
    plot_ly(h, x = ~mid, y = ~n, type = "bar",
            marker = list(color = ~col, line = list(color = "#ffffff", width = 0.6)),
            hovertemplate = "P %{x:.3f}<br>%{y} analyses<extra></extra>") |>
      finish(bargap = 0, xaxis = axis_style("Nominal P value", range = c(0, 1)), yaxis = axis_style("Analyses"),
             shapes = list(list(type = "line", x0 = 0, x1 = 1, y0 = flat, y1 = flat, line = list(color = COL$sig, dash = "dash"))),
             annotations = list(list(x = 0.98, y = flat, text = "expected without effects", showarrow = FALSE, xanchor = "right",
                                     yanchor = "bottom", font = list(size = 11, color = COL$sig))))
  })
  output$o_scatter <- renderPlotly({
    g <- AN[, .(tests = .N, hits = sum(p_value < 0.05)), by = gene_symbol][, rate := hits / tests]
    set.seed(2); g[, jx := tests * exp(runif(.N, -0.05, 0.05))][, jy := hits + runif(.N, -0.2, 0.2)]
    q4 <- g[gene_symbol %in% QUERY_GENES]
    plot_ly() |>
      add_markers(data = g, x = ~jx, y = ~jy,
                  text = ~sprintf("<b>%s</b><br>%d tested, %d with P < 0.05<br>hit rate %.1f%%", gene_symbol, tests, hits, 100 * rate),
                  hoverinfo = "text", marker = list(color = ~rate, colorscale = HOT, cmin = 0, cmax = max(g$rate), size = 6, opacity = 0.85,
                                                    colorbar = list(title = list(text = "hit rate", side = "top"), thickness = 8, len = 0.45))) |>
      add_markers(data = q4, x = ~tests, y = ~hits, text = ~gene_symbol, hoverinfo = "text",
                  marker = list(color = COL$sig, size = 11, line = list(color = "#fff", width = 1.5))) |>
      add_annotations(data = q4, x = ~log10(tests), y = ~hits, text = ~paste0("<i>", gene_symbol, "</i>"), ax = -45,
                      ay = rep(c(-20, 18, -32, 30, 0), length.out = nrow(q4)), arrowhead = 0, arrowwidth = 1,
                      arrowcolor = COL$ink2, font = list(size = 12)) |>
      finish(showlegend = FALSE, xaxis = axis_style("Phenotypes tested (log scale)", type = "log"),
             yaxis = axis_style("Phenotypes with P < 0.05"))
  })
  output$o_proc_plot <- renderPlotly({
    x <- AN[, .(n = .N, k = sum(p_value < 0.05)), by = procedure_name][n >= 100][, r := k / n][order(r)]
    ci <- wilson(x$k, x$n); x[, `:=`(lo = ci[[1]], hi = ci[[2]])]
    x[, proc := factor(procedure_name, levels = procedure_name)]
    x[, col := ramp_cols(r, c("#0d0887", "#7e03a8", "#cc4778", "#f89441"))]
    plot_ly(x, height = 24 * nrow(x) + 80) |>
      add_segments(x = ~lo, xend = ~hi, y = ~proc, yend = ~proc, line = list(color = "#c9d6f2", width = 2), hoverinfo = "skip") |>
      add_markers(x = ~r, y = ~proc, marker = list(color = x$col, size = 10, line = list(color = "#ffffff", width = 1)),
                  text = ~sprintf("<b>%s</b><br>%.1f%% of %s analyses", procedure_name, 100 * r, fmtn(n)), hoverinfo = "text") |>
      finish(showlegend = FALSE, xaxis = axis_style("Analyses with P < 0.05", tickformat = ".0%"),
             yaxis = axis_style(NULL, showgrid = FALSE),
             shapes = list(list(type = "line", x0 = mean(AN$p_value < 0.05), x1 = mean(AN$p_value < 0.05), y0 = 0, y1 = 1,
                                yref = "paper", line = list(color = COL$sig, dash = "dash", width = 1))))
  })
  bar_rate <- function(x, lev = NULL) {
    if (!is.null(lev)) x[, grp := factor(grp, levels = lev)]
    x[, col := OI[(seq_len(.N) - 1) %% length(OI) + 1]]
    plot_ly(x, x = ~grp, y = ~n, type = "bar", marker = list(color = x$col, line = list(color = "#ffffff", width = 0.7)),
            text = ~sprintf("%s analyses<br>%.1f%% with P < 0.05", fmtn(n), 100 * r), hoverinfo = "text", textposition = "none") |>
      finish(xaxis = axis_style(NULL, showgrid = FALSE), yaxis = axis_style("Analyses"))
  }
  output$o_stage <- renderPlotly(bar_rate(AN[!is.na(life_stage_name), .(n = .N, r = mean(p_value < 0.05)), by = .(grp = life_stage_name)],
                                          c("E9.5", "E12.5", "E15.5", "E18.5", "Early adult", "Middle aged adult", "Late adult")))
  output$o_strain <- renderPlotly(bar_rate(AN[, .(n = .N, r = mean(p_value < 0.05)), by = .(grp = strain_name)][order(-n)]) |>
                                    layout(xaxis = list(tickangle = -30, categoryorder = "total descending")))

  # ---- parameter groups
  GA <- merge(GM, AN, by = "parameter_stable_id", allow.cartesian = TRUE)
  output$g_rate <- renderPlotly({
    x <- GA[, .(n = .N, k = sum(p_value < 0.05)), by = group_name][, r := k / n][order(r)]
    ci <- wilson(x$k, x$n); x[, `:=`(lo = ci[[1]], hi = ci[[2]])]
    x[, req := group_name %in% c("Weight", "Images", "Brain")]
    x[, grp := factor(group_name, levels = group_name)]
    x[, col := OI[(seq_len(.N) - 1) %% length(OI) + 1]]
    plot_ly(x) |>
      add_segments(x = ~lo, xend = ~hi, y = ~grp, yend = ~grp, line = list(color = "#c9d6f2", width = 2), hoverinfo = "skip") |>
      add_markers(x = ~r, y = ~grp, marker = list(color = x$col, size = 11, symbol = ifelse(x$req, "diamond", "circle"),
                                                  line = list(color = "#ffffff", width = 1)),
                  text = ~sprintf("<b>%s</b>%s<br>%.1f%% of %s analyses", group_name, ifelse(req, " (required)", " (added)"), 100 * r, fmtn(n)),
                  hoverinfo = "text") |>
      finish(showlegend = FALSE, xaxis = axis_style("Analyses with P < 0.05", tickformat = ".0%"), yaxis = axis_style(NULL, showgrid = FALSE),
             shapes = list(list(type = "line", x0 = mean(AN$p_value < 0.05), x1 = mean(AN$p_value < 0.05), y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = COL$ink2, dash = "dash", width = 1))),
             annotations = list(list(x = 1, y = 0, xref = "paper", yref = "paper", xanchor = "right", yanchor = "bottom", showarrow = FALSE,
                                     text = sprintf("<span style='color:%s'>◆</span> required  ● added", COL$sig), font = list(size = 11, color = COL$ink2))))
  })
  output$g_overlap <- renderPlotly({
    g <- unique(GM[parameter_stable_id %in% AN$parameter_stable_id, .(parameter_stable_id, group_name)])
    pr <- merge(g, g, by = "parameter_stable_id", allow.cartesian = TRUE)[, .N, by = .(a = group_name.x, b = group_name.y)]
    lev <- g[, .N, by = group_name][order(-N), group_name]
    z <- matrix(NA_real_, length(lev), length(lev), dimnames = list(lev, lev))
    for (i in seq_len(nrow(pr))) z[pr$a[i], pr$b[i]] <- pr$N[i]
    diag(z) <- NA; z[upper.tri(z)] <- NA
    plot_ly(x = lev, y = lev, z = z, type = "heatmap", colorscale = SEQ, xgap = 2, ygap = 2, text = z, texttemplate = "%{text}",
            hovertemplate = "%{y} ∩ %{x}<br>%{z} shared parameters<extra></extra>",
            colorbar = list(title = list(text = "shared", side = "top"), thickness = 10, len = 0.6)) |>
      finish(xaxis = axis_style(NULL, tickangle = -40, showline = FALSE, ticks = "", showgrid = FALSE),
             yaxis = axis_style(NULL, autorange = "reversed", showline = FALSE, ticks = "", showgrid = FALSE))
  })
  output$g_rule <- renderTable({
    r <- unique(GM[group_name == input$g_sel, rule_description])[1]
    n <- uniqueN(GM[group_name == input$g_sel & parameter_stable_id %in% AN$parameter_stable_id, parameter_stable_id])
    data.frame(Item = c("Membership rule", "Parameters in data"), Value = c(r, n))
  }, colnames = FALSE, striped = TRUE, width = "100%")
  output$g_title <- renderText(paste0("Knockouts most affected in ", input$g_sel))
  output$g_top <- renderPlotly({
    x <- GA[group_name == input$g_sel, .(k = sum(p_value < 0.05), n = .N, best = max(nlp(p_value, 30))), by = gene_symbol]
    x <- x[k > 0][order(-k, -best)][1:min(.N, 25)][order(k, best)]
    validate(need(nrow(x) > 0, "No significant knockouts in this group."))
    x[, gene := factor(gene_symbol, levels = gene_symbol)]
    plot_ly(x, height = 22 * nrow(x) + 90, x = ~k, y = ~gene, type = "bar", orientation = "h",
            marker = list(color = ~best, colorscale = HOT, cmin = 0, cmax = 20, line = list(width = 0),
                          colorbar = list(title = list(text = "max −log10 P", side = "top"), thickness = 10, len = 0.5)),
            text = ~sprintf("<b>%s</b><br>%d of %d tests significant<br>strongest −log10 P = %.1f", gene_symbol, k, n, best),
            hoverinfo = "text", textposition = "none") |>
      finish(xaxis = axis_style("Phenotypes with P < 0.05 in this group"), yaxis = axis_style(NULL, showgrid = FALSE))
  })

  # ---- disease & homology
  DA <- EX$disease_all[in_impc_data == 1]
  output$d_top <- renderPlotly({
    x <- DA[, .(n = uniqueN(mgi_gene_id)), by = .(doid, disease_name)][order(-n)][1:15][order(n)]
    x[, d := factor(disease_name, levels = disease_name)]
    x[, col := OI[(seq_len(.N) - 1) %% length(OI) + 1]]
    plot_ly(x, x = ~n, y = ~d, type = "bar", orientation = "h", marker = list(color = x$col, line = list(color = "#ffffff", width = 0.7)),
            text = ~sprintf("<b>%s</b> (%s)<br>%d knockout genes", disease_name, doid, n), hoverinfo = "text", textposition = "none") |>
      finish(xaxis = axis_style("IMPC knockout genes linked"), yaxis = axis_style(NULL, showgrid = FALSE))
  })
  GS <- AN[, .(n = .N, r = mean(p_value < 0.05)), by = mgi_gene_id][n >= 5][, grp := fifelse(mgi_gene_id %in% dis_genes, "Disease-linked", "No disease link")]
  output$d_box <- renderPlotly({
    plot_ly(GS, x = ~grp, y = ~r, type = "box", color = ~grp, colors = c(COL$sig, COL$teal), boxpoints = "outliers",
            marker = list(size = 4), line = list(width = 1.5)) |>
      finish(showlegend = FALSE, xaxis = axis_style(NULL, showgrid = FALSE), yaxis = axis_style("Tests with P < 0.05 per gene", tickformat = ".0%"))
  })
  output$d_test <- renderText({
    w <- suppressWarnings(wilcox.test(r ~ grp, data = GS))
    m <- GS[, .(m = median(r), n = .N), by = grp]
    sprintf("Median hit rate: disease-linked %.1f%% (n = %s) vs. no link %.1f%% (n = %s); two-sided Wilcoxon rank-sum P = %.2g.",
            100 * m[grp == "Disease-linked", m], fmtn(m[grp == "Disease-linked", n]), 100 * m[grp != "Disease-linked", m],
            fmtn(m[grp != "Disease-linked", n]), w$p.value)
  })
  output$h_violin <- renderPlotly({
    h <- EX$homology_all[, ht := factor(sub("ortholog_", "", homology_type), levels = c("one2one", "one2many", "many2many"),
                                        labels = c("One-to-one", "One-to-many", "Many-to-many"))]
    plot_ly(h, x = ~ht, y = ~pct_identity, color = ~species_name, colors = c(COL$ns, COL$sig), type = "violin",
            box = list(visible = TRUE), meanline = list(visible = FALSE), points = FALSE, spanmode = "hard") |>
      layout(violinmode = "group") |>
      finish(xaxis = axis_style(NULL, showgrid = FALSE), yaxis = axis_style("Sequence identity to mouse gene (%)", range = c(0, 100)),
             legend = list(orientation = "h", y = 1.08))
  })
  dchoices <- DA[, .N, by = disease_name][order(-N), disease_name]
  updateSelectizeInput(session, "d_sel", choices = dchoices, selected = dchoices[1], server = TRUE)
  output$d_genes <- renderDT({
    req(input$d_sel)
    g <- unique(DA[disease_name == input$d_sel, .(mgi_gene_id, Gene = gene_symbol, `Human gene` = human_symbol)])
    s <- AN[mgi_gene_id %in% g$mgi_gene_id, .(Tested = .N, `P < 0.05` = sum(p_value < 0.05), `Min P` = fmt_p(min(p_value))), by = mgi_gene_id]
    dt(merge(g, s, by = "mgi_gene_id", all.x = TRUE)[order(-`P < 0.05`)][, mgi_gene_id := NULL], page = 8, dom = "tp")
  })

  # ---- data quality
  Q <- EX$quality
  output$q_log <- renderText(fmtn(Q$log_rows)); output$q_imp <- renderText(Q$import_repairs)
  output$q_dup <- renderText(Q$dup_files);      output$q_rep <- renderText(fmtn(Q$repeats))
  output$q_pid <- renderText(Q$pid_recovered);  output$q_pm  <- renderText(Q$p_missing)
  output$q_fields <- renderPlotly({
    x <- EX$cleaning[, .(n = sum(n)), by = field_name][order(n)]
    x[, f := factor(gsub("_", " ", sub("^lacz_", "lacZ ", field_name)), levels = gsub("_", " ", sub("^lacz_", "lacZ ", field_name)))]
    x[, col := OI[(seq_len(.N) - 1) %% length(OI) + 1]]
    plot_ly(x, height = 22 * nrow(x) + 80, x = ~n, y = ~f, type = "bar", orientation = "h",
            marker = list(color = x$col, line = list(color = "#ffffff", width = 0.7)),
            text = ~fmtn(n), textposition = "outside", textfont = list(size = 11), hovertemplate = "%{y}: %{x:,} values<extra></extra>") |>
      finish(xaxis = axis_style("Values corrected", range = c(0, max(x$n) * 1.15)), yaxis = axis_style(NULL, showgrid = FALSE))
  })
  output$q_tables <- renderPlotly({
    x <- EX$table_rows[order(table_rows)][, t := factor(table_name, levels = table_name)]
    x[, col := OI[(seq_len(.N) - 1) %% length(OI) + 1]]
    plot_ly(x, height = 22 * nrow(x) + 80, x = ~pmax(table_rows, 1), y = ~t, type = "bar", orientation = "h",
            marker = list(color = x$col, line = list(color = "#ffffff", width = 0.7)), hovertemplate = "%{y}: %{x:,} rows<extra></extra>") |>
      finish(xaxis = axis_style("Rows (log scale)", type = "log"),
             yaxis = axis_style(NULL, showgrid = FALSE, tickfont = list(family = "Menlo, monospace", size = 11)))
  })
  output$q_examples <- renderDT({
    x <- copy(EX$cleaning_examples)
    x[, visible := is.na(clean_value) | trimws(raw_value) != clean_value]
    x <- x[order(-visible, -n)]
    dt(x[, .(Field = gsub("_", " ", field_name), `Raw value` = sprintf("\u201c%s\u201d", raw_value),
             `Clean value` = fifelse(is.na(clean_value), "NULL (missing)", clean_value),
             Change = fifelse(visible, "value corrected", "whitespace trimmed"), Rule = rule, Occurrences = n)], page = 10, dom = "ftip")
  })

  # ---- gene comparison workbench
  comp <- reactive({
    ids <- input$compare_genes
    validate(need(length(ids) >= 2, "Select at least two genes to compare."))
    rbindlist(lapply(ids, function(g) {
      x <- gene_results(ds, g)
      x[, mgi_gene_id := g]
      x[, gene_symbol := GENES$gene_symbol[GENES$mgi_gene_id == g][1]]
      x
    }), fill = TRUE)
  })
  comp_summary <- reactive({
    x <- comp()
    validate(need(nrow(x) > 0, "No comparison results available."))
    if (input$compare_level == "group") {
      rows <- rbindlist(lapply(split(x, x$mgi_gene_id), function(g) {
        if (is.null(g$parameter_groups) || all(is.na(g$parameter_groups))) return(data.table())
        gn <- unique(g$gene_symbol)
        members <- unique(unlist(strsplit(g$parameter_groups[!is.na(g$parameter_groups)], ";\\s*", perl = TRUE)))
        rbindlist(lapply(members, function(f) {
          y <- g[grepl(paste0("(^|; )", f, "($|; )"), parameter_groups)]
          data.table(gene_symbol = gn, feature = f, best = max(nlp(y$p_value, 20)),
                     significant = sum(y$p_value < 0.05), tested = nrow(y))
        }))
      }), fill = TRUE)
    } else {
      x[, .(gene_symbol = unique(gene_symbol), best = max(nlp(p_value, 20)),
            significant = sum(p_value < 0.05), tested = .N),
        by = .(mgi_gene_id, feature = fifelse(is.na(procedure_name), "(unknown)", procedure_name))]
    }
  })
  output$kc_n <- renderText(length(input$compare_genes))
  output$kc_features <- renderText({
    s <- comp_summary(); validate(need(nrow(s) > 0, ""))
    uniqueN(s$feature)
  })
  output$kc_shared <- renderText({
    s <- comp_summary(); validate(need(nrow(s) > 0, ""))
    shared <- s[significant > 0, .N, by = feature][N >= 2, feature]
    length(shared)
  })
  output$kc_top <- renderText({
    s <- comp_summary(); validate(need(nrow(s) > 0, ""))
    z <- s[which.max(best)]
    paste0(z$gene_symbol, " / ", z$feature)
  })
  output$p_compare <- renderPlotly({
    s <- comp_summary(); validate(need(nrow(s) > 0, "No comparison results."))
    s <- s[is.finite(best)]
    validate(need(nrow(s) > 0, "No finite comparison scores are available."))
    w <- dcast(s, feature ~ gene_symbol, value.var = "best", fun.aggregate = max)
    tx <- dcast(s, feature ~ gene_symbol, value.var = "significant", fun.aggregate = sum)
    nn <- dcast(s, feature ~ gene_symbol, value.var = "tested", fun.aggregate = sum)
    genes <- setdiff(names(w), "feature")
    validate(need(length(genes) >= 1, "No gene columns are available for the heatmap."))
    for (g in genes) {
      if (!g %in% names(tx)) tx[, (g) := 0]
      if (!g %in% names(nn)) nn[, (g) := 0]
    }
    z0 <- as.matrix(w[, ..genes])
    storage.mode(z0) <- "double"
    z0[is.na(z0) | !is.finite(z0)] <- 0
    ri <- order(rowSums(z0, na.rm = TRUE), decreasing = TRUE)
    z <- z0[ri, , drop = FALSE]
    tm <- as.matrix(tx[match(w$feature[ri], tx$feature), ..genes]); tm[is.na(tm) | !is.finite(tm)] <- 0
    nm <- as.matrix(nn[match(w$feature[ri], nn$feature), ..genes]); nm[is.na(nm) | !is.finite(nm)] <- 0
    storage.mode(tm) <- "integer"
    storage.mode(nm) <- "integer"
    lab <- paste0(tm, "/", nm)
    plot_ly(x = genes, y = w$feature[ri], z = z, type = "heatmap", colorscale = SEQ,
            zmin = 0, zmax = 20, xgap = 2, ygap = 2, text = lab, texttemplate = "%{text}",
            textfont = list(size = 11),
            hovertemplate = "<b>%{x}</b> - %{y}<br>%{text} significant/tested<br>strongest P score = %{z:.1f}<extra></extra>",
            colorbar = list(title = list(text = "-log10 P", side = "top"), thickness = 10, len = 0.5)) |>
      finish(xaxis = axis_style(NULL, tickfont = list(size = 12), showline = FALSE, ticks = ""),
             yaxis = axis_style(NULL, showline = FALSE, ticks = "", autorange = "reversed"))
  })
  output$t_compare <- renderDT({
    s <- comp_summary(); validate(need(nrow(s) > 0, ""))
    dt(s[order(feature, -best)][, .(Feature = feature, Gene = gene_symbol,
                                     `Strongest -log10P` = round(best, 2),
                                     `P < 0.05` = significant, Tested = tested)],
       page = 12, dom = "ftip")
  })
  output$dl_compare <- downloadHandler(
    function() "gene_comparison.csv",
    function(f) fwrite(comp_summary()[, .(gene_symbol, feature, strongest_minus_log10p = round(best, 4),
                                         significant, tested)], f))

  # ---- phenotype enrichment tests
  enrich_summary <- reactive({
    req(input$enrich_gene)
    threshold <- as.numeric(input$enrich_threshold)
    gid <- input$enrich_gene
    gene_name <- GENES[mgi_gene_id == gid, gene_symbol][1]
    groups <- unique(GM$group_name)
    rbindlist(lapply(groups, function(grp) {
      params <- unique(GM[group_name == grp, parameter_stable_id])
      selected <- AN[mgi_gene_id == gid & parameter_stable_id %in% params]
      rest <- AN[mgi_gene_id != gid & parameter_stable_id %in% params]
      tested_selected <- nrow(selected); hits_selected <- sum(selected$p_value < threshold)
      tested_rest <- nrow(rest); hits_rest <- sum(rest$p_value < threshold)
      if (tested_selected == 0 || tested_rest == 0) return(data.table())
      ft <- fisher.test(matrix(c(hits_selected, tested_selected - hits_selected,
                                 hits_rest, tested_rest - hits_rest),
                               nrow = 2, byrow = TRUE), alternative = "two.sided")
      data.table(gene_symbol = gene_name, parameter_group = grp,
                 selected_tested = tested_selected, selected_hits = hits_selected,
                 rest_tested = tested_rest, rest_hits = hits_rest,
                 selected_hit_rate = hits_selected / tested_selected,
                 odds_ratio = unname(ft$estimate), p_value = ft$p.value)
    }), fill = TRUE)[, q_value := p.adjust(p_value, "BH")][order(p_value)]
  })
  output$ke_groups <- renderText({
    x <- enrich_summary(); nrow(x)
  })
  output$ke_nom <- renderText({
    x <- enrich_summary(); sum(x$p_value < 0.05)
  })
  output$ke_fdr <- renderText({
    x <- enrich_summary(); sum(x$q_value < 0.05)
  })
  output$ke_top <- renderText({
    x <- enrich_summary(); validate(need(nrow(x) > 0, ""))
    paste0(x[1, parameter_group], " (P ", fmt_p(x[1, p_value]), ")")
  })
  output$p_enrich <- renderPlotly({
    x <- enrich_summary(); validate(need(nrow(x) > 0, "No enrichment tests available."))
    x[, log2or := log2(pmax(odds_ratio, 1e-6))]
    x[, col := fifelse(q_value < 0.05, COL$sig, fifelse(p_value < 0.05, COL$amber, COL$ns))]
    x[, grp := factor(parameter_group, levels = parameter_group)]
    plot_ly(x, x = ~log2or, y = ~grp, type = "scatter", mode = "markers",
            marker = list(color = ~col, size = 10,
                          line = list(color = "#ffffff", width = 1)),
            text = ~sprintf("<b>%s</b><br>OR %.2f<br>P %s<br>q %s<br>%d/%d selected hits",
                            parameter_group, odds_ratio, fmt_p(p_value), fmt_p(q_value),
                            selected_hits, selected_tested),
            hoverinfo = "text") |>
      finish(showlegend = FALSE,
             xaxis = axis_style("log2 odds ratio"), yaxis = axis_style(NULL, showgrid = FALSE),
             shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = COL$ink2, dash = "dash", width = 1))))
  })
  output$t_enrich <- renderDT({
    x <- enrich_summary(); validate(need(nrow(x) > 0, ""))
    dt(x[, .(Group = parameter_group, `Selected hits/tested` = paste0(selected_hits, "/", selected_tested),
             `Rest hits/tested` = paste0(rest_hits, "/", rest_tested),
             `Selected hit rate` = sprintf("%.1f%%", 100 * selected_hit_rate),
             `Odds ratio` = round(odds_ratio, 3),
             `P value` = fmt_p(p_value), `BH q value` = fmt_p(q_value))],
       page = 12, dom = "ftip")
  })

  # ---- global search: suggestions, filters, statistics and clickable results
  search_results <- reactive({
    q <- trimws(input$global_search %||% "")
    if (!nzchar(q)) return(data.table(Category = character(), Name = character(),
                                      Identifier = character(), Detail = character()))
    pat <- tolower(q)
    dis_count <- EX$disease_all[, .(n_dis = uniqueN(disease_name)), by = mgi_gene_id]
    genes <- merge(GENES[grepl(pat, tolower(gene_symbol), fixed = TRUE) |
                         grepl(pat, tolower(mgi_gene_id), fixed = TRUE),
                         .(Category = "Gene", Name = gene_symbol, Identifier = mgi_gene_id,
                           mgi_gene_id = mgi_gene_id, n_tests = n_tests)],
                   dis_count, by = "mgi_gene_id", all.x = TRUE)
    genes[is.na(n_dis), n_dis := 0]
    genes[, Detail := sprintf("%s analyses · %s diseases", fmtn(n_tests), fmtn(n_dis))]
    genes <- genes[, .(Category, Name, Identifier, Detail)]

    params <- PARAMS[grepl(pat, tolower(parameter_name), fixed = TRUE) |
                     grepl(pat, tolower(parameter_stable_id), fixed = TRUE),
                     .(Category = "Phenotype", Name = parameter_name, Identifier = parameter_stable_id,
                       Detail = sprintf("%s · %s genes", procedure_name, fmtn(n_genes)))]

    dis_all <- EX$disease_all[, .(doid = fifelse(is.na(doid[1]), "", doid[1])), by = disease_name]
    diseases <- dis_all[grepl(pat, tolower(disease_name), fixed = TRUE),
                        .(Category = "Disease", Name = disease_name, Identifier = doid,
                          disease_name = disease_name)]
    dis_genes <- EX$disease_all[, .(n_genes = uniqueN(mgi_gene_id)), by = disease_name]
    diseases <- merge(diseases, dis_genes, by = "disease_name", all.x = TRUE)
    diseases[, Detail := sprintf("%s · %s knockouts",
                                 fifelse(nzchar(Identifier), paste("DOID", Identifier), "No DOID"), fmtn(n_genes))]
    diseases <- diseases[, .(Category, Name, Identifier, Detail)]

    out <- rbindlist(list(genes, params, diseases), use.names = TRUE, fill = TRUE)
    type <- input$search_category %||% "all"
    if (type != "all") {
      keep <- switch(type, gene = "Gene", phenotype = "Phenotype", disease = "Disease", "")
      out <- out[Category == keep]
    }
    out <- head(out, input$search_limit %||% 50)
    if (nrow(out)) {
      out[, (c("Name", "Detail")) := lapply(.SD, function(z) htmltools::htmlEscape(as.character(z))),
          .SDcols = c("Name", "Detail")]
      btn <- function(i, action, label, style = "outline-primary") {
        sprintf(paste0('<button type="button" class="btn btn-%s btn-sm" ',
                       'onclick="Shiny.setInputValue(\'search_go\', {i: %d, action: \'%s\'}, ',
                       '{priority: \'event\'})">%s</button>'), style, i, action, label)
      }
      out[, Action := vapply(seq_len(.N), function(i) {
        if (Category[i] == "Gene")
          paste0(btn(i, "detail", "Detail"), " ", btn(i, "network", "Network", "outline-success"))
        else btn(i, "detail", "Detail")
      }, character(1))]
    } else out[, Action := character()]
    out[]
  })

  output$ks_total <- renderText(fmtn(nrow(search_results())))
  output$ks_genes <- renderText(fmtn(sum(search_results()$Category == "Gene")))
  output$ks_params <- renderText(fmtn(sum(search_results()$Category == "Phenotype")))
  output$ks_diseases <- renderText(fmtn(sum(search_results()$Category == "Disease")))

  output$t_search <- renderDT({
    x <- search_results()
    validate(need(nrow(x) > 0, "Enter a gene, phenotype or disease name, or choose a suggestion."))
    dt(x, page = 10, dom = "tp", escape = FALSE)
  })

  output$dl_search <- downloadHandler(
    filename = function() paste0("impc_search_",
                                 gsub("[^A-Za-z0-9_-]", "_", input$global_search %||% "all"), ".csv"),
    content = function(file) {
      x <- search_results()
      cols <- setdiff(names(x), "Action")
      fwrite(x[, ..cols], file)
    }
  )

  observeEvent(input$search_go, {
    go <- input$search_go
    x <- search_results()
    i <- suppressWarnings(as.integer(go$i))
    if (is.null(x) || is.na(i) || i < 1 || i > nrow(x)) return(invisible(NULL))
    row <- x[i]
    if (identical(go$action, "network")) {
      if (row$Identifier %in% GENES$mgi_gene_id) {
        updateSelectizeInput(session, "network_gene", selected = row$Identifier, server = TRUE)
        showNotification(paste("Network updated for", row$Name), type = "message", duration = 3)
      }
      return(invisible(NULL))
    }
    if (row$Category == "Gene" && row$Identifier %in% GENES$mgi_gene_id) {
      updateSelectizeInput(session, "gene", selected = row$Identifier, server = TRUE)
      nav_select("nav", TABS[["gene"]])
    } else if (row$Category == "Phenotype" && row$Identifier %in% PARAMS$parameter_stable_id) {
      updateSelectizeInput(session, "param", selected = row$Identifier, server = TRUE)
      nav_select("nav", TABS[["phenotype"]])
    } else if (row$Category == "Disease") {
      match_dis <- EX$disease_all[htmltools::htmlEscape(disease_name) == row$Name]
      if (nzchar(row$Identifier)) match_dis <- EX$disease_all[doid == row$Identifier]
      picked <- unique(match_dis$disease_name)[1]
      if (!is.na(picked) && picked %in% dchoices) {
        updateSelectizeInput(session, "d_sel", selected = picked, server = TRUE)
        nav_select("nav", TABS[["disease"]])
      } else {
        showNotification(paste(row$Name, "has no IMPC knockout data in the disease tab."),
                         type = "message", duration = 4)
      }
    }
    invisible(NULL)
  })

  # ---- gene / phenotype / disease network with detail and export ------------
  network_data <- reactive({
    req(input$network_gene)
    x <- gene_results(ds, input$network_gene)
    validate(need(nrow(x) > 0, "No phenotype results for this gene."))
    x <- x[!is.na(p_value)]
    validate(need(nrow(x) > 0, "No P values for this gene."))
    x <- x[order(p_value)][1:min(.N, input$network_limit)]
    x[, score := nlp(p_value, 20)]
    q_col <- if ("q_value_bh" %in% names(x)) x$q_value_bh else rep(NA_real_, nrow(x))
    x[, q_value := q_col]
    g_col <- if ("parameter_groups" %in% names(x)) x$parameter_groups else rep(NA_character_, nrow(x))
    x[, parameter_groups := g_col]

    phen <- x[, {
      k <- which.min(p_value)
      .(p_value = p_value[k], q_value = q_value[k], score = score[k],
        parameter_stable_id = parameter_stable_id[k], procedure_name = procedure_name[k],
        parameter_groups = parameter_groups[k], allele_symbol = allele_symbol[k],
        life_stage_name = life_stage_name[k], strain_name = strain_name[k],
        significant = sum(p_value < 0.05), tested = .N)
    }, by = .(name = parameter_name)]
    setorder(phen, p_value)
    phen[, y := seq_len(.N)]

    dis <- data.table()
    if (isTRUE(input$network_diseases)) {
      d <- gene_disease(ds, input$network_gene)
      if (nrow(d) > 0) {
        d <- head(d[order(-n_mouse_model_genotypes)], 12)
        dis <- d[, .(name = disease_name, doid = fifelse(is.na(doid), "", doid),
                     n_mouse_model_genotypes = n_mouse_model_genotypes)]
        dis[, y := seq_len(.N)]
      }
    }

    gene_name <- GENES[mgi_gene_id == input$network_gene, gene_symbol][1]
    gy <- (max(nrow(phen), nrow(dis), 1) + 1) / 2
    gene_nodes <- data.table(name = gene_name, x = 0, y = gy,
                             detail = sprintf("%s phenotypes shown<br>%s", nrow(phen), input$network_gene))
    phen_nodes <- phen[, .(name = name, x = 1, y = y, score = score,
                           p_value = p_value, q_value = q_value,
                           detail = sprintf(paste0("<b>%s</b><br>Parameter: %s<br>Procedure: %s<br>",
                                                   "Group: %s<br>Allele: %s<br>%s · %s<br>",
                                                   "P = %s · q = %s<br>−log10 P = %.1f<br>",
                                                   "%s/%s analyses P < 0.05"),
                                            name, parameter_stable_id, procedure_name,
                                            ifelse(is.na(parameter_groups) | !nzchar(parameter_groups), "—", parameter_groups),
                                            allele_symbol, life_stage_name, strain_name, fmt_p(p_value),
                                            ifelse(is.na(q_value), "—", fmt_p(q_value)), score,
                                            fmtn(significant), fmtn(tested)))]
    phen_nodes[, size := 8 + 10 * pmin(score / 20, 1)]
    dis_nodes <- if (nrow(dis))
      dis[, .(name = name, x = 2, y = y,
              detail = sprintf("<b>%s</b><br>DOID: %s<br>Mouse model genotypes: %s",
                               name, doid, fmtn(n_mouse_model_genotypes)))]
    else data.table(name = character(), x = numeric(), y = numeric(), detail = character())
    phen_edges <- data.table(from = gene_name, to = phen$name, x0 = 0, x1 = 1, y0 = gy, y1 = phen$y)
    dis_edges <- if (nrow(dis))
      data.table(from = gene_name, to = dis$name, x0 = 0, x1 = 2, y0 = gy, y1 = dis$y)
    else data.table(from = character(), to = character(), x0 = numeric(), x1 = numeric(),
                    y0 = numeric(), y1 = numeric())
    list(gene = gene_name, phen = phen, gene_nodes = gene_nodes, phen_nodes = phen_nodes,
         dis_nodes = dis_nodes, phen_edges = phen_edges, dis_edges = dis_edges)
  })

  output$p_network <- renderPlotly({
    nw <- network_data()
    p <- plot_ly()
    if (nrow(nw$phen_edges))
      p <- p |> add_segments(data = nw$phen_edges, x = ~x0, xend = ~x1, y = ~y0, yend = ~y1,
                             line = list(color = "#c9d3e0", width = 1.2), hoverinfo = "skip")
    if (nrow(nw$dis_edges))
      p <- p |> add_segments(data = nw$dis_edges, x = ~x0, xend = ~x1, y = ~y0, yend = ~y1,
                             line = list(color = COL$sig, width = 1.2, dash = "dot"), hoverinfo = "skip")
    p <- p |> add_markers(data = nw$gene_nodes, x = ~x, y = ~y, text = ~detail,
                          marker = list(size = 20, color = COL$navy),
                          hovertemplate = paste0("<b>", nw$gene, "</b><br>%{text}<extra></extra>"), name = "Gene")
    p <- p |> add_markers(data = nw$phen_nodes, x = ~x, y = ~y, text = ~detail,
                          marker = list(size = ~size, color = ~score, colorscale = SEQ,
                                        cmin = 0, cmax = 20,
                                        line = list(color = "#ffffff", width = 1)),
                          hovertemplate = "%{text}<extra></extra>", name = "Phenotype")
    if (nrow(nw$dis_nodes))
      p <- p |> add_markers(data = nw$dis_nodes, x = ~x, y = ~y, text = ~detail,
                            marker = list(size = 11, color = COL$sig,
                                          line = list(color = "#ffffff", width = 1)),
                            hovertemplate = "%{text}<extra></extra>", name = "Disease")
    ann <- list(list(x = 0, y = nw$gene_nodes$y, text = nw$gene, showarrow = FALSE,
                     xshift = -40, font = list(size = 13, color = COL$ink, family = FONT)),
                list(x = 1, y = 0.3, text = "PHENOTYPES", showarrow = FALSE,
                     font = list(size = 10, color = COL$ink2, family = FONT)))
    if (nrow(nw$dis_nodes))
      ann[[length(ann) + 1]] <- list(x = 2, y = 0.3, text = "DISEASES", showarrow = FALSE,
                                     font = list(size = 10, color = COL$ink2, family = FONT))
    xr <- if (nrow(nw$dis_nodes)) c(-0.35, 2.35) else c(-0.35, 1.35)
    yr <- c(0.5, max(nrow(nw$phen), nrow(nw$dis_nodes), 1) + 0.5)
    p |> finish(showlegend = FALSE,
                xaxis = axis_style(NULL, range = xr, showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
                yaxis = axis_style(NULL, range = yr, showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
                annotations = ann)
  })

  output$t_network <- renderDT({
    nw <- network_data()
    x <- nw$phen[, .(Phenotype = name, `Parameter ID` = parameter_stable_id,
                     Procedure = procedure_name,
                     `Parameter group` = fifelse(is.na(parameter_groups) | !nzchar(parameter_groups),
                                                 "—", parameter_groups),
                     Allele = allele_symbol, `Life stage` = life_stage_name, Strain = strain_name,
                     P = vapply(p_value, fmt_p, character(1)),
                     `BH q` = vapply(q_value, function(z) if (is.na(z)) "—" else fmt_p(z), character(1)),
                     `−log10 P` = sprintf("%.1f", score), Hits = significant, Tested = tested)]
    dt(x, page = 8, dom = "ftp")
  })

  output$dl_network <- downloadHandler(
    filename = function() paste0("impc_network_", input$network_gene %||% "gene", ".csv"),
    content = function(file) {
      nw <- network_data()
      edges <- rbindlist(list(nw$phen_edges, nw$dis_edges), use.names = TRUE, fill = TRUE)
      clean <- function(z) gsub("<br>", "; ", z, fixed = TRUE)
      en <- data.table(Element = "edge", Source = edges$from, Target = edges$to,
                       Name = edges$to, X = edges$x0, Y = edges$y0, Score = NA_real_,
                       P = NA_real_, Q = NA_real_, Detail = NA_character_)
      gn <- data.table(Element = "node", Source = NA_character_, Target = NA_character_,
                       Name = nw$gene_nodes$name, X = nw$gene_nodes$x, Y = nw$gene_nodes$y,
                       Score = NA_real_, P = NA_real_, Q = NA_real_, Detail = clean(nw$gene_nodes$detail))
      pn <- data.table(Element = "node", Source = NA_character_, Target = NA_character_,
                       Name = nw$phen_nodes$name, X = nw$phen_nodes$x, Y = nw$phen_nodes$y,
                       Score = nw$phen_nodes$score, P = nw$phen_nodes$p_value, Q = nw$phen_nodes$q_value,
                       Detail = clean(nw$phen_nodes$detail))
      dn <- if (nrow(nw$dis_nodes))
        data.table(Element = "node", Source = NA_character_, Target = NA_character_,
                   Name = nw$dis_nodes$name, X = nw$dis_nodes$x, Y = nw$dis_nodes$y,
                   Score = NA_real_, P = NA_real_, Q = NA_real_, Detail = clean(nw$dis_nodes$detail))
      else NULL
      fwrite(rbindlist(list(en, gn, pn, dn), use.names = TRUE, fill = TRUE), file)
    }
  )

  # ---- one-click HTML gene report
  html_escape <- function(z) {
    z <- gsub("&", "&amp;", z, fixed = TRUE)
    z <- gsub("<", "&lt;", z, fixed = TRUE)
    gsub(">", "&gt;", z, fixed = TRUE)
  }
  report_preview_data <- reactive({
    req(input$report_gene)
    x <- gene_results(ds, input$report_gene)
    list(results = x, disease = gene_disease(ds, input$report_gene),
         homology = gene_homology(ds, input$report_gene),
         symbol = GENES[mgi_gene_id == input$report_gene, gene_symbol][1])
  })
  output$report_preview <- renderUI({
    r <- report_preview_data()
    x <- r$results
    tags$div(style = "line-height:1.5",
      tags$h3(r$symbol),
      tags$p(sprintf("%d phenotype tests; %d nominal hits at P < 0.05.",
                     nrow(x), sum(x$p_value < 0.05))),
      tags$p(sprintf("%d disease Ontology links; %d homolog records.",
                     uniqueN(r$disease$doid), nrow(r$homology))),
      tags$strong("Top phenotypes"),
      tags$ul(lapply(head(x[order(p_value), parameter_name], 8), tags$li)))
  })
  output$dl_report <- downloadHandler(
    function() {
      r <- report_preview_data()
      paste0(r$symbol, "_IMPC_report.html")
    },
    function(file) {
      r <- report_preview_data()
      x <- r$results[order(p_value)]
      top <- head(x[, .(Phenotype = parameter_name, Procedure = procedure_name,
                        P = fmt_p(p_value), Life_stage = life_stage_name,
                        Allele = allele_symbol)], 100)
      disease <- r$disease[, .(Human_gene = human_symbol, Disease = disease_name, DOID = doid)]
      homology <- r$homology[, .(Species = species_name, Gene = homolog_gene_name,
                                 Type = homology_type, Identity = pct_identity)]
      rows <- function(df) {
        if (!nrow(df)) return("<tr><td colspan='99'>No records</td></tr>")
        paste0(apply(df, 1, function(row)
          paste0("<tr>", paste0("<td>", html_escape(as.character(row)), "</td>", collapse = ""), "</tr>"),
          simplify = TRUE), collapse = "")
      }
      table_html <- function(df) {
        if (!nrow(df)) return("<p>No records.</p>")
        paste0("<table><thead><tr>",
               paste0("<th>", html_escape(names(df)), "</th>", collapse = ""),
               "</tr></thead><tbody>", rows(df), "</tbody></table>")
      }
      html <- paste0("<!doctype html><html><head><meta charset='utf-8'><title>",
                     html_escape(r$symbol), " IMPC report</title>",
                     "<style>body{font-family:Arial,sans-serif;max-width:1100px;margin:32px auto;color:#14213d}",
                     "h1,h2{color:#0039a6}table{border-collapse:collapse;width:100%;font-size:13px;margin:10px 0 28px}",
                     "th,td{border:1px solid #d9e1ec;padding:7px;text-align:left}th{background:#e8eef7}",
                     ".summary{display:flex;gap:16px}.kpi{border:1px solid #d9e1ec;border-top:4px solid #0039a6;padding:12px;flex:1}",
                     "</style></head><body>",
                     "<h1>", html_escape(r$symbol), " knockout phenotype report</h1>",
                     "<div class='summary'><div class='kpi'><b>", nrow(x),
                     "</b><br>Phenotype tests</div><div class='kpi'><b>", sum(x$p_value < 0.05),
                     "</b><br>P &lt; 0.05 hits</div><div class='kpi'><b>", uniqueN(r$disease$doid),
                     "</b><br>Disease links</div></div>",
                     "<h2>Top phenotype results</h2>", table_html(top),
                     "<h2>Disease Ontology links</h2>", table_html(disease),
                     "<h2>Homology</h2>", table_html(homology),
                     "<p>Generated from the IMPC Group 11 dashboard snapshot.</p></body></html>")
      writeLines(html, file, useBytes = TRUE)
    })

  # ---------------------------------------------------------------- 1 -------
  gres <- reactive({
    req(input$gene)
    x <- gene_results(ds, input$gene)
    if (nzchar(input$grp1)) x <- x[grepl(input$grp1, parameter_groups, fixed = TRUE)]
    if (isTRUE(input$adult1)) x <- x[grepl("adult", life_stage_name)]
    x[procedure_name == "(unknown)", procedure_name := "Not in IMPC reference"]
    x
  })
  is_fdr <- reactive(identical(input$thr1, "fdr"))
  thr1 <- reactive(if (is_fdr()) { x <- gres(); s <- x[q_value_bh < 0.05, max(p_value)]; if (is.finite(s)) s * (1 + 1e-9) else 0 }
                   else as.numeric(input$thr1))   # FDR: equivalent P cut-off for this gene
  sym  <- reactive(GENES[mgi_gene_id == input$gene, gene_symbol][1])
  gdis <- reactive(gene_disease(ds, input$gene))
  output$k_tested <- renderText(nrow(gres()))
  output$k_sig    <- renderText({ x <- gres(); sprintf("%d (%.0f%%)", sum(x$p_value < thr1()), 100 * mean(x$p_value < thr1())) })
  output$k_top    <- renderText({ x <- gres(); if (!nrow(x)) "–" else x[which.min(p_value), parameter_name] })
  output$k_dis    <- renderText(uniqueN(gdis()$doid))

  output$p_gene <- renderPlotly({
    x <- copy(gres()); validate(need(nrow(x) > 0, "No results for this selection."))
    cap <- input$cap1
    x[, score := nlp(p_value, cap)][, sig := p_value < thr1()]
    ord <- x[, .(m = max(score)), by = procedure_name][order(m), procedure_name]
    x[, yi := match(procedure_name, ord)]
    set.seed(1); x[, yj := yi + runif(.N, -0.28, 0.28)]
    x[, tip := sprintf("<b>%s</b><br>%s<br>P = %s<br>%s · %s", parameter_name, procedure_name, fmt_p(p_value),
                       life_stage_name, parameter_stable_id)]
    h <- max(380, 30 * length(ord) + 90)
    plot_ly(height = h) |>
      add_markers(data = x[sig == FALSE], x = ~score, y = ~yj, text = ~tip, hoverinfo = "text",
                  marker = list(color = COL$ns, size = 8, opacity = 0.9), name = "Not significant") |>
      add_markers(data = x[sig == TRUE], x = ~score, y = ~yj, text = ~tip, hoverinfo = "text",
                  marker = list(color = COL$sig, size = 11, line = list(color = "#ffffff", width = 1),
                                symbol = ifelse(x[sig == TRUE]$score >= cap, "triangle-right", "circle")),
                  name = "Significant") |>
      finish(showlegend = FALSE,
             xaxis = axis_style("−log10 P", range = c(-0.4, cap * 1.1)),
             yaxis = axis_style(NULL, tickvals = seq_along(ord), ticktext = ord, range = c(0.4, length(ord) + 0.6),
                                showgrid = TRUE, gridcolor = "#eef2fd"),
             shapes = list(list(type = "line", x0 = nlp(thr1()), x1 = nlp(thr1()), y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = COL$sig, dash = "dash", width = 1.2))))
  })
  output$t_top <- renderDT({
    x <- gres()[p_value < thr1()][order(p_value)][, .(Phenotype = parameter_name, Procedure = procedure_name, P = fmt_p(p_value),
                                                       q = fmt_p(q_value_bh))]
    dt(unique(x, by = c("Phenotype", "Procedure")), page = 12, dom = "tp")
  })
  gcols <- c("parameter_stable_id", "parameter_name", "procedure_name", "parameter_groups", "p_value", "q_value_bh", "life_stage_name", "allele_symbol", "analysis_id")
  output$t_gene_all <- renderDT(dt(gres()[order(p_value), intersect(gcols, names(gres())), with = FALSE]) |> formatSignif(c("p_value", "q_value_bh"), 3))
  output$t_gene_dis <- renderDT(dt(gdis()))
  output$t_gene_hom <- renderDT(dt(gene_homology(ds, input$gene)))
  output$dl1 <- downloadHandler(function() paste0(sym(), "_phenotypes.csv"), function(f) fwrite(gres(), f))

  # ---------------------------------------------------------------- 2 -------
  pres <- reactive({ req(input$param); parameter_results(ds, input$param) })
  thr2 <- reactive(as.numeric(input$thr2))
  output$k2_n    <- renderText(uniqueN(pres()$gene_symbol))
  output$k2_sig  <- renderText(uniqueN(pres()[p_value < thr2(), gene_symbol]))
  output$k2_proc <- renderText(PARAMS[parameter_stable_id == input$param, procedure_name][1])
  output$k2_top  <- renderText({ x <- pres(); if (!nrow(x)) "–" else x[which.min(p_value), gene_symbol] })

  output$p_param <- renderPlotly({
    x <- pres(); validate(need(nrow(x) > 0, "No results for this phenotype."))
    x <- x[order(p_value)][, .SD[1], by = gene_symbol]
    top <- unique(rbind(head(x, input$topn), x[gene_symbol %in% input$hl2]), by = "gene_symbol")[order(-p_value)]
    top[, score := nlp(p_value, 30)]
    top[, col := fifelse(gene_symbol %in% input$hl2, COL$hl, fifelse(p_value < thr2(), COL$sig, COL$ns))]
    top[, tip := sprintf("<b>%s</b><br>P = %s<br>%s · %s", gene_symbol, fmt_p(p_value), allele_symbol, life_stage_name)]
    top[, gene := factor(gene_symbol, levels = gene_symbol)]
    h <- max(380, 18 * nrow(top) + 80)
    p <- plot_ly(height = h)
    for (cc in unique(top$col)) p <- p |>
      add_segments(data = top[col == cc], x = 0, xend = ~score, y = ~gene, yend = ~gene,
                   line = list(color = cc, width = 2), hoverinfo = "skip") |>
      add_markers(data = top[col == cc], x = ~score, y = ~gene, text = ~tip, hoverinfo = "text",
                  marker = list(color = cc, size = 9, line = list(color = "#fff", width = 1)))
    p |>
      finish(showlegend = FALSE, xaxis = axis_style("−log10 P"),
             yaxis = axis_style(NULL, tickfont = list(size = 11, color = COL$ink), showgrid = FALSE,
                                categoryorder = "array", categoryarray = levels(top$gene)),
             shapes = list(list(type = "line", x0 = nlp(thr2()), x1 = nlp(thr2()), y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = COL$sig, dash = "dash", width = 1.2))))
  })
  output$p_param_hist <- renderPlotly({
    x <- pres(); validate(need(nrow(x) > 0, ""))
    hh <- hist(nlp(x$p_value, 30), breaks = 40, plot = FALSE)
    h <- data.table(mid = hh$mids, n = hh$counts)
    h[, col := ramp_cols(mid, c("#440154", "#31688e", "#35b779", "#fde725"))]
    plot_ly(h, x = ~mid, y = ~n, type = "bar", marker = list(color = ~col, line = list(color = "#ffffff", width = 0.6)),
            hovertemplate = "−log10 P %{x:.2f}<br>%{y} knockouts<extra></extra>") |>
      finish(bargap = 0.02, xaxis = axis_style("−log10 P"), yaxis = axis_style("Knockouts"),
             shapes = list(list(type = "line", x0 = nlp(thr2()), x1 = nlp(thr2()), y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = COL$sig, width = 1.5))))
  })
  output$t_param <- renderDT(dt(pres()[order(p_value), .(Gene = gene_symbol, MGI = mgi_gene_id, Allele = allele_symbol,
                                                         P = p_value, `Life stage` = life_stage_name, Strain = strain_name)]) |> formatSignif("P", 3))
  output$dl2 <- downloadHandler(function() paste0(input$param, "_knockouts.csv"), function(f) fwrite(pres(), f))

  # ---------------------------------------------------------------- 3 -------
  long <- reactive(score_long(ds, input$lvl))
  mat <- reactive({
    m <- build_matrix(long(), input$minf, input$topf)
    validate(need(nrow(m) > input$k && ncol(m) >= 2, "Too few genes or features for this setting; lower the minimum."))
    m
  })
  clu <- reactive(cluster_genes(mat(), input$k, input$meth, input$dist %||% "euclidean"))
  output$p_pca <- renderPlotly({
    m <- mat(); r <- clu(); pv <- round(100 * r$pca$sdev^2 / sum(r$pca$sdev^2), 1)
    d <- data.table(gene = rownames(m), PC1 = r$pca$x[, 1], PC2 = r$pca$x[, 2], cl = r$cluster)
    labs <- sprintf("C%d (n = %d)", sort(unique(d$cl)), tabulate(d$cl)[sort(unique(d$cl))])
    d[, cluster := factor(labs[match(cl, sort(unique(cl)))], levels = labs)]
    hl <- d[gene %in% input$hl3]
    plot_ly(d, x = ~PC1, y = ~PC2, color = ~cluster, colors = OI[seq_len(nlevels(d$cluster))], type = "scatter",
            mode = "markers", marker = list(size = 6, opacity = 0.75), text = ~gene,
            hovertemplate = "<b>%{text}</b><br>%{fullData.name}<extra></extra>") |>
      add_trace(data = hl, x = ~PC1, y = ~PC2, type = "scatter", mode = "markers", text = ~gene, inherit = FALSE,
                marker = list(size = 13, color = "rgba(0,0,0,0)", line = list(color = COL$ink, width = 1.6)),
                name = "Gene of interest", hoverinfo = "text") |>
      add_annotations(data = hl[order(PC2)], x = ~PC1, y = ~PC2, text = ~paste0("<i>", gene, "</i>"), showarrow = TRUE,
                      arrowhead = 0, arrowwidth = 1, arrowcolor = COL$ink2, font = list(size = 12, color = COL$ink),
                      ax = rep(c(60, 70, 55, 75, 60), length.out = nrow(hl)), ay = rep(c(25, -25, 45, -45, 0), length.out = nrow(hl)),
                      bgcolor = "rgba(255,255,255,0.9)") |>
      finish(xaxis = axis_style(sprintf("PC1 (%.1f%%)", pv[1])), yaxis = axis_style(sprintf("PC2 (%.1f%%)", pv[2])),
             legend = list(orientation = "h", y = -0.15, font = list(size = 12)))
  })
  output$p_prof <- renderPlotly({
    m <- mat(); cl <- clu()$cluster; ks <- sort(unique(cl))
    z <- t(sapply(ks, function(k) colMeans(m[cl == k, , drop = FALSE] > -log10(0.05)))) * 100
    rownames(z) <- sprintf("C%d (n = %d)", ks, tabulate(cl)[ks])
    fo <- order(-apply(z, 2, max)); z <- z[rev(seq_len(nrow(z))), fo, drop = FALSE]
    plot_ly(x = colnames(z), y = rownames(z), z = z, type = "heatmap", colorscale = SEQ, zmin = 0, zmax = 100, xgap = 2, ygap = 2,
            text = round(z), texttemplate = "%{text}", textfont = list(size = 11),
            hovertemplate = "%{y}<br>%{x}<br>%{z:.0f}% of genes with P < 0.05<extra></extra>",
            colorbar = list(title = list(text = "%", side = "top"), thickness = 10, len = 0.6)) |>
      finish(xaxis = axis_style(NULL, tickangle = -40, showline = FALSE, ticks = ""), yaxis = axis_style(NULL, showline = FALSE, ticks = ""))
  })
  output$p_heat <- renderPlotly({
    m <- mat(); r <- clu(); mm <- m[r$order, , drop = FALSE]
    fo <- if (ncol(mm) > 2) hclust(dist(t(mm)))$order else seq_len(ncol(mm)); mm <- mm[, fo, drop = FALSE]
    plot_ly(x = colnames(mm), y = rownames(mm), z = mm, type = "heatmap", colorscale = SEQ, zmin = 0, zmax = 10,
            hovertemplate = "<b>%{y}</b><br>%{x}<br>−log10 P = %{z:.2f}<extra></extra>",
            colorbar = list(title = list(text = "−log10 P", side = "top"), thickness = 10, len = 0.6)) |>
      finish(xaxis = axis_style(NULL, tickangle = -40, showline = FALSE, ticks = ""),
             yaxis = axis_style(NULL, showticklabels = FALSE, showline = FALSE, ticks = "", autorange = "reversed"))
  })
  output$t_clu <- renderDT({
    m <- mat(); cl <- clu()$cluster
    dt(data.table(Gene = rownames(m), Cluster = paste0("C", cl), `Features tested` = rowSums(m > 0),
                  `Features with P < 0.05` = rowSums(m > -log10(0.05)),
                  `Strongest feature` = colnames(m)[max.col(m, ties.method = "first")])[order(Cluster, -`Features with P < 0.05`)])
  })

  # ---------------------------------------------------------------- 4 -------
  qres <- reactive(rbindlist(lapply(GENES[gene_symbol %in% QUERY_GENES, mgi_gene_id], function(g) gene_results(ds, g)), fill = TRUE))
  qdis <- reactive(rbindlist(lapply(GENES[gene_symbol %in% QUERY_GENES, mgi_gene_id], function(g) gene_disease(ds, g)[, mgi := g]), fill = TRUE))
  output$k4_n     <- renderText(length(QUERY_GENES))
  output$k4_tests <- renderText(nrow(qres()))
  output$k4_sig   <- renderText(sum(qres()$p_value < 0.05))
  output$k4_dis   <- renderText(uniqueN(qdis()$mgi))
  output$p_query <- renderPlotly({
    x <- copy(qres()); validate(need(nrow(x) > 0, "No results."))
    x[procedure_name == "(unknown)", procedure_name := "Not in IMPC reference"]
    s <- x[, .(best = max(nlp(p_value, 20)), sig = sum(p_value < 0.05), n = .N), by = .(gene_symbol, procedure_name)]
    po <- s[, .(m = max(best)), by = procedure_name][order(m), procedure_name]
    go <- s[, .(k = sum(sig)), by = gene_symbol][order(-k), gene_symbol]
    w <- dcast(s, procedure_name ~ gene_symbol, value.var = "best")
    tx <- dcast(s, procedure_name ~ gene_symbol, value.var = "sig")
    nn <- dcast(s, procedure_name ~ gene_symbol, value.var = "n")
    ri <- match(po, w$procedure_name)
    z  <- as.matrix(w[ri, go, with = FALSE]); tm <- as.matrix(tx[ri, go, with = FALSE]); nm <- as.matrix(nn[ri, go, with = FALSE])
    lab <- ifelse(is.na(tm), "", paste0(tm, "/", nm))
    plot_ly(x = go, y = po, z = z, type = "heatmap", colorscale = SEQ, zmin = 0, zmax = 20, xgap = 2, ygap = 2,
            text = lab, texttemplate = "%{text}", textfont = list(size = 11),
            hovertemplate = "<b>%{x}</b> · %{y}<br>%{text} significant<br>strongest −log10 P = %{z:.1f}<extra></extra>",
            colorbar = list(title = list(text = "−log10 P", side = "top"), thickness = 10, len = 0.5)) |>
      finish(xaxis = axis_style(NULL, side = "top", showline = FALSE, ticks = "", tickfont = list(size = 13, color = COL$ink)),
             yaxis = axis_style(NULL, showline = FALSE, ticks = ""))
  })
  output$t_query <- renderDT({
    x <- qres()
    s <- x[, .(Tested = .N, `P < 0.05` = sum(p_value < 0.05), `P < 0.001` = sum(p_value < 0.001),
               `Min P` = fmt_p(min(p_value))), by = .(Gene = gene_symbol)][order(-`P < 0.05`)]
    s[, `DO diseases` := sapply(Gene, function(g) uniqueN(qdis()[mgi == GENES[gene_symbol == g, mgi_gene_id], doid]))]
    dt(s, page = 10, dom = "t")
  })
}

shinyApp(ui, server)
