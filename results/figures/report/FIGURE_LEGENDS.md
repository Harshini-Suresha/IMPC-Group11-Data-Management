# Figure legends

Figures are produced by `R/05_report_figures.R` from the cleaned tables loaded into `IMPC_Group11`. Each is supplied as a vector PDF and a 600 dpi PNG at 183 mm (double-column) width. Gene symbols are italicised. *P* values are the nominal IMPC statistical outputs, and repeat analyses with identical content (`content_duplicate_of` not NULL) are excluded throughout.

**Figure 1. Data cleaning and database contents.**
(A) Values corrected by the cleaning pipeline, by field (59,519 corrections in total; each is recorded in `data_cleaning_log` with its original value, corrected value and the rule applied).
(B) Share of records that were invalid or missing in the raw files (orange) and after cleaning (blue), by field. "Invalid" means the value did not match the SOP vocabulary, the IMPC parameter reference or the identifier format. Remaining missing values (for example 300 *P* values and 308 life stages) were missing in the source and were not imputed.
(C) Rows per table in the normalised MySQL database (log scale), coloured by domain.

**Figure 2. Phenotype landscape across all knockouts.**
(A) Distribution of *P* values below 1 (13,486 analyses; 13,334 analyses report *P* = 1 exactly and are not shown). The dashed line marks the flat level estimated from *P* ≥ 0.5, which is the level expected without real effects.
(B) Number of phenotypes tested versus number with *P* < 0.05 for each of 2,843 knockout genes (both axes log-scaled). Points are slightly jittered, and the collaborator's genes are shown in orange.
(C) Share of analyses with *P* < 0.05 for each phenotyping procedure with at least 100 analyses. Numbers give the analyses per procedure. "No IMPC procedure" covers centre-specific parameters that are not in the IMPC reference.

**Figure 3. Parameter groupings.** Number of analyses (and distinct parameters, in brackets) in each of the 11 parameter groups: the three required by the brief (Weight, Images, Brain; orange) and the eight added by the group (blue). Membership is many-to-many, through the table `parameter_group_member`; the rule for each group is stored in `parameter_group.rule_description`.

**Figure 4. Phenotypes of the collaborator's genes of interest.**
(A) −log10 *P* for every phenotype tested in each knockout, ordered by procedure (alternate procedures are shown in light and dark grey; *P* < 0.05 in blue; dashed line, *P* = 0.05). Scores are capped at 20 for display: *Ifng* viability outcomes are reported as *P* = 0, below numeric precision. The two strongest distinct phenotypes are labelled for each gene.
(B) Phenotypes with *P* < 0.05 out of those tested, per parameter group. Shading gives the strongest effect (max −log10 *P*); grey cells were not tested.

**Figure 5. Clustering of knockout genes by phenotype profile.**
Each gene tested in at least four parameter groups (n = 1,928) is described by its maximum −log10 *P* per group (capped at 10; untested combinations scored 0). Genes were clustered with Ward hierarchical clustering on Euclidean distance (k = 5).
(A) First two principal components, coloured by cluster; the genes of interest are circled.
(B) Percentage of genes in each cluster with *P* < 0.05 in each parameter group. *Tspyl2*, *Lpcat2b* and *Myof* fall in cluster C2, defined by behavioural and neurological phenotypes (99% of members). *Ifng* falls in C4, defined by embryonic and viability phenotypes.

**Figure 6. Bespoke homology task, disease links and lacZ expression.**
(A) Percentage sequence identity of human and vervet (African green monkey) homologs of mouse genes, by Ensembl orthology type (48,529 mouse–homolog pairs for 20,954 mouse genes; genes without any homolog were excluded). Boxes show the median and interquartile range.
(B) Per-gene share of tests with *P* < 0.05 for knockouts with at least five tests, split by whether the human ortholog has a Disease Ontology (DO) annotation (two-sided Wilcoxon rank-sum test).
(C) lacZ reporter call per knockout gene and tissue (the most frequent call per gene after vocabulary cleaning). Tissues are ordered by the share of genes with expression.

**Figure 7. Data-processing workflow.** The three source streams (experimental analyses, the IMPC parameter/procedure references with the Disease Ontology report, and the bespoke Ensembl homology export), the cleaning and reshaping steps applied to each, and the record counts at each stage, ending in the IMPC_Group11 MySQL database.

**Figure 8. Parameter groups in depth.**
(A) UpSet plot of the 14 most frequent group-membership combinations among parameters in the data. Bars give the number of parameters; filled dots joined by lines show which groups a combination contains; * marks the three groups required by the brief.
(B) Share of analyses with *P* < 0.05 in each group, with Wilson 95% confidence intervals. The dashed line is the overall rate.
(C) Pleiotropy: the number of parameter groups in which each knockout (tested in at least five groups) has at least one phenotype with *P* < 0.05. Values for the genes of interest are listed in the panel.

**Figure 9. Clustered heatmap of knockouts by phenotyping procedure.** The strongest effect (max −log10 *P*, capped at 10) per gene and procedure, for the 60 most broadly tested knockouts plus the genes of interest (marked * and in orange), and procedures tested in at least 40% of these genes. Genes and procedures are clustered with Ward's method on Euclidean distance.

**Figure 10. Ortholog conservation and sources of variation in hit rate.**
(A) Sequence identity of human versus vervet one-to-one orthologs for the same mouse gene (2D histogram; colour shows the number of genes on a log scale). The dashed line marks equal identity.
(B) Share of analyses with *P* < 0.05 by life stage, with Wilson 95% confidence intervals; numbers give the analyses per stage.
(C) As in (B), by genetic background (strains with at least 50 analyses).
