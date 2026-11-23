-- =============================================================================
-- IMPC_Group11 : queries for the collaborator's genotypes of interest
-- Genes supplied in group11_query_genes.csv: Cog2, Myof, Lpcat2b, Ifng, Tspyl2
-- Run:  mysql --defaults-extra-file=db_config.cnf IMPC_Group11 < sql/03_collaborator_queries.sql
-- To query other genes, edit the list in the first statement only.
-- =============================================================================
USE IMPC_Group11;

DROP TEMPORARY TABLE IF EXISTS query_genes;
CREATE TEMPORARY TABLE query_genes (gene_symbol VARCHAR(50) PRIMARY KEY);
INSERT INTO query_genes VALUES ('Cog2'), ('Myof'), ('Lpcat2b'), ('Ifng'), ('Tspyl2');

-- Q0. Are the genes present? (gene table, alleles, number of analyses)
SELECT q.gene_symbol, g.mgi_gene_id,
       GROUP_CONCAT(DISTINCT al.allele_symbol SEPARATOR ', ') AS alleles,
       COUNT(a.analysis_id) AS n_analyses
FROM query_genes q
LEFT JOIN gene g      ON g.gene_symbol = q.gene_symbol
LEFT JOIN analysis a  ON a.mgi_gene_id = g.mgi_gene_id
LEFT JOIN allele al   ON al.allele_id = a.allele_id
GROUP BY q.gene_symbol, g.mgi_gene_id;

-- Q1. ALL information per analysis: parameter, parameter group(s), procedure,
--     p-value, life stage, strain, allele, and whether the gene is disease-linked.
SELECT g.gene_symbol, g.mgi_gene_id, a.analysis_id, al.allele_symbol, s.strain_name,
       ls.life_stage_name,
       a.parameter_stable_id, p.parameter_name,
       GROUP_CONCAT(DISTINCT pg.group_name ORDER BY pg.group_name SEPARATOR '; ') AS parameter_groups,
       pr.procedure_name, p.is_mandatory,
       a.p_value, a.content_duplicate_of,
       CASE WHEN a.p_value < 0.05 THEN 'significant' ELSE 'not significant' END AS significance_0_05,
       EXISTS (SELECT 1 FROM mouse_human_ortholog o JOIN human_gene_disease hgd ON hgd.hgnc_id = o.hgnc_id
               WHERE o.mgi_gene_id = g.mgi_gene_id) AS linked_to_disease
FROM query_genes q
JOIN gene g       ON g.gene_symbol = q.gene_symbol
JOIN analysis a   ON a.mgi_gene_id = g.mgi_gene_id
LEFT JOIN allele al     ON al.allele_id = a.allele_id
JOIN strain s           ON s.strain_id = a.strain_id
LEFT JOIN life_stage ls ON ls.life_stage_id = a.life_stage_id
LEFT JOIN parameter p   ON p.parameter_stable_id = a.parameter_stable_id
LEFT JOIN `procedure` pr ON pr.procedure_id = p.procedure_id
LEFT JOIN parameter_group_member m ON m.parameter_stable_id = a.parameter_stable_id
LEFT JOIN parameter_group pg ON pg.group_id = m.group_id
GROUP BY a.analysis_id
ORDER BY g.gene_symbol, a.p_value;

-- Q2. Significant phenotypes only (p < 0.05), most significant first
--     (content-identical repeat analyses are collapsed)
SELECT g.gene_symbol, p.parameter_name, a.parameter_stable_id, pr.procedure_name,
       ls.life_stage_name, a.p_value
FROM query_genes q
JOIN gene g ON g.gene_symbol = q.gene_symbol
JOIN analysis a ON a.mgi_gene_id = g.mgi_gene_id
LEFT JOIN parameter p ON p.parameter_stable_id = a.parameter_stable_id
LEFT JOIN `procedure` pr ON pr.procedure_id = p.procedure_id
LEFT JOIN life_stage ls ON ls.life_stage_id = a.life_stage_id
WHERE a.p_value < 0.05 AND a.content_duplicate_of IS NULL
ORDER BY g.gene_symbol, a.p_value;

-- Q3. Summary per gene: tests, significant tests, best p-value, procedures hit
--     (unique analyses, i.e. content-identical repeats excluded)
SELECT g.gene_symbol,
       COUNT(*) AS n_tests,
       SUM(a.p_value < 0.05)  AS n_sig_0_05,
       SUM(a.p_value < 0.001) AS n_sig_0_001,
       MIN(a.p_value) AS min_p,
       COUNT(DISTINCT CASE WHEN a.p_value < 0.05 THEN pr.procedure_name END) AS n_procedures_with_hits
FROM query_genes q
JOIN gene g ON g.gene_symbol = q.gene_symbol
JOIN analysis a ON a.mgi_gene_id = g.mgi_gene_id
LEFT JOIN parameter p ON p.parameter_stable_id = a.parameter_stable_id
LEFT JOIN `procedure` pr ON pr.procedure_id = p.procedure_id
WHERE a.content_duplicate_of IS NULL
GROUP BY g.gene_symbol;

-- Q4. Significant hits per parameter group (a parameter may sit in several groups)
SELECT g.gene_symbol, pg.group_name, COUNT(DISTINCT a.analysis_id) AS n_tests,
       SUM(a.p_value < 0.05) AS n_significant, MIN(a.p_value) AS min_p
FROM query_genes q
JOIN gene g ON g.gene_symbol = q.gene_symbol
JOIN analysis a ON a.mgi_gene_id = g.mgi_gene_id
JOIN parameter_group_member m ON m.parameter_stable_id = a.parameter_stable_id
JOIN parameter_group pg ON pg.group_id = m.group_id
WHERE a.content_duplicate_of IS NULL
GROUP BY g.gene_symbol, pg.group_name
ORDER BY g.gene_symbol, n_significant DESC;

-- Q5. Human disease links (Disease Ontology) through one-to-one human orthologs
SELECT g.gene_symbol, h.human_symbol, h.hgnc_id, d.doid, d.disease_name,
       COUNT(dmg.mgi_genotype_id) AS n_mouse_model_genotypes
FROM query_genes q
JOIN gene g ON g.gene_symbol = q.gene_symbol
JOIN mouse_human_ortholog o ON o.mgi_gene_id = g.mgi_gene_id
JOIN human_gene h ON h.hgnc_id = o.hgnc_id
JOIN human_gene_disease hgd ON hgd.hgnc_id = h.hgnc_id
JOIN disease d ON d.doid = hgd.doid
LEFT JOIN disease_model_genotype dmg ON dmg.hgnc_id = hgd.hgnc_id AND dmg.doid = hgd.doid
GROUP BY g.gene_symbol, h.human_symbol, h.hgnc_id, d.doid, d.disease_name
ORDER BY g.gene_symbol, d.disease_name;

-- Q6. Bespoke task - homology of the genes (human and vervet-AGM homologs)
SELECT g.gene_symbol, x.ensembl_gene_id AS mouse_ensembl_gene_id, sp.species_name,
       hm.homolog_ensembl_gene_id, eg.gene_name AS homolog_gene_name,
       ht.homology_type, tx.taxon_name AS last_common_ancestor, hm.pct_identity
FROM query_genes q
JOIN gene g ON g.gene_symbol = q.gene_symbol
JOIN gene_ensembl_xref x ON x.mgi_gene_id = g.mgi_gene_id
JOIN homology hm ON hm.mouse_ensembl_gene_id = x.ensembl_gene_id
JOIN ensembl_gene eg ON eg.ensembl_gene_id = hm.homolog_ensembl_gene_id
JOIN species sp ON sp.species_id = eg.species_id
JOIN homology_type ht ON ht.homology_type_id = hm.homology_type_id
JOIN taxon tx ON tx.taxon_id = hm.lca_taxon_id
ORDER BY g.gene_symbol, sp.species_name, hm.pct_identity DESC;

-- Q7. Bespoke task - are the genes in the homology list at all? (overlap check)
SELECT q.gene_symbol,
       (SELECT COUNT(*) FROM gene_ensembl_xref x JOIN gene g ON g.mgi_gene_id = x.mgi_gene_id
         WHERE g.gene_symbol = q.gene_symbol) AS in_homology_list,
       (SELECT COUNT(*) FROM v_gene_homology v WHERE v.gene_symbol = q.gene_symbol) AS n_homologs
FROM query_genes q;

-- Q8. Data provenance: values corrected by the cleaning pipeline for these genes
SELECT g.gene_symbol, l.field_name, COUNT(*) AS n_values_corrected
FROM query_genes q
JOIN gene g ON g.gene_symbol = q.gene_symbol
JOIN analysis a ON a.mgi_gene_id = g.mgi_gene_id
JOIN data_cleaning_log l ON l.analysis_id = a.analysis_id
GROUP BY g.gene_symbol, l.field_name
ORDER BY g.gene_symbol, n_values_corrected DESC;

-- Q9. Bespoke task - overall overlap between the homology gene list and IMPC genes
SELECT COUNT(DISTINCT eg.ensembl_gene_id) AS mouse_genes_with_homologs,
       COUNT(DISTINCT x.ensembl_gene_id)  AS linked_to_mgi_gene,
       COUNT(DISTINCT CASE WHEN g.in_impc_data = 1 THEN x.ensembl_gene_id END) AS with_impc_phenotype_data
FROM ensembl_gene eg
LEFT JOIN gene_ensembl_xref x ON x.ensembl_gene_id = eg.ensembl_gene_id
LEFT JOIN gene g ON g.mgi_gene_id = x.mgi_gene_id
WHERE eg.species_id = (SELECT species_id FROM species WHERE species_name = 'Mouse');

-- Q10. Multiple-testing correction: phenotypes of the genes of interest that remain
--      significant after Benjamini-Hochberg FDR control (q < 0.05) across all analyses
SELECT g.gene_symbol, p.parameter_name, pr.procedure_name, f.p_value, f.q_value_bh
FROM query_genes q
JOIN gene g ON g.gene_symbol = q.gene_symbol
JOIN v_analysis_fdr f ON f.mgi_gene_id = g.mgi_gene_id
LEFT JOIN parameter p ON p.parameter_stable_id = f.parameter_stable_id
LEFT JOIN `procedure` pr ON pr.procedure_id = p.procedure_id
WHERE f.q_value_bh < 0.05
ORDER BY g.gene_symbol, f.q_value_bh;
