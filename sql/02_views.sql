-- =============================================================================
-- Convenience views for collaborators and the RShiny dashboard
-- =============================================================================

-- Every analysis with all descriptive information resolved
CREATE OR REPLACE VIEW v_analysis_full AS
SELECT a.analysis_id,
       g.mgi_gene_id, g.gene_symbol,
       al.allele_symbol,
       s.strain_name,
       ls.life_stage_name,
       a.parameter_stable_id,
       p.parameter_name,
       pr.procedure_name,
       a.p_value,
       -LOG10(GREATEST(a.p_value, 1e-300))          AS neg_log10_p,
       (a.p_value < 0.05)                            AS significant_0_05,
       a.p_value_flag,
       a.content_duplicate_of
FROM analysis a
JOIN gene g            ON g.mgi_gene_id = a.mgi_gene_id
LEFT JOIN allele al    ON al.allele_id = a.allele_id
JOIN strain s          ON s.strain_id = a.strain_id
LEFT JOIN life_stage ls ON ls.life_stage_id = a.life_stage_id
LEFT JOIN parameter p  ON p.parameter_stable_id = a.parameter_stable_id
LEFT JOIN `procedure` pr ON pr.procedure_id = p.procedure_id;

-- Parameters with their (possibly several) groups
CREATE OR REPLACE VIEW v_parameter_groups AS
SELECT p.parameter_stable_id, p.parameter_name,
       GROUP_CONCAT(pg.group_name ORDER BY pg.group_name SEPARATOR '; ') AS parameter_groups
FROM parameter p
LEFT JOIN parameter_group_member m ON m.parameter_stable_id = p.parameter_stable_id
LEFT JOIN parameter_group pg ON pg.group_id = m.group_id
GROUP BY p.parameter_stable_id, p.parameter_name;

-- Mouse gene -> human ortholog -> Disease Ontology terms
CREATE OR REPLACE VIEW v_gene_disease AS
SELECT g.mgi_gene_id, g.gene_symbol, h.hgnc_id, h.human_symbol, d.doid, d.disease_name,
       (SELECT COUNT(*) FROM disease_model_genotype dmg
         WHERE dmg.hgnc_id = hgd.hgnc_id AND dmg.doid = hgd.doid) AS n_mouse_model_genotypes
FROM gene g
JOIN mouse_human_ortholog o ON o.mgi_gene_id = g.mgi_gene_id
JOIN human_gene h           ON h.hgnc_id = o.hgnc_id
JOIN human_gene_disease hgd ON hgd.hgnc_id = h.hgnc_id
JOIN disease d              ON d.doid = hgd.doid;

-- Mouse gene -> Ensembl homologs in other species (bespoke task)
CREATE OR REPLACE VIEW v_gene_homology AS
SELECT x.mgi_gene_id, g.gene_symbol, hm.mouse_ensembl_gene_id,
       sp.species_name, hm.homolog_ensembl_gene_id, eg.gene_name AS homolog_gene_name,
       ht.homology_type, tx.taxon_name AS last_common_ancestor, hm.pct_identity
FROM homology hm
JOIN ensembl_gene eg  ON eg.ensembl_gene_id = hm.homolog_ensembl_gene_id
JOIN species sp       ON sp.species_id = eg.species_id
JOIN homology_type ht ON ht.homology_type_id = hm.homology_type_id
JOIN taxon tx         ON tx.taxon_id = hm.lca_taxon_id
LEFT JOIN gene_ensembl_xref x ON x.ensembl_gene_id = hm.mouse_ensembl_gene_id
LEFT JOIN gene g      ON g.mgi_gene_id = x.mgi_gene_id;

-- Benjamini-Hochberg false discovery rate (q-values) computed in SQL with window
-- functions: q_i = min_{j >= i} ( p_j * n / rank_j ), over unique analyses.
CREATE OR REPLACE VIEW v_analysis_fdr AS
WITH ranked AS (
  SELECT analysis_id, mgi_gene_id, parameter_stable_id, p_value,
         ROW_NUMBER() OVER (ORDER BY p_value, analysis_id) AS rk,
         COUNT(*)     OVER ()                              AS n
  FROM analysis
  WHERE p_value IS NOT NULL AND p_value <= 1 AND content_duplicate_of IS NULL
), adjusted AS (
  SELECT analysis_id, mgi_gene_id, parameter_stable_id, p_value, rk, LEAST(1, p_value * n / rk) AS raw_q
  FROM ranked
)
SELECT analysis_id, mgi_gene_id, parameter_stable_id, p_value,
       MIN(raw_q) OVER (ORDER BY rk DESC ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS q_value_bh
FROM adjusted;
