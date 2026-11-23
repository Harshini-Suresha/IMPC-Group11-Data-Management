-- =============================================================================
-- IMPC_Group11 : normalised (3NF) MySQL schema
-- Group 11 - Data Cleaning and Data Management (7BBG1003)
--
-- Domains
--   A. Experimental core : analysis (fact table) + gene, allele, strain,
--                          life_stage, parameter, procedure, lacZ
--   B. Parameter groups  : parameter_group, parameter_group_member (M:N)
--   C. Human disease     : human_gene, disease, human_gene_disease,
--                          mouse_human_ortholog, disease_model_genotype
--   D. Bespoke homology  : species, ensembl_gene, gene_ensembl_xref,
--                          homology_type, taxon, homology (species-generic)
--   E. Provenance        : duplicate_source_file, data_cleaning_log
-- =============================================================================

SET FOREIGN_KEY_CHECKS = 0;
DROP VIEW  IF EXISTS v_analysis_full, v_gene_disease, v_gene_homology, v_parameter_groups;
DROP TABLE IF EXISTS data_cleaning_log, duplicate_source_file, homology, taxon, homology_type,
  gene_ensembl_xref, ensembl_gene, species, disease_model_genotype, mouse_human_ortholog,
  human_gene_disease, disease, human_gene, analysis_lacz, analysis, lacz_call, lacz_tissue,
  parameter_group_member, parameter_group, parameter_version, parameter, `procedure`,
  allele, gene, strain, life_stage;
SET FOREIGN_KEY_CHECKS = 1;

-- ---------------------------------------------------------------- A. core ----
CREATE TABLE gene (
  mgi_gene_id    VARCHAR(15)  NOT NULL COMMENT 'MGI gene accession, e.g. MGI:1859162',
  gene_symbol    VARCHAR(50)  NOT NULL COMMENT 'Official mouse gene symbol',
  in_impc_data   TINYINT(1)   NOT NULL DEFAULT 0 COMMENT '1 = gene has IMPC analyses in this dataset',
  PRIMARY KEY (mgi_gene_id),
  KEY idx_gene_symbol (gene_symbol),
  CONSTRAINT chk_mgi_format CHECK (mgi_gene_id REGEXP '^MGI:[0-9]+$')
) ENGINE=InnoDB COMMENT='Mouse genes (IMPC + MGI disease report)';

CREATE TABLE allele (
  allele_id      INT UNSIGNED NOT NULL,
  allele_symbol  VARCHAR(60)  NOT NULL,
  mgi_gene_id    VARCHAR(15)  NOT NULL,
  allele_valid   TINYINT(1)   NOT NULL COMMENT '1 = brackets balanced and prefix equals gene symbol',
  PRIMARY KEY (allele_id),
  UNIQUE KEY uq_allele (allele_symbol, mgi_gene_id),
  CONSTRAINT fk_allele_gene FOREIGN KEY (mgi_gene_id) REFERENCES gene (mgi_gene_id)
) ENGINE=InnoDB COMMENT='Knockout alleles';

CREATE TABLE strain (
  strain_id    TINYINT UNSIGNED NOT NULL,
  strain_name  VARCHAR(40) NOT NULL,
  PRIMARY KEY (strain_id), UNIQUE KEY uq_strain (strain_name)
) ENGINE=InnoDB COMMENT='Mouse genetic background (SOP vocabulary)';

CREATE TABLE life_stage (
  life_stage_id   TINYINT UNSIGNED NOT NULL,
  life_stage_name VARCHAR(20) NOT NULL,
  is_embryonic    TINYINT(1)  NOT NULL,
  PRIMARY KEY (life_stage_id), UNIQUE KEY uq_life_stage (life_stage_name)
) ENGINE=InnoDB COMMENT='Developmental stage (SOP vocabulary)';

CREATE TABLE `procedure` (
  procedure_id    SMALLINT UNSIGNED NOT NULL,
  procedure_name  VARCHAR(100) NOT NULL,
  procedure_code  VARCHAR(20)  NULL COMMENT 'IMPReSS code, e.g. OFD',
  description     TEXT NULL,
  PRIMARY KEY (procedure_id), UNIQUE KEY uq_procedure_name (procedure_name)
) ENGINE=InnoDB COMMENT='IMPC phenotyping procedures (IMPReSS)';

CREATE TABLE parameter (
  parameter_stable_id   VARCHAR(30)  NOT NULL COMMENT 'IMPReSS parameter stable id',
  parameter_name        VARCHAR(255) NULL,
  parameter_description TEXT NULL,
  procedure_id          SMALLINT UNSIGNED NULL,
  procedure_link        VARCHAR(40)  NULL COMMENT 'IMPC_reference | inferred_from_procedure_code',
  is_mandatory          TINYINT(1)   NULL,
  in_impc_reference     TINYINT(1)   NOT NULL COMMENT '1 = present in supplied IMPC parameter reference',
  PRIMARY KEY (parameter_stable_id),
  KEY idx_param_name (parameter_name),
  KEY idx_param_proc (procedure_id),
  CONSTRAINT fk_param_proc FOREIGN KEY (procedure_id) REFERENCES `procedure` (procedure_id)
) ENGINE=InnoDB COMMENT='Phenotyping parameters (reference + centre-specific ids used in data)';

CREATE TABLE parameter_version (
  impc_parameter_orig_id INT UNSIGNED NOT NULL COMMENT 'impcParameterOrigId',
  parameter_stable_id    VARCHAR(30)  NOT NULL,
  name                   VARCHAR(255) NOT NULL,
  description            TEXT NULL,
  is_mandatory           TINYINT(1)   NOT NULL,
  PRIMARY KEY (impc_parameter_orig_id),
  KEY idx_pv_param (parameter_stable_id),
  CONSTRAINT fk_pv_param FOREIGN KEY (parameter_stable_id) REFERENCES parameter (parameter_stable_id)
) ENGINE=InnoDB COMMENT='IMPC parameter instances (one stable id may have several orig ids)';

CREATE TABLE lacz_tissue (
  tissue_id    TINYINT UNSIGNED NOT NULL,
  tissue_name  VARCHAR(30) NOT NULL,
  PRIMARY KEY (tissue_id), UNIQUE KEY uq_tissue (tissue_name)
) ENGINE=InnoDB;

CREATE TABLE lacz_call (
  call_id     TINYINT UNSIGNED NOT NULL,
  call_label  VARCHAR(30) NOT NULL,
  PRIMARY KEY (call_id), UNIQUE KEY uq_call (call_label)
) ENGINE=InnoDB COMMENT='lacZ reporter expression vocabulary (SOP)';

CREATE TABLE analysis (
  analysis_id          CHAR(12)     NOT NULL COMMENT 'an + 10 digits',
  mgi_gene_id          VARCHAR(15)  NOT NULL,
  allele_id            INT UNSIGNED NULL,
  strain_id            TINYINT UNSIGNED NOT NULL,
  life_stage_id        TINYINT UNSIGNED NULL,
  parameter_stable_id  VARCHAR(30)  NULL,
  p_value              DOUBLE       NULL,
  p_value_flag         VARCHAR(30)  NULL COMMENT 'missing | out_of_range_[0,1]',
  parameter_id_source  VARCHAR(50)  NULL COMMENT 'how the parameter id was validated/recovered',
  source_file          VARCHAR(40)  NOT NULL,
  import_issue         VARCHAR(255) NULL COMMENT 'structural problems repaired at import',
  analysis_id_note     VARCHAR(80)  NULL,
  content_duplicate_of CHAR(12)     NULL COMMENT 'first analysis with identical gene/allele/strain/stage/parameter/p-value',
  PRIMARY KEY (analysis_id),
  KEY idx_an_dup (content_duplicate_of),
  KEY idx_an_gene (mgi_gene_id),
  KEY idx_an_param (parameter_stable_id),
  KEY idx_an_param_p (parameter_stable_id, p_value),
  KEY idx_an_gene_p (mgi_gene_id, p_value),
  CONSTRAINT fk_an_gene   FOREIGN KEY (mgi_gene_id) REFERENCES gene (mgi_gene_id),
  CONSTRAINT fk_an_allele FOREIGN KEY (allele_id) REFERENCES allele (allele_id),
  CONSTRAINT fk_an_strain FOREIGN KEY (strain_id) REFERENCES strain (strain_id),
  CONSTRAINT fk_an_stage  FOREIGN KEY (life_stage_id) REFERENCES life_stage (life_stage_id),
  CONSTRAINT fk_an_param  FOREIGN KEY (parameter_stable_id) REFERENCES parameter (parameter_stable_id),
  CONSTRAINT fk_an_dup    FOREIGN KEY (content_duplicate_of) REFERENCES analysis (analysis_id),
  CONSTRAINT chk_an_id    CHECK (analysis_id REGEXP '^an[0-9]{10}$'),
  CONSTRAINT chk_p_nonneg CHECK (p_value IS NULL OR p_value >= 0)
) ENGINE=InnoDB COMMENT='One IMPC statistical analysis (gene knockout x parameter)';

CREATE TABLE analysis_lacz (
  analysis_id  CHAR(12) NOT NULL,
  tissue_id    TINYINT UNSIGNED NOT NULL,
  call_id      TINYINT UNSIGNED NOT NULL,
  PRIMARY KEY (analysis_id, tissue_id),
  KEY idx_lacz_tissue_call (tissue_id, call_id),
  CONSTRAINT fk_lz_an     FOREIGN KEY (analysis_id) REFERENCES analysis (analysis_id) ON DELETE CASCADE,
  CONSTRAINT fk_lz_tissue FOREIGN KEY (tissue_id) REFERENCES lacz_tissue (tissue_id),
  CONSTRAINT fk_lz_call   FOREIGN KEY (call_id) REFERENCES lacz_call (call_id)
) ENGINE=InnoDB COMMENT='lacZ expression per analysis and tissue (long format)';

-- ------------------------------------------------------ B. parameter groups ---
CREATE TABLE parameter_group (
  group_id          SMALLINT UNSIGNED NOT NULL,
  group_name        VARCHAR(60)  NOT NULL,
  group_description VARCHAR(255) NULL,
  rule_description  TEXT NULL COMMENT 'how membership was assigned (reproducible rule)',
  PRIMARY KEY (group_id), UNIQUE KEY uq_group_name (group_name)
) ENGINE=InnoDB;

CREATE TABLE parameter_group_member (
  group_id             SMALLINT UNSIGNED NOT NULL,
  parameter_stable_id  VARCHAR(30) NOT NULL,
  PRIMARY KEY (group_id, parameter_stable_id),
  KEY idx_pgm_param (parameter_stable_id),
  CONSTRAINT fk_pgm_group FOREIGN KEY (group_id) REFERENCES parameter_group (group_id) ON DELETE CASCADE,
  CONSTRAINT fk_pgm_param FOREIGN KEY (parameter_stable_id) REFERENCES parameter (parameter_stable_id)
) ENGINE=InnoDB COMMENT='Many-to-many: a parameter can belong to several groups';

-- ------------------------------------------------------------ C. disease -----
CREATE TABLE human_gene (
  hgnc_id       VARCHAR(15)  NOT NULL,
  human_symbol  VARCHAR(30)  NOT NULL,
  human_name    VARCHAR(255) NULL,
  PRIMARY KEY (hgnc_id), KEY idx_hs_symbol (human_symbol)
) ENGINE=InnoDB;

CREATE TABLE disease (
  doid          VARCHAR(20)  NOT NULL COMMENT 'Disease Ontology id',
  disease_name  VARCHAR(255) NOT NULL,
  PRIMARY KEY (doid), KEY idx_disease_name (disease_name)
) ENGINE=InnoDB;

CREATE TABLE human_gene_disease (
  hgnc_id  VARCHAR(15) NOT NULL,
  doid     VARCHAR(20) NOT NULL,
  PRIMARY KEY (hgnc_id, doid), KEY idx_hgd_doid (doid),
  CONSTRAINT fk_hgd_gene FOREIGN KEY (hgnc_id) REFERENCES human_gene (hgnc_id),
  CONSTRAINT fk_hgd_dis  FOREIGN KEY (doid) REFERENCES disease (doid)
) ENGINE=InnoDB;

CREATE TABLE mouse_human_ortholog (
  mgi_gene_id  VARCHAR(15) NOT NULL,
  hgnc_id      VARCHAR(15) NOT NULL,
  PRIMARY KEY (mgi_gene_id, hgnc_id), KEY idx_mho_hgnc (hgnc_id),
  CONSTRAINT fk_mho_mouse FOREIGN KEY (mgi_gene_id) REFERENCES gene (mgi_gene_id),
  CONSTRAINT fk_mho_human FOREIGN KEY (hgnc_id) REFERENCES human_gene (hgnc_id)
) ENGINE=InnoDB COMMENT='One-to-one mouse/human orthologs from the MGI DO report';

CREATE TABLE disease_model_genotype (
  hgnc_id          VARCHAR(15) NOT NULL,
  doid             VARCHAR(20) NOT NULL,
  mgi_genotype_id  VARCHAR(15) NOT NULL COMMENT 'MGI genotype modelling the disease',
  PRIMARY KEY (hgnc_id, doid, mgi_genotype_id),
  KEY idx_dmg_doid (doid),
  CONSTRAINT fk_dmg_hgd FOREIGN KEY (hgnc_id, doid) REFERENCES human_gene_disease (hgnc_id, doid)
) ENGINE=InnoDB;

-- ------------------------------------------------- D. bespoke : homology -----
CREATE TABLE species (
  species_id           TINYINT UNSIGNED NOT NULL,
  species_name         VARCHAR(50) NOT NULL,
  ensembl_gene_prefix  VARCHAR(20) NULL,
  PRIMARY KEY (species_id), UNIQUE KEY uq_species (species_name)
) ENGINE=InnoDB COMMENT='Extensible: add rows for new species';

CREATE TABLE ensembl_gene (
  ensembl_gene_id  VARCHAR(25) NOT NULL,
  species_id       TINYINT UNSIGNED NOT NULL,
  gene_name        VARCHAR(60) NULL,
  PRIMARY KEY (ensembl_gene_id), KEY idx_eg_name (gene_name), KEY idx_eg_species (species_id),
  CONSTRAINT fk_eg_species FOREIGN KEY (species_id) REFERENCES species (species_id)
) ENGINE=InnoDB;

CREATE TABLE gene_ensembl_xref (
  mgi_gene_id      VARCHAR(15) NOT NULL,
  ensembl_gene_id  VARCHAR(25) NOT NULL,
  match_method     VARCHAR(30) NOT NULL,
  PRIMARY KEY (mgi_gene_id, ensembl_gene_id), KEY idx_gex_ens (ensembl_gene_id),
  CONSTRAINT fk_gex_gene FOREIGN KEY (mgi_gene_id) REFERENCES gene (mgi_gene_id),
  CONSTRAINT fk_gex_ens  FOREIGN KEY (ensembl_gene_id) REFERENCES ensembl_gene (ensembl_gene_id)
) ENGINE=InnoDB COMMENT='Links MGI genes to mouse Ensembl genes';

CREATE TABLE homology_type (
  homology_type_id  TINYINT UNSIGNED NOT NULL,
  homology_type     VARCHAR(40) NOT NULL,
  PRIMARY KEY (homology_type_id), UNIQUE KEY uq_ht (homology_type)
) ENGINE=InnoDB;

CREATE TABLE taxon (
  taxon_id    TINYINT UNSIGNED NOT NULL,
  taxon_name  VARCHAR(50) NOT NULL,
  PRIMARY KEY (taxon_id), UNIQUE KEY uq_taxon (taxon_name)
) ENGINE=InnoDB COMMENT='Last common ancestor clades';

CREATE TABLE homology (
  mouse_ensembl_gene_id    VARCHAR(25) NOT NULL,
  homolog_ensembl_gene_id  VARCHAR(25) NOT NULL,
  homology_type_id         TINYINT UNSIGNED NOT NULL,
  lca_taxon_id             TINYINT UNSIGNED NOT NULL,
  pct_identity             DECIMAL(7,4) NULL COMMENT '% of target gene identical to query (mouse) gene',
  PRIMARY KEY (mouse_ensembl_gene_id, homolog_ensembl_gene_id),
  KEY idx_hom_target (homolog_ensembl_gene_id),
  CONSTRAINT fk_hom_mouse  FOREIGN KEY (mouse_ensembl_gene_id) REFERENCES ensembl_gene (ensembl_gene_id),
  CONSTRAINT fk_hom_target FOREIGN KEY (homolog_ensembl_gene_id) REFERENCES ensembl_gene (ensembl_gene_id),
  CONSTRAINT fk_hom_type   FOREIGN KEY (homology_type_id) REFERENCES homology_type (homology_type_id),
  CONSTRAINT fk_hom_taxon  FOREIGN KEY (lca_taxon_id) REFERENCES taxon (taxon_id)
) ENGINE=InnoDB COMMENT='Ensembl (BioMart) homology, one row per mouse gene x homolog';

-- --------------------------------------------------------- E. provenance -----
CREATE TABLE duplicate_source_file (
  source_file               VARCHAR(40) NOT NULL,
  analysis_id               CHAR(12)    NOT NULL,
  identical_after_cleaning  TINYINT(1)  NOT NULL,
  note                      VARCHAR(255) NULL,
  PRIMARY KEY (source_file),
  CONSTRAINT fk_dup_an FOREIGN KEY (analysis_id) REFERENCES analysis (analysis_id)
) ENGINE=InnoDB COMMENT='Source files that duplicated an existing analysis';

CREATE TABLE data_cleaning_log (
  log_id       INT UNSIGNED NOT NULL,
  analysis_id  CHAR(12)     NOT NULL,
  field_name   VARCHAR(40)  NOT NULL,
  raw_value    VARCHAR(255) NULL,
  clean_value  VARCHAR(255) NULL,
  rule         VARCHAR(80)  NOT NULL,
  PRIMARY KEY (log_id),
  KEY idx_log_an (analysis_id), KEY idx_log_field (field_name)
) ENGINE=InnoDB COMMENT='Audit trail: every value changed by the cleaning pipeline';
