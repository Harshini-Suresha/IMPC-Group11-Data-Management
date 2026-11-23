GROUP 11 - DATA CLEANING AND DATA MANAGEMENT

FINAL SUBMISSION CONTENTS

Core datasets:
- parameter_description_clean.rds: cleaned parameter reference data.
- procedure_data.rds: procedure reference data.
- raw_records_imported.rds: imported experimental key-value records.
- source_file_list.rds: source filenames corresponding to imported records.
- experimental_metadata_reconstructed.rds: reconstructed experimental metadata.

Validation folder:
- Contains available structural, reference, metadata and quality-control reports.

Important data handling decisions:
- Original source CSV files were retained unchanged.
- Exact duplicate reference rows were removed from the cleaned parameter reference.
- Missing and ambiguous parameter identifiers were not automatically overwritten.
- Proposed mappings remain proposals pending validation.
- Source-level anomalies and unresolved records are documented in validation reports.

Metadata integrity and file inventory are provided separately.
R environment details are recorded in R_session_information.txt.
