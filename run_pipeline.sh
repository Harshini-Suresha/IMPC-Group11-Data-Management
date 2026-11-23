#!/usr/bin/env bash
# Runs the full Group 11 pipeline: clean -> build database -> export results -> dump.
set -euo pipefail
cd "$(dirname "$0")"
CNF="${1:-db_config.cnf}"
Rscript R/01_clean_data.R
Rscript R/02_build_database.R "$CNF"
Rscript R/03_export_results.R "$CNF"
Rscript R/04_schema_diagram.R
Rscript R/05_report_figures.R
mysqldump --defaults-extra-file="$CNF" --no-tablespaces --single-transaction --set-gtid-purged=OFF \
  --databases IMPC_Group11 | sed -E 's/DEFINER=`[^`]+`@`[^`]+`//g; s/SQL SECURITY DEFINER/SQL SECURITY INVOKER/' \
  > database/IMPC_Group11_dump.sql
echo "Pipeline complete."
