# Set project directory
setwd("/scratch/grp/msc_appbio/Group11_Project/Group11")

# Create output directories
dir.create("cleaned_output", showWarnings = FALSE)
dir.create("cleaned_output/validation", showWarnings = FALSE)

# Identify all CSV files
files <- list.files(
  "group11_data",
  pattern = "\\.csv$",
  full.names = TRUE
)

length(files)

# Function to import one key-value CSV
read_impc_file <- function(file) {
  
  data <- tryCatch({
    
    read.csv(
      file,
      header = FALSE,
      col.names = c("field", "value"),
      colClasses = "character",
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = NULL
    )
    
  }, error = function(e) {
    NULL
  })
  
  data
}

# Import all files
raw_records <- lapply(files, read_impc_file)

# Count successful imports
successful_imports <- sum(!sapply(raw_records, is.null))

successful_imports


# Create file-level summary
file_validation <- data.frame(
  
  file_name = basename(files),
  
  record_count = sapply(raw_records, function(x) {
    if (is.null(x)) NA_integer_ else nrow(x)
  }),
  
  import_status = sapply(raw_records, function(x) {
    if (is.null(x)) "Failed" else "Successful"
  }),
  
  stringsAsFactors = FALSE
)

# Identify files with unexpected record counts
file_validation$record_status <- ifelse(
  is.na(file_validation$record_count),
  "Import failed",
  ifelse(
    file_validation$record_count == 20,
    "Expected",
    "Unexpected record count"
  )
)

# Display record-count distribution
table(file_validation$record_count, useNA = "ifany")

# Save report
write.csv(
  file_validation,
  "cleaned_output/validation/file_validation.csv",
  row.names = FALSE
)

exceptional_files <- file_validation[
  file_validation$record_count != 20 |
    is.na(file_validation$record_count),
]

print(exceptional_files)

for (i in which(
  file_validation$record_count != 20 |
  is.na(file_validation$record_count)
)) {
  
  cat("\nFILE:", basename(files[i]), "\n")
  
  print(raw_records[[i]])
}

duplicate_fields <- lapply(seq_along(raw_records), function(i) {
  
  x <- raw_records[[i]]
  
  if (is.null(x)) {
    return(NULL)
  }
  
  duplicated_names <- unique(
    x$field[duplicated(x$field)]
  )
  
  if (length(duplicated_names) > 0) {
    
    data.frame(
      file_name = basename(files[i]),
      duplicate_field = duplicated_names
    )
    
  } else {
    NULL
  }
})

duplicate_fields <- do.call(
  rbind,
  duplicate_fields
)

if (is.null(duplicate_fields)) {
  
  cat("No duplicate field names detected.\n")
  
} else {
  
  print(duplicate_fields)
  
  write.csv(
    duplicate_fields,
    "cleaned_output/validation/duplicate_fields.csv",
    row.names = FALSE
  )
}

# Extract unique field names from each file
field_structures <- lapply(raw_records, function(x) {
  
  if (is.null(x)) {
    return(character(0))
  }
  
  unique(x$field)
})

# Get all fields present across the dataset
all_fields <- sort(
  unique(unlist(field_structures))
)

print(all_fields)

length(all_fields)

saveRDS(
  raw_records,
  "cleaned_output/raw_records_imported.rds"
)

saveRDS(
  files,
  "cleaned_output/source_file_list.rds"
)




setwd("/scratch/grp/msc_appbio/Group11_Project/Group11")

if (!exists("files") || !exists("raw_records")) {
  stop("The objects 'files' and 'raw_records' are missing. Run the initial import code first.")
}

cat("Total source files:", length(files), "\n")
cat("Total imported records:", length(raw_records), "\n")

exceptional_names <- c(
  "an8028640307.csv",
  "an8097586748.csv",
  "an8174255925.csv",
  "an8586019060.csv"
)

exceptional_inspection <- list()

for (f in exceptional_names) {
  cat("\n")
  cat("====================================================\n")
  cat("FILE:", f, "\n")
  cat("====================================================\n")
  
  path <- file.path("group11_data", f)
  
  if (!file.exists(path)) {
    cat("WARNING: File not found\n")
    next
  }
  
  lines <- readLines(path, warn = FALSE)
  
  exceptional_inspection[[f]] <- data.frame(
    file_name = f,
    physical_line_number = seq_along(lines),
    raw_content = lines,
    stringsAsFactors = FALSE
  )
  
  cat("Physical line count:", length(lines), "\n")
  cat("Original file contents:\n")
  cat(paste(lines, collapse = "\n"), "\n")
}

if (length(exceptional_inspection) > 0) {
  exceptional_report <- do.call(rbind, exceptional_inspection)
  
  write.csv(
    exceptional_report,
    "cleaned_output/validation/exceptional_file_inspection.csv",
    row.names = FALSE,
    na = ""
  )
  
  cat("\nExceptional-file inspection report saved.\n")
}

field_report_list <- lapply(seq_along(raw_records), function(i) {
  x <- raw_records[[i]]
  
  if (is.null(x)) {
    return(NULL)
  }
  
  data.frame(
    file_name = basename(files[i]),
    field_name = x$field,
    stringsAsFactors = FALSE
  )
})

field_report_list <- Filter(
  Negate(is.null),
  field_report_list
)

if (length(field_report_list) == 0) {
  stop("No successfully imported records are available.")
}

field_report <- do.call(rbind, field_report_list)

field_frequency <- as.data.frame(
  table(field_report$field_name),
  stringsAsFactors = FALSE
)

names(field_frequency) <- c(
  "field_name",
  "frequency"
)

field_frequency <- field_frequency[
  order(field_frequency$frequency),
]

rownames(field_frequency) <- NULL

cat("\nFIELD FREQUENCY REPORT:\n")
print(field_frequency)

write.csv(
  field_frequency,
  "cleaned_output/validation/field_frequency.csv",
  row.names = FALSE
)

unusual_fields <- field_frequency[
  grepl(
    '["\n\r]|^_|,|[&@!*%)]',
    field_frequency$field_name
  ),
]

cat("\nUNUSUAL OR POTENTIALLY MALFORMED FIELD LABELS:\n")
print(unusual_fields)

field_presence <- data.frame(
  field_name = sort(unique(field_report$field_name)),
  stringsAsFactors = FALSE
)

field_presence$files_containing_field <- vapply(
  field_presence$field_name,
  function(field) {
    length(unique(
      field_report$file_name[
        field_report$field_name == field
      ]
    ))
  },
  integer(1)
)

field_presence$total_files <- length(files)

field_presence$percentage_of_files <- round(
  field_presence$files_containing_field /
    field_presence$total_files * 100,
  2
)

cat("\nFIELD PRESENCE SUMMARY:\n")
print(field_presence)

write.csv(
  field_presence,
  "cleaned_output/validation/field_presence_summary.csv",
  row.names = FALSE
)

if (exists("file_validation")) {
  cat("\nIMPORT STATUS SUMMARY:\n")
  print(table(
    file_validation$import_status,
    useNA = "ifany"
  ))
  
  cat("\nRECORD COUNT SUMMARY:\n")
  print(table(
    file_validation$record_count,
    useNA = "ifany"
  ))
} else {
  cat("\nfile_validation object not found; import summary skipped.\n")
}

saveRDS(
  exceptional_inspection,
  "cleaned_output/exceptional_inspection.rds"
)

saveRDS(
  field_report,
  "cleaned_output/field_report.rds"
)

saveRDS(
  field_frequency,
  "cleaned_output/field_frequency.rds"
)

saveRDS(
  field_presence,
  "cleaned_output/field_presence.rds"
)

cat("\n")
cat("====================================================\n")
cat("VALIDATION DIAGNOSTICS COMPLETED\n")
cat("====================================================\n")

cat("\nFiles generated:\n")

print(list.files(
  "cleaned_output/validation",
  full.names = TRUE
))

cat("\nAll original source CSV files remain unchanged.\n")
###initial import, structural inspection and validation diagnostics. -- done


setwd("/scratch/grp/msc_appbio/Group11_Project/Group11")

# Load parameter descriptions
parameter_description <- read.delim(
  "IMPC_parameter_description.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Load procedure descriptions
procedure_data <- read.delim(
  "IMPC_procedure.txt",
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Inspect dimensions
cat("Parameter description dimensions:\n")
print(dim(parameter_description))

cat("\nProcedure dimensions:\n")
print(dim(procedure_data))

# Inspect column names
names(parameter_description)
names(procedure_data)

# Inspect initial records
head(parameter_description)
head(procedure_data)


# Missing values in parameter descriptions
colSums(is.na(parameter_description))

# Missing values in procedures
colSums(is.na(procedure_data))

# Duplicate parameter identifiers
sum(duplicated(parameter_description$parameterId))

# Duplicate original procedure parameter IDs
sum(duplicated(procedure_data$impcParameterOrigId))

# Unique parameter identifiers
length(unique(parameter_description$parameterId))

# Unique procedure parameter identifiers
length(unique(procedure_data$impcParameterOrigId))

saveRDS(
  parameter_description,
  "cleaned_output/parameter_description.rds"
)

saveRDS(
  procedure_data,
  "cleaned_output/procedure_data.rds"
)


setwd("/scratch/grp/msc_appbio/Group11_Project/Group11")

# Import parameter descriptions
parameter_description <- read.csv(
  "IMPC_parameter_description.txt",
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)

# Import procedure descriptions
procedure_data <- read.csv(
  "IMPC_procedure.txt",
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)

# Check dimensions
dim(parameter_description)
dim(procedure_data)

# Check column names
names(parameter_description)
names(procedure_data)

# Inspect first records
head(parameter_description)
head(procedure_data)


# Missing values
colSums(is.na(parameter_description))
colSums(is.na(procedure_data))

# Duplicate identifiers
sum(duplicated(parameter_description$parameterId))

sum(duplicated(procedure_data$impcParameterOrigId))

# Unique identifiers
length(unique(parameter_description$parameterId))

length(unique(procedure_data$impcParameterOrigId))



# Missing values
colSums(is.na(parameter_description))
colSums(is.na(procedure_data))

# Duplicate identifiers
sum(duplicated(parameter_description$parameterId))

sum(duplicated(procedure_data$impcParameterOrigId))

# Unique identifiers
length(unique(parameter_description$parameterId))

length(unique(procedure_data$impcParameterOrigId))


saveRDS(
  parameter_description,
  "cleaned_output/parameter_description.rds"
)

saveRDS(
  procedure_data,
  "cleaned_output/procedure_data.rds"
)



# Check whether original parameter IDs are unique
sum(duplicated(parameter_description$impcParameterOrigId))

length(unique(parameter_description$impcParameterOrigId))

# Inspect parameter IDs that occur more than once
parameter_id_counts <- as.data.frame(
  table(parameter_description$parameterId)
)

names(parameter_id_counts) <- c("parameterId", "frequency")

duplicate_parameter_ids <- subset(
  parameter_id_counts,
  frequency > 1
)

head(
  duplicate_parameter_ids[
    order(-duplicate_parameter_ids$frequency),
  ],
  20
)


example_duplicate_id <- duplicate_parameter_ids$parameterId[1]

subset(
  parameter_description,
  parameterId == example_duplicate_id
)

# Check how many parameter IDs appear in both files
sum(
  parameter_description$impcParameterOrigId %in%
    procedure_data$impcParameterOrigId
)

# IDs in parameter descriptions without a procedure match
parameter_without_procedure <- subset(
  parameter_description,
  !impcParameterOrigId %in% procedure_data$impcParameterOrigId
)

# IDs in procedures without a parameter description match
procedure_without_parameter <- subset(
  procedure_data,
  !impcParameterOrigId %in% parameter_description$impcParameterOrigId
)

nrow(parameter_without_procedure)
nrow(procedure_without_parameter)

write.csv(
  duplicate_parameter_ids,
  "cleaned_output/validation/duplicate_parameter_ids.csv",
  row.names = FALSE
)

write.csv(
  parameter_without_procedure,
  "cleaned_output/validation/parameters_without_procedure.csv",
  row.names = FALSE
)

write.csv(
  procedure_without_parameter,
  "cleaned_output/validation/procedures_without_parameter.csv",
  row.names = FALSE
)


#investigating the 14 duplicate id

# Convert identifiers to character for consistent comparison
parameter_description$impcParameterOrigId <-
  as.character(parameter_description$impcParameterOrigId)

procedure_data$impcParameterOrigId <-
  as.character(procedure_data$impcParameterOrigId)

# Identify duplicated original parameter IDs
duplicate_original_ids <- unique(
  parameter_description$impcParameterOrigId[
    duplicated(parameter_description$impcParameterOrigId) |
      duplicated(parameter_description$impcParameterOrigId, fromLast = TRUE)
  ]
)

length(duplicate_original_ids)

# Inspect all records associated with duplicated IDs
duplicate_original_records <- parameter_description[
  parameter_description$impcParameterOrigId %in% duplicate_original_ids,
]

duplicate_original_records[
  order(duplicate_original_records$impcParameterOrigId),
]

# Count distinct versions of each original ID
duplicate_comparison <- aggregate(
  cbind(
    name = as.character(name),
    description = as.character(description),
    parameterId = as.character(parameterId)
  ) ~ impcParameterOrigId,
  data = duplicate_original_records,
  FUN = function(x) length(unique(x))
)

duplicate_comparison

first_duplicate_parameter_id <-
  as.character(duplicate_parameter_ids$parameterId[1])

first_duplicate_parameter_id

subset(
  parameter_description,
  parameterId == first_duplicate_parameter_id
)


# Check leading and trailing whitespace
parameter_description$parameterId[
  grepl("^\\s|\\s$", parameter_description$parameterId)
] |> head(20)

# Count values affected by whitespace
sum(
  parameter_description$parameterId !=
    trimws(parameter_description$parameterId)
)

# Compare duplicate counts before and after trimming
sum(duplicated(parameter_description$parameterId))

sum(duplicated(trimws(parameter_description$parameterId)))

# Inspect the first repeated ID after trimming
parameter_description$parameterId <-
  trimws(parameter_description$parameterId)

parameter_id_counts <- as.data.frame(
  table(parameter_description$parameterId),
  stringsAsFactors = FALSE
)

names(parameter_id_counts) <- c("parameterId", "frequency")

duplicate_parameter_ids <- subset(
  parameter_id_counts,
  frequency > 1
)

head(
  duplicate_parameter_ids[
    order(-duplicate_parameter_ids$frequency),
  ],
  20
)



# Preserve the imported version
saveRDS(
  parameter_description,
  "cleaned_output/parameter_description_before_deduplication.rds"
)

# Identify exact duplicate rows
exact_duplicate_rows <- duplicated(parameter_description)

sum(exact_duplicate_rows)

# Create the deduplicated reference table
parameter_description_clean <- parameter_description[
  !exact_duplicate_rows,
]

# Verify
dim(parameter_description)
dim(parameter_description_clean)

sum(duplicated(parameter_description_clean$impcParameterOrigId))


saveRDS(
  parameter_description_clean,
  "cleaned_output/parameter_description_clean.rds"
)


# Identify all repeated stable parameter IDs
repeated_stable_ids <- duplicate_parameter_ids$parameterId

# Inspect the records for repeated stable IDs
repeated_stable_records <- parameter_description_clean[
  parameter_description_clean$parameterId %in% repeated_stable_ids,
]

# Count distinct original IDs for each stable ID
stable_id_mapping <- aggregate(
  impcParameterOrigId ~ parameterId,
  data = repeated_stable_records,
  FUN = function(x) length(unique(x))
)

names(stable_id_mapping)[2] <- "number_of_original_ids"

# Inspect mappings
head(
  stable_id_mapping[
    order(-stable_id_mapping$number_of_original_ids),
  ],
  20
)

# Save the mapping investigation
write.csv(
  stable_id_mapping,
  "cleaned_output/validation/stable_parameter_id_mapping.csv",
  row.names = FALSE
)


parameter_id_whitespace_log <- data.frame(
  field = "parameterId",
  records_affected = 5324,
  action = "Removed leading/trailing whitespace using trimws()",
  reason = "Standardised identifier formatting",
  stringsAsFactors = FALSE
)

write.csv(
  parameter_id_whitespace_log,
  "cleaned_output/validation/parameter_id_whitespace_log.csv",
  row.names = FALSE
)

# Group records by stable parameter ID and count distinct descriptions
stable_id_description_check <- aggregate(
  cbind(
    name = as.character(name),
    description = as.character(description)
  ) ~ parameterId,
  data = repeated_stable_records,
  FUN = function(x) length(unique(x))
)

names(stable_id_description_check)[2:3] <-
  c("distinct_names", "distinct_descriptions")

# Identify stable IDs with inconsistent descriptions
inconsistent_stable_ids <- subset(
  stable_id_description_check,
  distinct_names > 1 | distinct_descriptions > 1
)

nrow(inconsistent_stable_ids)

head(inconsistent_stable_ids, 20)


# Select the first five inconsistent stable IDs
ids_to_inspect <- inconsistent_stable_ids$parameterId[1:5]

# Retrieve all records associated with these IDs
inconsistent_records <- parameter_description_clean[
  parameter_description_clean$parameterId %in% ids_to_inspect,
  c(
    "impcParameterOrigId",
    "parameterId",
    "name",
    "description"
  )
]

# Display the records
inconsistent_records[
  order(inconsistent_records$parameterId,
        inconsistent_records$impcParameterOrigId),
]


ids_to_inspect_2 <- c(
  "IMPC_EXD_007_001",
  "IMPC_EXD_008_001"
)

subset(
  parameter_description_clean,
  parameterId %in% ids_to_inspect_2,
  select = c(
    impcParameterOrigId,
    parameterId,
    name,
    description
  )
)


# Standardise missing descriptions for validation
parameter_description_clean$description[
  trimws(parameter_description_clean$description) == ""
] <- NA

# Recalculate distinct names and descriptions,
# excluding missing values
stable_id_description_check_corrected <- aggregate(
  cbind(
    name = as.character(name),
    description = as.character(description)
  ) ~ parameterId,
  data = repeated_stable_records,
  FUN = function(x) length(unique(na.omit(x)))
)


repeated_stable_records <- parameter_description_clean[
  parameter_description_clean$parameterId %in%
    duplicate_parameter_ids$parameterId,
]

stable_id_description_check_corrected <- aggregate(
  cbind(
    name = as.character(name),
    description = as.character(description)
  ) ~ parameterId,
  data = repeated_stable_records,
  FUN = function(x) length(unique(na.omit(x)))
)

names(stable_id_description_check_corrected)[2:3] <-
  c("distinct_names", "distinct_descriptions")

inconsistent_stable_ids_corrected <- subset(
  stable_id_description_check_corrected,
  distinct_names > 1 | distinct_descriptions > 1
)

nrow(inconsistent_stable_ids_corrected)

head(inconsistent_stable_ids_corrected, 20)


# Get all 21 IDs requiring investigation
ids_requiring_review <- inconsistent_stable_ids_corrected$parameterId

# Extract their complete records
records_requiring_review <- parameter_description_clean[
  parameter_description_clean$parameterId %in% ids_requiring_review,
  c(
    "impcParameterOrigId",
    "parameterId",
    "name",
    "description"
  )
]

# Sort for easier comparison
records_requiring_review <- records_requiring_review[
  order(
    records_requiring_review$parameterId,
    records_requiring_review$impcParameterOrigId
  ),
]

# Display all records
print(records_requiring_review, row.names = FALSE)


subset(
  parameter_description_clean,
  parameterId == "IMPC_EXD_015_001",
  select = c(
    impcParameterOrigId,
    parameterId,
    name,
    description
  )
)

write.csv(
  records_requiring_review,
  "cleaned_output/validation/stable_id_inconsistency_details.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  inconsistent_stable_ids_corrected,
  "cleaned_output/validation/inconsistent_stable_ids_corrected.csv",
  row.names = FALSE
)

# Save the updated cleaned reference table
saveRDS(
  parameter_description_clean,
  "cleaned_output/parameter_description_clean.rds"
)


# Get all original IDs associated with the 21 flagged stable IDs
ids_requiring_review <- inconsistent_stable_ids_corrected$parameterId

review_original_ids <- parameter_description_clean[
  parameter_description_clean$parameterId %in% ids_requiring_review,
  "impcParameterOrigId"
]

# Match these records to their procedure information
procedure_review <- merge(
  parameter_description_clean[
    parameter_description_clean$parameterId %in% ids_requiring_review,
  ],
  procedure_data,
  by = "impcParameterOrigId",
  all.x = TRUE,
  suffixes = c("_parameter", "_procedure")
)

# Inspect the results
procedure_review <- procedure_review[
  order(procedure_review$parameterId,
        procedure_review$impcParameterOrigId),
]

print(procedure_review, row.names = FALSE)
subset(
  procedure_review,
  parameterId == "IMPC_EXD_015_001"
)

write.csv(
  procedure_review,
  "cleaned_output/validation/parameter_procedure_review.csv",
  row.names = FALSE,
  na = ""
)

saveRDS(
  parameter_description_clean,
  "cleaned_output/parameter_description_clean.rds"
)

saveRDS(
  procedure_data,
  "cleaned_output/procedure_data.rds"
)


reference_validation_summary <- data.frame(
  dataset = c(
    "Parameter description - original",
    "Parameter description - cleaned",
    "Procedure data"
  ),
  records = c(
    nrow(parameter_description),
    nrow(parameter_description_clean),
    nrow(procedure_data)
  ),
  unique_original_ids = c(
    length(unique(parameter_description$impcParameterOrigId)),
    length(unique(parameter_description_clean$impcParameterOrigId)),
    length(unique(procedure_data$impcParameterOrigId))
  ),
  stringsAsFactors = FALSE
)

print(reference_validation_summary)

write.csv(
  reference_validation_summary,
  "cleaned_output/validation/reference_validation_summary.csv",
  row.names = FALSE
)

write.csv(
  inconsistent_stable_ids_corrected,
  "cleaned_output/validation/stable_id_review.csv",
  row.names = FALSE
)


# Load the SOP
sop <- read.csv(
  "IMPC_SOP_2026.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Inspect SOP fields
print(sop)

# Extract unique field names from experimental records
experimental_fields <- sort(unique(
  unlist(lapply(raw_records, function(x) {
    if (is.null(x)) return(NULL)
    x$field
  }))
))

# Compare experimental fields with SOP fields
field_comparison <- data.frame(
  experimental_field = experimental_fields,
  stringsAsFactors = FALSE
)

field_comparison$present_in_sop <-
  field_comparison$experimental_field %in% sop$dataField

print(field_comparison)

# Save comparison
write.csv(
  field_comparison,
  "cleaned_output/validation/experimental_field_sop_comparison.csv",
  row.names = FALSE
)


# Define the experimental fields expected by the SOP
expected_experimental_fields <- c(
  "analysis_id",
  "gene_accession_id",
  "gene_symbol",
  "strain_name",
  "mouse_life_stage",
  "allele_symbol",
  "parameter_stable_id",
  "parameter_name",
  "p_value"
)

# Add individual lacZ fields
lacz_fields <- experimental_fields[
  grepl("^lacz_", experimental_fields)
]

expected_experimental_fields <- c(
  expected_experimental_fields,
  lacz_fields
)

# Compare against actual parsed experimental fields
expected_field_comparison <- data.frame(
  field = expected_experimental_fields,
  present_in_experimental_data =
    expected_experimental_fields %in% experimental_fields
)

print(expected_field_comparison)

# Identify files containing alternative field names
alternative_field_files <- lapply(
  seq_along(raw_records),
  function(i) {
    x <- raw_records[[i]]
    
    if (is.null(x)) return(NULL)
    
    alternative_fields <- intersect(
      x$field,
      c("parameterName", "strainName")
    )
    
    if (length(alternative_fields) == 0) return(NULL)
    
    data.frame(
      file_name = basename(files[i]),
      field = alternative_fields,
      stringsAsFactors = FALSE
    )
  }
)

alternative_field_files <- do.call(
  rbind,
  alternative_field_files
)

print(alternative_field_files)

write.csv(
  alternative_field_files,
  "cleaned_output/validation/alternative_field_names.csv",
  row.names = FALSE
)


field_frequency_all <- sort(
  table(unlist(
    lapply(raw_records, function(x) {
      if (is.null(x)) return(NULL)
      x$field
    })
  )),
  decreasing = TRUE
)

field_frequency_table <- data.frame(
  field = names(field_frequency_all),
  frequency = as.integer(field_frequency_all),
  row.names = NULL
)

print(field_frequency_table)

write.csv(
  field_frequency_table,
  "cleaned_output/validation/experimental_field_frequency.csv",
  row.names = FALSE
)

setwd("/scratch/grp/msc_appbio/Group11_Project/Group11")

# Check alternative field names
alternative_fields <- c("parameterName", "strainName")

alternative_field_files <- lapply(seq_along(raw_records), function(i) {
  x <- raw_records[[i]]
  
  if (is.null(x)) return(NULL)
  
  matched <- intersect(x$field, alternative_fields)
  
  if (length(matched) == 0) return(NULL)
  
  data.frame(
    file_name = basename(files[i]),
    field = matched,
    stringsAsFactors = FALSE
  )
})

alternative_field_files <- do.call(rbind, alternative_field_files)

print(alternative_field_files)

write.csv(
  alternative_field_files,
  "cleaned_output/validation/alternative_field_names.csv",
  row.names = FALSE
)

field_frequency_all <- sort(
  table(unlist(lapply(raw_records, function(x) {
    if (is.null(x)) return(NULL)
    x$field
  }))),
  decreasing = TRUE
)

field_frequency_table <- data.frame(
  field = names(field_frequency_all),
  frequency = as.integer(field_frequency_all),
  row.names = NULL
)

print(field_frequency_table)

write.csv(
  field_frequency_table,
  "cleaned_output/validation/experimental_field_frequency.csv",
  row.names = FALSE
)

files_to_inspect <- c(
  "an3544725987.csv",
  "an8545935526.csv"
)

for (f in files_to_inspect) {
  cat("\n\n==========", f, "==========\n")
  print(readLines(
    file.path("group11_data", f),
    warn = FALSE
  ))
}


# Inspect the reference entry for the first file
parameter_description_clean[
  trimws(parameter_description_clean$parameterId) == "IMPC_ECH_013_001",
  c("impcParameterOrigId", "name", "description", "parameterId")
]

# Inspect the reference entry for the second file
parameter_description_clean[
  trimws(parameter_description_clean$parameterId) == "MGP_IMM_124_001",
  c("impcParameterOrigId", "name", "description", "parameterId")
]


# Search the complete cleaned reference table
reference_match <- parameter_description_clean[
  grepl(
    "MGP_IMM_124_001",
    parameter_description_clean$parameterId,
    fixed = TRUE
  ),
]

print(reference_match)

# Search by the experimental parameter name
name_match <- parameter_description_clean[
  grepl(
    "Monocytes",
    parameter_description_clean$name,
    ignore.case = TRUE
  ),
  c("impcParameterOrigId", "name", "description", "parameterId")
]

print(name_match)

# Check whether the stable ID exists anywhere in the original reference data
sum(
  trimws(parameter_description$parameterId) == "MGP_IMM_124_001",
  na.rm = TRUE
)

unmatched_parameter_ids <- data.frame(
  file_name = "an8545935526.csv",
  experimental_parameter_name = "Monocytes - % of CD45+",
  experimental_parameter_id = "MGP_IMM_124_001",
  validation_status = "Unmatched reference ID",
  action = "Retain original; investigate reference coverage",
  stringsAsFactors = FALSE
)

write.csv(
  unmatched_parameter_ids,
  "cleaned_output/validation/unmatched_experimental_parameter_ids.csv",
  row.names = FALSE
)

unmatched_parameter_ids <- data.frame(
  file_name = "an8545935526.csv",
  experimental_parameter_name = "Monocytes - % of CD45+",
  experimental_parameter_id = "MGP_IMM_124_001",
  validation_status = "Unmatched reference ID",
  action = "Retain original; investigate reference coverage",
  stringsAsFactors = FALSE
)

write.csv(
  unmatched_parameter_ids,
  "cleaned_output/validation/unmatched_experimental_parameter_ids.csv",
  row.names = FALSE
)

# Extract experimental parameter IDs
experimental_parameter_ids <- unique(unlist(
  lapply(raw_records, function(x) {
    if (is.null(x)) return(NULL)
    
    idx <- which(x$field == "parameter_stable_id")
    
    if (length(idx) == 0) return(NULL)
    
    trimws(x$value[idx])
  })
))

# Compare against reference IDs
reference_parameter_ids <- unique(
  trimws(parameter_description_clean$parameterId)
)

# Identify unmatched IDs
unmatched_ids <- setdiff(
  experimental_parameter_ids,
  reference_parameter_ids
)

print(length(experimental_parameter_ids))
print(length(unmatched_ids))
print(unmatched_ids)

write.csv(
  data.frame(parameter_stable_id = unmatched_ids),
  "cleaned_output/validation/unmatched_parameter_ids.csv",
  row.names = FALSE
)

# 1. Extract one parameter ID per experimental file
experimental_id_records <- do.call(
  rbind,
  lapply(seq_along(raw_records), function(i) {
    
    x <- raw_records[[i]]
    
    if (is.null(x)) return(NULL)
    
    idx <- which(x$field == "parameter_stable_id")
    
    if (length(idx) == 0) return(NULL)
    
    data.frame(
      file_name = basename(files[i]),
      parameter_stable_id = trimws(x$value[idx[1]]),
      stringsAsFactors = FALSE
    )
  })
)

# 2. Identify IDs not present in the reference table
reference_ids <- unique(
  trimws(parameter_description_clean$parameterId)
)

reference_ids <- reference_ids[
  !is.na(reference_ids) & nzchar(reference_ids)
]

experimental_id_records$in_reference <-
  experimental_id_records$parameter_stable_id %in% reference_ids

unmatched_records <- subset(
  experimental_id_records,
  !in_reference
)

# 3. Count the frequency of each unmatched ID
unmatched_frequency <- as.data.frame(
  table(unmatched_records$parameter_stable_id),
  stringsAsFactors = FALSE
)

names(unmatched_frequency) <- c(
  "parameter_stable_id",
  "record_count"
)

unmatched_frequency <- unmatched_frequency[
  order(-unmatched_frequency$record_count),
]

# 4. Classify the unmatched identifiers
unmatched_frequency$is_missing <- (
  is.na(unmatched_frequency$parameter_stable_id) |
    unmatched_frequency$parameter_stable_id == "" |
    toupper(unmatched_frequency$parameter_stable_id) == "NA"
)

unmatched_frequency$has_unusual_characters <- grepl(
  "[^A-Za-z0-9_]",
  unmatched_frequency$parameter_stable_id
)

unmatched_frequency$contains_lowercase <- grepl(
  "[a-z]",
  unmatched_frequency$parameter_stable_id
)

# 5. Summarise the results
cat("Total experimental records with an ID:",
    nrow(experimental_id_records), "\n")

cat("Records with unmatched IDs:",
    nrow(unmatched_records), "\n")

cat("Distinct unmatched IDs:",
    nrow(unmatched_frequency), "\n")

cat("Distinct IDs containing unusual characters:",
    sum(unmatched_frequency$has_unusual_characters), "\n")

cat("Distinct missing/literal NA IDs:",
    sum(unmatched_frequency$is_missing), "\n")

cat("Distinct IDs containing lowercase characters:",
    sum(unmatched_frequency$contains_lowercase), "\n")

# 6. Display the most frequent unmatched IDs
print(head(unmatched_frequency, 30))

# 7. Save detailed validation outputs
write.csv(
  unmatched_frequency,
  "cleaned_output/validation/unmatched_parameter_id_frequency.csv",
  row.names = FALSE
)

write.csv(
  unmatched_records,
  "cleaned_output/validation/unmatched_parameter_id_records.csv",
  row.names = FALSE
)

# Classify identifier formats
unmatched_frequency$id_class <- ifelse(
  unmatched_frequency$is_missing,
  "Missing or literal NA",
  ifelse(
    grepl(
      "^[A-Z0-9]+(_[A-Z0-9]+)+$",
      unmatched_frequency$parameter_stable_id
    ),
    "Standard-looking uppercase ID",
    ifelse(
      grepl(
        "^[a-z0-9]+(_[a-z0-9]+)+$",
        unmatched_frequency$parameter_stable_id
      ),
      "Lowercase ID",
      "Unusual format - investigate"
    )
  )
)

# Summarise distinct IDs
print(table(unmatched_frequency$id_class))

# Summarise affected experimental records
record_summary <- aggregate(
  record_count ~ id_class,
  data = unmatched_frequency,
  FUN = sum
)

print(record_summary)

# Inspect the most frequent standard-looking unmatched IDs
standard_ids <- subset(
  unmatched_frequency,
  id_class == "Standard-looking uppercase ID"
)

print(head(standard_ids, 40))

# Save classification
write.csv(
  unmatched_frequency,
  "cleaned_output/validation/unmatched_parameter_id_classification.csv",
  row.names = FALSE
)

write.csv(
  record_summary,
  "cleaned_output/validation/unmatched_parameter_id_class_summary.csv",
  row.names = FALSE
)

# 1. Extract all experimental parameter IDs
all_experimental_ids <- unique(
  trimws(experimental_id_records$parameter_stable_id)
)

all_experimental_ids <- all_experimental_ids[
  !is.na(all_experimental_ids) &
    nzchar(all_experimental_ids) &
    toupper(all_experimental_ids) != "NA"
]

# 2. Extract reference parameter IDs
all_reference_ids <- unique(
  trimws(parameter_description_clean$parameterId)
)

all_reference_ids <- all_reference_ids[
  !is.na(all_reference_ids) &
    nzchar(all_reference_ids)
]

# 3. Extract the first two identifier components
get_prefix <- function(x) {
  sub("^([^_]+_[^_]+)_.*$", "\\1", x)
}

experimental_prefixes <- data.frame(
  parameter_stable_id = all_experimental_ids,
  prefix = get_prefix(all_experimental_ids),
  stringsAsFactors = FALSE
)

reference_prefixes <- data.frame(
  parameterId = all_reference_ids,
  prefix = get_prefix(all_reference_ids),
  stringsAsFactors = FALSE
)

# 4. Count identifiers belonging to each family
experimental_prefix_summary <- as.data.frame(
  table(experimental_prefixes$prefix)
)

reference_prefix_summary <- as.data.frame(
  table(reference_prefixes$prefix)
)

names(experimental_prefix_summary) <- c(
  "prefix", "experimental_id_count"
)

names(reference_prefix_summary) <- c(
  "prefix", "reference_id_count"
)

# 5. Compare the families
prefix_comparison <- merge(
  experimental_prefix_summary,
  reference_prefix_summary,
  by = "prefix",
  all = TRUE
)

prefix_comparison$experimental_id_count[
  is.na(prefix_comparison$experimental_id_count)
] <- 0

prefix_comparison$reference_id_count[
  is.na(prefix_comparison$reference_id_count)
] <- 0

prefix_comparison <- prefix_comparison[
  order(-prefix_comparison$experimental_id_count),
]

print(prefix_comparison, row.names = FALSE)

# 6. Save results
write.csv(
  prefix_comparison,
  "cleaned_output/validation/parameter_id_prefix_comparison.csv",
  row.names = FALSE
)

# 1. Extract one ID per experimental record
family_records <- experimental_id_records

# 2. Remove missing/literal NA identifiers for this analysis
family_records <- subset(
  family_records,
  !is.na(parameter_stable_id) &
    nzchar(parameter_stable_id) &
    toupper(parameter_stable_id) != "NA"
)

# 3. Extract the first two components
family_records$prefix <- sub(
  "^([^_]+_[^_]+)_.*$",
  "\\1",
  family_records$parameter_stable_id
)

# 4. Identify exact reference matches
family_records$in_reference <-
  family_records$parameter_stable_id %in% all_reference_ids

# 5. Summarise by prefix using experimental record counts
family_summary <- aggregate(
  cbind(
    total_records = rep(1, nrow(family_records)),
    matched_records = as.integer(family_records$in_reference),
    unmatched_records = as.integer(!family_records$in_reference)
  ),
  by = list(prefix = family_records$prefix),
  FUN = sum
)

# 6. Calculate percentage matched
family_summary$matched_percentage <- round(
  100 * family_summary$matched_records /
    family_summary$total_records,
  2
)

# 7. Sort by number of experimental records
family_summary <- family_summary[
  order(-family_summary$total_records),
]

print(head(family_summary, 60), row.names = FALSE)

# 8. Save
write.csv(
  family_summary,
  "cleaned_output/validation/parameter_family_record_summary.csv",
  row.names = FALSE
)

# 1. Create a normalisation function for comparison only
normalise_id <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", trimws(x)))
}

# 2. Prepare unmatched experimental IDs
impc_unmatched <- subset(
  unmatched_frequency,
  !is_missing
)

impc_unmatched$normalised_id <- normalise_id(
  impc_unmatched$parameter_stable_id
)

# Focus on identifiers whose normalised form starts with IMPC
impc_unmatched <- subset(
  impc_unmatched,
  startsWith(normalised_id, "IMPC")
)

# 3. Prepare reference identifiers
reference_lookup <- data.frame(
  reference_id = unique(all_reference_ids),
  stringsAsFactors = FALSE
)

reference_lookup$normalised_id <- normalise_id(
  reference_lookup$reference_id
)

# 4. Find identifiers with matching normalised forms
candidate_matches <- merge(
  impc_unmatched[
    , c("parameter_stable_id", "record_count", "normalised_id")
  ],
  reference_lookup,
  by = "normalised_id"
)

# 5. Count the number of possible reference matches per experimental ID
candidate_summary <- aggregate(
  reference_id ~ parameter_stable_id + record_count,
  data = candidate_matches,
  FUN = function(x) paste(unique(x), collapse = "; ")
)

candidate_counts <- aggregate(
  reference_id ~ parameter_stable_id + record_count,
  data = candidate_matches,
  FUN = function(x) length(unique(x))
)

names(candidate_counts)[3] <- "candidate_count"

candidate_summary <- merge(
  candidate_summary,
  candidate_counts,
  by = c("parameter_stable_id", "record_count")
)

candidate_summary$match_status <- ifelse(
  candidate_summary$candidate_count == 1,
  "Unique normalised candidate",
  "Ambiguous normalised candidates"
)

# 6. Summarise results
cat("Unmatched IMPC-like IDs:", nrow(impc_unmatched), "\n")
cat("IDs with a normalised reference match:",
    nrow(candidate_summary), "\n")
cat("Unique candidate matches:",
    sum(candidate_summary$candidate_count == 1), "\n")
cat("Ambiguous candidate matches:",
    sum(candidate_summary$candidate_count > 1), "\n")

print(
  head(
    candidate_summary[
      order(-candidate_summary$record_count),
    ],
    30
  ),
  row.names = FALSE
)

# 7. Save results
write.csv(
  candidate_summary,
  "cleaned_output/validation/impc_normalised_candidate_matches.csv",
  row.names = FALSE
)

# Count distinct original experimental IDs mapping to each reference ID
mapping_collisions <- aggregate(
  parameter_stable_id ~ reference_id,
  data = candidate_matches,
  FUN = function(x) length(unique(x))
)

names(mapping_collisions)[2] <- "original_id_count"

# Retain reference IDs with multiple original IDs
mapping_collisions <- subset(
  mapping_collisions,
  original_id_count > 1
)

mapping_collisions <- mapping_collisions[
  order(-mapping_collisions$original_id_count),
]

cat(
  "Reference IDs receiving multiple original experimental IDs:",
  nrow(mapping_collisions), "\n"
)

print(head(mapping_collisions, 30), row.names = FALSE)

# Add original/reference comparison
candidate_matches$exact_match <-
  candidate_matches$parameter_stable_id ==
  candidate_matches$reference_id

candidate_matches$case_only_match <-
  toupper(candidate_matches$parameter_stable_id) ==
  toupper(candidate_matches$reference_id)

candidate_matches$format_difference <- ifelse(
  candidate_matches$exact_match,
  "Exact",
  ifelse(
    candidate_matches$case_only_match,
    "Case difference only",
    "Punctuation or structural difference"
  )
)

print(table(candidate_matches$format_difference))

# Save detailed diagnostic output
write.csv(
  candidate_matches,
  "cleaned_output/validation/impc_candidate_match_details.csv",
  row.names = FALSE
)





# 1. Identify reference IDs receiving multiple original IDs
collision_reference_ids <- mapping_collisions$reference_id

# 2. Extract all candidate mappings involving these references
collision_details <- subset(
  candidate_matches,
  reference_id %in% collision_reference_ids
)

# 3. Summarise original IDs and records per reference ID
collision_summary <- aggregate(
  cbind(
    distinct_original_ids = rep(1, nrow(collision_details)),
    affected_records = collision_details$record_count
  ),
  by = list(reference_id = collision_details$reference_id),
  FUN = sum
)

collision_summary <- collision_summary[
  order(-collision_summary$affected_records),
]

# 4. Overall impact
cat(
  "Reference IDs with collisions:",
  nrow(collision_summary), "\n"
)

cat(
  "Distinct original IDs involved:",
  length(unique(collision_details$parameter_stable_id)), "\n"
)

cat(
  "Experimental records involved:",
  sum(collision_details$record_count), "\n"
)

# 5. Inspect the 20 most affected reference IDs
print(head(collision_summary, 20), row.names = FALSE)

# 6. Inspect the original IDs mapping to the most affected reference
top_reference <- collision_summary$reference_id[1]

print(
  subset(
    collision_details,
    reference_id == top_reference
  )[
    , c(
      "parameter_stable_id",
      "reference_id",
      "record_count",
      "format_difference"
    )
  ],
  row.names = FALSE
)

# 7. Save collision investigation
write.csv(
  collision_details,
  "cleaned_output/validation/impc_mapping_collision_details.csv",
  row.names = FALSE
)

write.csv(
  collision_summary,
  "cleaned_output/validation/impc_mapping_collision_summary.csv",
  row.names = FALSE
)

# 1. Calculate one edit distance for each original/reference pair
candidate_matches$edit_distance <- mapply(
  function(original, reference) {
    as.numeric(adist(
      toupper(original),
      toupper(reference)
    ))
  },
  candidate_matches$parameter_stable_id,
  candidate_matches$reference_id
)

# 2. Calculate string-length difference
candidate_matches$length_difference <-
  nchar(candidate_matches$parameter_stable_id) -
  nchar(candidate_matches$reference_id)

# 3. Compare underscore counts
candidate_matches$original_underscore_count <- 
  lengths(regmatches(
    candidate_matches$parameter_stable_id,
    gregexpr("_", candidate_matches$parameter_stable_id, fixed = TRUE)
  ))

candidate_matches$reference_underscore_count <-
  lengths(regmatches(
    candidate_matches$reference_id,
    gregexpr("_", candidate_matches$reference_id, fixed = TRUE)
  ))

# 4. Summarise edit distances
edit_distance_summary <- as.data.frame(
  table(candidate_matches$edit_distance)
)

record_edit_summary <- aggregate(
  record_count ~ edit_distance,
  data = candidate_matches,
  FUN = sum
)

print(record_edit_summary)

# Examine pairs with edit distance 1
distance_one <- subset(
  candidate_matches,
  edit_distance == 1
)

distance_one <- distance_one[
  order(-distance_one$record_count),
]

print(
  head(
    distance_one[
      , c(
        "parameter_stable_id",
        "reference_id",
        "record_count",
        "edit_distance",
        "length_difference",
        "original_underscore_count",
        "reference_underscore_count"
      )
    ],
    30
  ),
  row.names = FALSE
)

# Save corrected diagnostic outputs
write.csv(
  candidate_matches,
  "cleaned_output/validation/impc_edit_distance_analysis.csv",
  row.names = FALSE
)

write.csv(
  edit_distance_summary,
  "cleaned_output/validation/impc_edit_distance_summary.csv",
  row.names = FALSE
)

write.csv(
  record_edit_summary,
  "cleaned_output/validation/impc_edit_distance_record_summary.csv",
  row.names = FALSE
)

# 1. Check the dimensions and unique IDs
cat("Candidate rows:", nrow(candidate_matches), "\n")

cat(
  "Unique experimental IDs:",
  length(unique(candidate_matches$parameter_stable_id)),
  "\n"
)

cat(
  "Unique reference IDs:",
  length(unique(candidate_matches$reference_id)),
  "\n"
)

# 2. Remove duplicate original-reference pairs
candidate_pairs_unique <- unique(
  candidate_matches[
    ,
    c(
      "parameter_stable_id",
      "reference_id",
      "record_count",
      "edit_distance"
    )
  ]
)

cat(
  "Unique candidate pairs:",
  nrow(candidate_pairs_unique),
  "\n"
)

# 3. Count how many reference candidates each original ID has
candidates_per_original <- aggregate(
  reference_id ~ parameter_stable_id,
  data = candidate_pairs_unique,
  FUN = function(x) length(unique(x))
)

print(table(candidates_per_original$reference_id))

# 4. Identify original IDs with multiple reference candidates
ambiguous_original_ids <- subset(
  candidates_per_original,
  reference_id > 1
)

cat(
  "Original IDs with multiple candidates:",
  nrow(ambiguous_original_ids),
  "\n"
)

names(edit_distance_summary) <- c(
  "edit_distance",
  "candidate_pair_count"
)

edit_distance_summary$edit_distance <- as.integer(
  as.character(edit_distance_summary$edit_distance)
)

edit_distance_summary$candidate_pair_count <- as.integer(
  as.character(edit_distance_summary$candidate_pair_count)
)

print(edit_distance_summary)

# 1. Check the dimensions and unique IDs
cat("Candidate rows:", nrow(candidate_matches), "\n")

cat(
  "Unique experimental IDs:",
  length(unique(candidate_matches$parameter_stable_id)),
  "\n"
)

cat(
  "Unique reference IDs:",
  length(unique(candidate_matches$reference_id)),
  "\n"
)

# 2. Remove duplicate original-reference pairs
candidate_pairs_unique <- unique(
  candidate_matches[
    ,
    c(
      "parameter_stable_id",
      "reference_id",
      "record_count",
      "edit_distance"
    )
  ]
)

cat(
  "Unique candidate pairs:",
  nrow(candidate_pairs_unique),
  "\n"
)

# 3. Count how many reference candidates each original ID has
candidates_per_original <- aggregate(
  reference_id ~ parameter_stable_id,
  data = candidate_pairs_unique,
  FUN = function(x) length(unique(x))
)

print(table(candidates_per_original$reference_id))

# 4. Identify original IDs with multiple reference candidates
ambiguous_original_ids <- subset(
  candidates_per_original,
  reference_id > 1
)

cat(
  "Original IDs with multiple candidates:",
  nrow(ambiguous_original_ids),
  "\n"
)



# One row per original experimental ID
unique_id_matches <- candidate_pairs_unique[
  ,
  c(
    "parameter_stable_id",
    "reference_id",
    "record_count",
    "edit_distance"
  )
]

# Summarise unique IDs by edit distance
id_edit_summary <- aggregate(
  parameter_stable_id ~ edit_distance,
  data = unique_id_matches,
  FUN = length
)

names(id_edit_summary)[2] <- "unique_experimental_ids"

# Summarise experimental records by edit distance
record_edit_summary_corrected <- aggregate(
  record_count ~ edit_distance,
  data = unique_id_matches,
  FUN = sum
)

# Combine the summaries
edit_distance_summary_final <- merge(
  id_edit_summary,
  record_edit_summary_corrected,
  by = "edit_distance"
)

edit_distance_summary_final <- edit_distance_summary_final[
  order(edit_distance_summary_final$edit_distance),
]

print(edit_distance_summary_final)

# Save final summary
write.csv(
  edit_distance_summary_final,
  "cleaned_output/validation/impc_edit_distance_summary_final.csv",
  row.names = FALSE
)

# Count original IDs mapping to each reference ID
reference_mapping_counts <- aggregate(
  parameter_stable_id ~ reference_id,
  data = unique_id_matches,
  FUN = function(x) length(unique(x))
)

names(reference_mapping_counts)[2] <- "original_id_count"

# Sort by number of original IDs
reference_mapping_counts <- reference_mapping_counts[
  order(-reference_mapping_counts$original_id_count),
]

# Display the 20 largest collisions
print(
  head(reference_mapping_counts, 20),
  row.names = FALSE
)

# Save mapping counts
write.csv(
  reference_mapping_counts,
  "cleaned_output/validation/impc_reference_mapping_counts.csv",
  row.names = FALSE
)


top_collision_ids <- head(
  reference_mapping_counts$reference_id,
  10
)

top_collision_details <- unique_id_matches[
  unique_id_matches$reference_id %in% top_collision_ids,
  c(
    "reference_id",
    "parameter_stable_id",
    "record_count",
    "edit_distance"
  )
]

top_collision_details <- top_collision_details[
  order(
    top_collision_details$reference_id,
    top_collision_details$parameter_stable_id
  ),
]

print(top_collision_details, row.names = FALSE)

write.csv(
  top_collision_details,
  "cleaned_output/validation/impc_top_collision_details.csv",
  row.names = FALSE
)


# Check available objects
cat("Experimental records:", length(raw_records), "\n")

cat("Reference table columns:\n")
print(names(parameter_description))

cat("Experimental record fields:\n")
print(sort(unique(unlist(
  lapply(raw_records, function(x) {
    if (is.null(x)) return(NULL)
    x$field
  })
))))



# Extract parameter IDs and names from every experimental file

experimental_parameter_names <- do.call(
  rbind,
  lapply(seq_along(raw_records), function(i) {
    
    x <- raw_records[[i]]
    
    if (is.null(x)) return(NULL)
    
    id_idx <- which(x$field == "parameter_stable_id")
    
    name_idx <- which(
      x$field %in% c("parameter_name", "parameterName")
    )
    
    if (length(id_idx) == 0 || length(name_idx) == 0) {
      return(NULL)
    }
    
    data.frame(
      file_name = basename(files[i]),
      parameter_stable_id = trimws(x$value[id_idx[1]]),
      experimental_parameter_name = trimws(x$value[name_idx[1]]),
      stringsAsFactors = FALSE
    )
  })
)

# Remove empty identifiers and names
experimental_parameter_names <- subset(
  experimental_parameter_names,
  !is.na(parameter_stable_id) &
    parameter_stable_id != "" &
    !is.na(experimental_parameter_name) &
    experimental_parameter_name != ""
)

cat(
  "Experimental records with parameter names:",
  nrow(experimental_parameter_names),
  "\n"
)

cat(
  "Distinct experimental parameter IDs:",
  length(unique(experimental_parameter_names$parameter_stable_id)),
  "\n"
)


# Standardise the reference parameter table
reference_names <- parameter_description[
  ,
  c("parameterId", "name")
]

reference_names$parameterId <- trimws(
  as.character(reference_names$parameterId)
)

reference_names$name <- trimws(
  as.character(reference_names$name)
)

# Remove missing reference identifiers and names
reference_names <- subset(
  reference_names,
  !is.na(parameterId) &
    parameterId != "" &
    !is.na(name) &
    name != ""
)

# Connect experimental names to proposed reference IDs
experimental_candidate_names <- merge(
  experimental_parameter_names,
  unique_id_matches[
    ,
    c("parameter_stable_id", "reference_id")
  ],
  by = "parameter_stable_id"
)

# Join candidate IDs to reference parameter names
parameter_name_comparison <- merge(
  experimental_candidate_names,
  reference_names,
  by.x = "reference_id",
  by.y = "parameterId",
  all.x = TRUE
)

# Compare names ignoring case and surrounding whitespace
parameter_name_comparison$experimental_name_normalised <- tolower(
  trimws(parameter_name_comparison$experimental_parameter_name)
)

parameter_name_comparison$reference_name_normalised <- tolower(
  trimws(parameter_name_comparison$name)
)

parameter_name_comparison$name_match <-
  parameter_name_comparison$experimental_name_normalised ==
  parameter_name_comparison$reference_name_normalised

print(
  head(parameter_name_comparison, 20),
  row.names = FALSE
)




# 1. Create distinct experimental ID-name combinations
experimental_name_pairs <- unique(
  experimental_parameter_names[
    ,
    c(
      "parameter_stable_id",
      "experimental_parameter_name"
    )
  ]
)

# 2. Create distinct reference ID-name combinations
reference_name_pairs <- unique(
  reference_names[
    ,
    c("parameterId", "name")
  ]
)

# 3. Normalise names for case-insensitive comparison
experimental_name_pairs$normalised_name <- tolower(
  trimws(experimental_name_pairs$experimental_parameter_name)
)

reference_name_pairs$normalised_name <- tolower(
  trimws(reference_name_pairs$name)
)

# 4. Compare each original ID with its candidate reference ID
candidate_name_check <- merge(
  experimental_name_pairs,
  unique_id_matches[
    ,
    c("parameter_stable_id", "reference_id")
  ],
  by = "parameter_stable_id"
)

candidate_name_check <- merge(
  candidate_name_check,
  reference_name_pairs[
    ,
    c("parameterId", "normalised_name")
  ],
  by.x = "reference_id",
  by.y = "parameterId",
  all.x = TRUE
)

# 5. Identify exact name matches
candidate_name_check$name_match <-
  !is.na(candidate_name_check$normalised_name.y) &
  candidate_name_check$normalised_name.x ==
  candidate_name_check$normalised_name.y

# 6. Summarise at the original experimental-ID level
id_name_support <- aggregate(
  name_match ~ parameter_stable_id,
  data = candidate_name_check,
  FUN = function(x) any(x, na.rm = TRUE)
)

# 7. Identify IDs with no available reference name
reference_name_availability <- aggregate(
  !is.na(normalised_name.y) ~ parameter_stable_id,
  data = candidate_name_check,
  FUN = any
)

names(reference_name_availability)[2] <- "reference_name_available"

# 8. Combine the two summaries
id_name_support <- merge(
  id_name_support,
  reference_name_availability,
  by = "parameter_stable_id"
)

id_name_support$classification <- ifelse(
  id_name_support$name_match,
  "Name-supported",
  ifelse(
    id_name_support$reference_name_available,
    "Name-conflicting",
    "Unresolved"
  )
)

# 9. Summarise unique experimental IDs
print(
  table(id_name_support$classification)
)

# 10. Save the results
write.csv(
  id_name_support,
  "cleaned_output/validation/impc_candidate_name_support.csv",
  row.names = FALSE
)

table(id_name_support$classification)



# Correct element-wise comparison
candidate_name_check$name_match <-
  !is.na(candidate_name_check$normalised_name.y) &
  candidate_name_check$normalised_name.x ==
  candidate_name_check$normalised_name.y

# Summarise at original experimental-ID level
id_name_support <- aggregate(
  name_match ~ parameter_stable_id,
  data = candidate_name_check,
  FUN = function(x) any(x, na.rm = TRUE)
)

# Check reference-name availability
reference_name_availability <- aggregate(
  !is.na(normalised_name.y) ~ parameter_stable_id,
  data = candidate_name_check,
  FUN = any
)

names(reference_name_availability)[2] <-
  "reference_name_available"

# Combine summaries
id_name_support <- merge(
  id_name_support,
  reference_name_availability,
  by = "parameter_stable_id"
)

# Classify
id_name_support$classification <- ifelse(
  id_name_support$name_match,
  "Name-supported",
  ifelse(
    id_name_support$reference_name_available,
    "Name-conflicting",
    "Unresolved"
  )
)

# Display results
print(table(id_name_support$classification))

# Save corrected results
write.csv(
  id_name_support,
  "cleaned_output/validation/impc_candidate_name_support.csv",
  row.names = FALSE
)

cat(
  "Rows with matching names:",
  sum(candidate_name_check$name_match, na.rm = TRUE),
  "\n"
)

cat(
  "Rows with non-matching names:",
  sum(!candidate_name_check$name_match, na.rm = TRUE),
  "\n"
)

cat(
  "Unique experimental IDs:",
  length(unique(id_name_support$parameter_stable_id)),
  "\n"
)


candidate_name_check$name_match <- !is.na(candidate_name_check$normalised_name.y) & (candidate_name_check$normalised_name.x == candidate_name_check$normalised_name.y)


table(candidate_name_check$name_match, useNA = "ifany")


# Number of distinct experimental names per ID
experimental_name_counts <- aggregate(
  normalised_name.x ~ parameter_stable_id,
  data = candidate_name_check,
  FUN = function(x) length(unique(x))
)

# Number of distinct reference names per ID
reference_name_counts <- aggregate(
  normalised_name.y ~ parameter_stable_id,
  data = candidate_name_check,
  FUN = function(x) length(unique(na.omit(x)))
)

# Number of matching name pairs per ID
matching_name_counts <- aggregate(
  name_match ~ parameter_stable_id,
  data = candidate_name_check,
  FUN = function(x) sum(x, na.rm = TRUE)
)

cat("Experimental IDs with multiple experimental names:",
    sum(experimental_name_counts[[2]] > 1), "\n")

cat("Experimental IDs with multiple reference names:",
    sum(reference_name_counts[[2]] > 1), "\n")

cat("Experimental IDs with more than one matching pair:",
    sum(matching_name_counts[[2]] > 1), "\n")


head(
  candidate_name_check[
    ,
    c(
      "parameter_stable_id",
      "experimental_parameter_name",
      "reference_id",
      "normalised_name.x",
      "normalised_name.y",
      "name_match"
    )
  ],
  20
)




# Record-level summary for name-supported candidate IDs
name_supported_records <- merge(
  experimental_parameter_names,
  id_name_support[
    id_name_support$classification == "Name-supported",
    c("parameter_stable_id", "classification")
  ],
  by = "parameter_stable_id"
)

cat("Name-supported experimental records:",
    nrow(name_supported_records), "\n")

cat("Distinct name-supported experimental IDs:",
    length(unique(name_supported_records$parameter_stable_id)), "\n")

cat("Percentage of all experimental records:",
    round(
      100 * nrow(name_supported_records) /
        nrow(experimental_parameter_names),
      2
    ), "%\n")


remaining_unmatched <- unmatched_frequency[
  !unmatched_frequency$parameter_stable_id %in%
    id_name_support$parameter_stable_id,
]

cat("Remaining unmatched distinct IDs:",
    nrow(remaining_unmatched), "\n")

cat("Records associated with remaining unmatched IDs:",
    sum(remaining_unmatched$frequency), "\n")



names(unmatched_frequency)
str(unmatched_frequency)
head(unmatched_frequency, 10)

sum(remaining_unmatched$YOUR_FREQUENCY_COLUMN, na.rm = TRUE)




cat(
  "Remaining unmatched distinct IDs:",
  nrow(remaining_unmatched), "\n"
)

cat(
  "Remaining unmatched experimental records:",
  sum(remaining_unmatched$record_count, na.rm = TRUE), "\n"
)


total_unmatched_records <- 9417

resolved_records <- nrow(name_supported_records)

remaining_records <- sum(
  remaining_unmatched$record_count,
  na.rm = TRUE
)

cat("Initially unmatched records:", total_unmatched_records, "\n")
cat("Name-supported records:", resolved_records, "\n")
cat("Remaining unmatched records:", remaining_records, "\n")

cat(
  "Percentage resolved:",
  round(100 * resolved_records / total_unmatched_records, 2),
  "%\n"
)

cat(
  "Percentage remaining:",
  round(100 * remaining_records / total_unmatched_records, 2),
  "%\n"
)



mapping_coverage_summary <- data.frame(
  category = c(
    "Initially unmatched",
    "Name-supported candidate mappings",
    "Remaining unmatched"
  ),
  distinct_ids = c(
    nrow(unmatched_frequency),
    length(unique(name_supported_records$parameter_stable_id)),
    nrow(remaining_unmatched)
  ),
  experimental_records = c(
    total_unmatched_records,
    resolved_records,
    remaining_records
  ),
  percentage_of_initially_unmatched = c(
    100,
    round(100 * resolved_records / total_unmatched_records, 2),
    round(100 * remaining_records / total_unmatched_records, 2)
  )
)

print(mapping_coverage_summary)

write.csv(
  mapping_coverage_summary,
  "cleaned_output/validation/impc_mapping_coverage_summary.csv",
  row.names = FALSE
)



# Summarise remaining unmatched IDs by classification
remaining_class_summary <- aggregate(
  cbind(record_count = record_count) ~ id_class,
  data = remaining_unmatched,
  FUN = sum
)

# Count distinct IDs in each classification
remaining_id_counts <- as.data.frame(
  table(remaining_unmatched$id_class)
)

names(remaining_id_counts) <- c("id_class", "distinct_ids")

# Combine summaries
remaining_class_summary <- merge(
  remaining_class_summary,
  remaining_id_counts,
  by = "id_class"
)

# Calculate percentages
remaining_class_summary$record_percentage <- round(
  100 * remaining_class_summary$record_count /
    sum(remaining_class_summary$record_count),
  2
)

remaining_class_summary$id_percentage <- round(
  100 * remaining_class_summary$distinct_ids /
    sum(remaining_class_summary$distinct_ids),
  2
)

print(remaining_class_summary)

write.csv(
  remaining_class_summary,
  "cleaned_output/validation/remaining_unmatched_class_summary.csv",
  row.names = FALSE
)



# Extract standard-looking unmatched IDs
standard_unmatched <- subset(
  remaining_unmatched,
  id_class == "Standard-looking uppercase ID"
)

# Extract parameter family/prefix
standard_unmatched$parameter_family <- sub(
  "_[0-9]+_[0-9]+$",
  "",
  standard_unmatched$parameter_stable_id
)

# Summarise by family
standard_family_summary <- aggregate(
  cbind(record_count = record_count) ~ parameter_family,
  data = standard_unmatched,
  FUN = sum
)

# Count distinct IDs per family
standard_family_ids <- aggregate(
  parameter_stable_id ~ parameter_family,
  data = standard_unmatched,
  FUN = function(x) length(unique(x))
)

names(standard_family_ids)[2] <- "distinct_ids"

standard_family_summary <- merge(
  standard_family_summary,
  standard_family_ids,
  by = "parameter_family"
)

standard_family_summary <- standard_family_summary[
  order(-standard_family_summary$record_count),
]

print(head(standard_family_summary, 30))


write.csv(
  standard_family_summary,
  "cleaned_output/validation/remaining_standard_family_summary.csv",
  row.names = FALSE
)



# Extract distinct reference parameter families
reference_families <- unique(
  sub(
    "_[0-9]+_[0-9]+$",
    "",
    trimws(as.character(parameter_description$parameterId))
  )
)

# Compare remaining experimental families with reference families
standard_family_summary$present_in_reference <-
  standard_family_summary$parameter_family %in%
  reference_families

print(
  standard_family_summary[
    order(
      standard_family_summary$present_in_reference,
      -standard_family_summary$record_count
    ),
  ],
  row.names = FALSE
)


write.csv(
  standard_family_summary,
  "cleaned_output/validation/remaining_standard_family_reference_comparison.csv",
  row.names = FALSE
)



cat("Unusual unmatched rows:", nrow(unusual_unmatched), "\n")
cat("Unusual candidate rows:", nrow(unusual_candidates), "\n")

cat(
  "Rows with reference candidates:",
  sum(!is.na(unusual_candidates$reference_id)),
  "\n"
)

cat(
  "Rows without reference candidates:",
  sum(is.na(unusual_candidates$reference_id)),
  "\n"
)

print(table(remaining_unmatched$id_class))

# Split reference candidates by original experimental ID
candidate_groups <- split(
  unusual_candidates$reference_id,
  unusual_candidates$parameter_stable_id
)

# Count distinct non-missing reference candidates per ID
candidate_counts <- data.frame(
  parameter_stable_id = names(candidate_groups),
  candidate_count = vapply(
    candidate_groups,
    function(x) length(unique(na.omit(x))),
    integer(1)
  ),
  row.names = NULL
)


print(table(candidate_counts$candidate_count))



# Extract experimental names for the unusual IDs
unusual_names <- experimental_parameter_names[
  experimental_parameter_names$parameter_stable_id %in%
    unusual_unmatched$parameter_stable_id,
]

# Keep unique experimental ID/name pairs
unusual_name_pairs <- unique(
  unusual_names[
    ,
    c("parameter_stable_id", "experimental_parameter_name")
  ]
)

# Normalise names for comparison
unusual_name_pairs$normalised_name <- tolower(
  trimws(unusual_name_pairs$experimental_parameter_name)
)

# Prepare reference names and IDs
reference_name_pairs <- unique(
  parameter_description[
    ,
    c("parameterId", "name")
  ]
)

reference_name_pairs$parameterId <- trimws(
  as.character(reference_name_pairs$parameterId)
)

reference_name_pairs$normalised_name <- tolower(
  trimws(as.character(reference_name_pairs$name))
)

reference_name_pairs <- subset(
  reference_name_pairs,
  !is.na(parameterId) & parameterId != "" &
    !is.na(normalised_name) & normalised_name != ""
)

reference_name_pairs <- unique(reference_name_pairs)

# Match by parameter name, independently of the experimental ID
unusual_name_candidates <- merge(
  unusual_name_pairs,
  reference_name_pairs,
  by = "normalised_name"
)

# Count distinct reference IDs per experimental ID
if (nrow(unusual_name_candidates) > 0) {
  
  unusual_name_counts <- aggregate(
    parameterId ~ parameter_stable_id,
    data = unusual_name_candidates,
    FUN = function(x) length(unique(x))
  )
  
  print(table(unusual_name_counts$parameterId))
  
} else {
  
  cat("No exact parameter-name matches found.\n")
  
}


# Add candidate counts to all 987 unusual IDs
unusual_name_summary <- merge(
  unusual_unmatched,
  unusual_name_counts,
  by = "parameter_stable_id",
  all.x = TRUE
)

unusual_name_summary$parameterId[
  is.na(unusual_name_summary$parameterId)
] <- 0

# Classify
unusual_name_summary$match_status <- ifelse(
  unusual_name_summary$parameterId == 1,
  "Single candidate",
  ifelse(
    unusual_name_summary$parameterId > 1,
    "Multiple candidates",
    "No exact name match"
  )
)

# Summarise records and IDs
unusual_name_coverage <- aggregate(
  cbind(record_count = record_count) ~ match_status,
  data = unusual_name_summary,
  FUN = sum
)

unusual_name_id_counts <- as.data.frame(
  table(unusual_name_summary$match_status)
)

names(unusual_name_id_counts) <- c(
  "match_status", "distinct_ids"
)

unusual_name_coverage <- merge(
  unusual_name_coverage,
  unusual_name_id_counts,
  by = "match_status"
)

unusual_name_coverage$record_percentage <- round(
  100 * unusual_name_coverage$record_count /
    sum(unusual_name_coverage$record_count),
  2
)

print(unusual_name_coverage)


write.csv(
  unusual_name_candidates,
  "cleaned_output/validation/unusual_id_name_candidates.csv",
  row.names = FALSE
)

write.csv(
  unusual_name_coverage,
  "cleaned_output/validation/unusual_id_name_coverage.csv",
  row.names = FALSE
)


# Select IDs with exactly one reference candidate
single_candidate_ids <- unusual_name_counts[
  unusual_name_counts$parameterId == 1,
  "parameter_stable_id"
]

# Get their experimental names
single_candidate_names <- unusual_name_pairs[
  unusual_name_pairs$parameter_stable_id %in%
    single_candidate_ids,
]

# Count distinct experimental names per ID
experimental_names_per_id <- aggregate(
  normalised_name ~ parameter_stable_id,
  data = single_candidate_names,
  FUN = function(x) length(unique(x))
)

# Count matched experimental names per ID
matched_names_per_id <- aggregate(
  normalised_name ~ parameter_stable_id,
  data = merge(
    single_candidate_names,
    unusual_name_candidates[
      ,
      c("parameter_stable_id", "normalised_name")
    ],
    by = c("parameter_stable_id", "normalised_name")
  ),
  FUN = function(x) length(unique(x))
)

# Compare counts
name_validation <- merge(
  experimental_names_per_id,
  matched_names_per_id,
  by = "parameter_stable_id",
  suffixes = c("_experimental", "_matched")
)

names(name_validation)[2:3] <- c(
  "experimental_name_count",
  "matched_name_count"
)

name_validation$status <- ifelse(
  name_validation$experimental_name_count ==
    name_validation$matched_name_count,
  "All names matched",
  "Partial name match"
)

print(table(name_validation$status))



ambiguous_ids <- unusual_name_counts[
  unusual_name_counts$parameterId > 1,
  "parameter_stable_id"
]

ambiguous_candidates <- unusual_name_candidates[
  unusual_name_candidates$parameter_stable_id %in%
    ambiguous_ids,
]

cat("Ambiguous experimental IDs:",
    length(unique(ambiguous_candidates$parameter_stable_id)), "\n")

cat("Candidate rows:", nrow(ambiguous_candidates), "\n")


reference_details <- unique(
  parameter_description[
    ,
    c("parameterId", "impcParameterOrigId", "name", "description")
  ]
)

reference_details$parameterId <- trimws(
  as.character(reference_details$parameterId)
)

ambiguous_candidates$parameterId <- trimws(
  as.character(ambiguous_candidates$parameterId)
)

ambiguous_candidates_detailed <- merge(
  ambiguous_candidates,
  reference_details,
  by = "parameterId",
  all.x = TRUE
)

ambiguous_candidates_detailed$edit_distance <- mapply(
  function(original, reference) {
    as.numeric(adist(
      toupper(trimws(original)),
      toupper(trimws(reference))
    ))
  },
  ambiguous_candidates_detailed$parameter_stable_id,
  ambiguous_candidates_detailed$parameterId
)


ambiguous_candidates_detailed <- ambiguous_candidates_detailed[
  order(
    ambiguous_candidates_detailed$parameter_stable_id,
    ambiguous_candidates_detailed$edit_distance
  ),
]

ambiguous_candidates_detailed$candidate_rank <- ave(
  ambiguous_candidates_detailed$edit_distance,
  ambiguous_candidates_detailed$parameter_stable_id,
  FUN = function(x) rank(x, ties.method = "min")
)

# Number of candidates tied at the minimum distance
ambiguous_candidates_detailed$minimum_distance_ties <- ave(
  ambiguous_candidates_detailed$edit_distance,
  ambiguous_candidates_detailed$parameter_stable_id,
  FUN = function(x) sum(x == min(x))
)

ambiguous_distance_summary <- aggregate(
  edit_distance ~ parameter_stable_id,
  data = ambiguous_candidates_detailed,
  FUN = function(x) {
    paste(sort(unique(x)), collapse = ", ")
  }
)

names(ambiguous_distance_summary)[2] <- "candidate_edit_distances"

print(head(ambiguous_distance_summary, 20))

names(ambiguous_candidates_detailed)

str(ambiguous_candidates_detailed)


head(
  ambiguous_candidates_detailed[
    order(
      ambiguous_candidates_detailed$parameter_stable_id,
      ambiguous_candidates_detailed$edit_distance
    ),
    c(
      "parameter_stable_id",
      "parameterId",
      "impcParameterOrigId",
      "experimental_parameter_name",
      "name.x",
      "name.y",
      "description",
      "edit_distance",
      "candidate_rank",
      "minimum_distance_ties"
    )
  ],
  30
)




library(dplyr)

ambiguous_id_summary <- ambiguous_candidates_detailed %>%
  group_by(parameter_stable_id) %>%
  summarise(
    experimental_name = first(experimental_parameter_name),
    
    candidate_stable_ids = n_distinct(parameterId),
    
    minimum_edit_distance = min(edit_distance, na.rm = TRUE),
    
    candidates_at_minimum = n_distinct(
      parameterId[edit_distance == min(edit_distance, na.rm = TRUE)]
    ),
    
    candidate_ids = paste(
      sort(unique(parameterId)),
      collapse = "; "
    ),
    
    candidate_descriptions = paste(
      unique(na.omit(description)),
      collapse = " | "
    ),
    
    .groups = "drop"
  ) %>%
  arrange(desc(candidates_at_minimum), desc(candidate_stable_ids))

print(ambiguous_id_summary, n = 85)

write.csv(
  ambiguous_id_summary,
  "cleaned_output/validation/ambiguous_id_summary.csv",
  row.names = FALSE
)


ambiguous_id_summary$mapping_status <- ifelse(
  ambiguous_id_summary$candidate_stable_ids == 1,
  "Single candidate",
  ifelse(
    ambiguous_id_summary$candidates_at_minimum == 1,
    "Multiple candidates - unique minimum distance",
    "Multiple candidates - tied minimum distance"
  )
)

print(table(ambiguous_id_summary$mapping_status))

print(
  ambiguous_id_summary[
    ambiguous_id_summary$mapping_status ==
      "Multiple candidates - unique minimum distance",
    c(
      "parameter_stable_id",
      "experimental_name",
      "candidate_stable_ids",
      "minimum_edit_distance",
      "candidates_at_minimum",
      "candidate_ids"
    )
  ],
  row.names = FALSE
)

write.csv(
  ambiguous_id_summary,
  "cleaned_output/validation/ambiguous_id_summary.csv",
  row.names = FALSE
)


library(dplyr)

best_ambiguous_candidates <- ambiguous_candidates_detailed %>%
  group_by(parameter_stable_id) %>%
  filter(edit_distance == min(edit_distance, na.rm = TRUE)) %>%
  distinct(
    parameter_stable_id,
    experimental_parameter_name,
    parameterId,
    impcParameterOrigId,
    description,
    edit_distance
  ) %>%
  ungroup() %>%
  arrange(parameter_stable_id)

best_candidate_summary <- best_ambiguous_candidates %>%
  group_by(parameter_stable_id, experimental_parameter_name) %>%
  summarise(
    best_candidate_count = n_distinct(parameterId),
    best_candidate_ids = paste(
      sort(unique(parameterId)),
      collapse = "; "
    ),
    best_candidate_descriptions = paste(
      unique(na.omit(description)),
      collapse = " | "
    ),
    minimum_edit_distance = min(edit_distance),
    .groups = "drop"
  )

print(best_candidate_summary, n = 85)

write.csv(
  best_candidate_summary,
  "cleaned_output/validation/ambiguous_best_candidates.csv",
  row.names = FALSE
)


best_candidate_summary$final_status <- ifelse(
  best_candidate_summary$best_candidate_count == 1,
  "One best candidate",
  "Multiple tied best candidates"
)

print(table(best_candidate_summary$final_status))

print(
  best_candidate_summary[
    best_candidate_summary$best_candidate_count == 1,
    c(
      "parameter_stable_id",
      "experimental_parameter_name",
      "best_candidate_ids",
      "minimum_edit_distance"
    )
  ],
  row.names = FALSE
)

print(
  best_candidate_summary[
    best_candidate_summary$best_candidate_count > 1,
    c(
      "parameter_stable_id",
      "experimental_parameter_name",
      "best_candidate_count",
      "best_candidate_ids",
      "minimum_edit_distance"
    )
  ],
  row.names = FALSE
)


library(dplyr)

# Retrieve reference names from the original parameter description table
reference_name_lookup <- parameter_description %>%
  mutate(
    parameterId = trimws(parameterId),
    reference_name = trimws(name)
  ) %>%
  filter(
    !is.na(parameterId),
    !is.na(reference_name),
    reference_name != ""
  ) %>%
  group_by(parameterId) %>%
  summarise(
    reference_name_count = n_distinct(reference_name),
    reference_name = paste(
      sort(unique(reference_name)),
      collapse = " | "
    ),
    .groups = "drop"
  )

# Keep candidates with exactly one distinct best reference ID
unique_best_candidates <- best_ambiguous_candidates %>%
  group_by(parameter_stable_id) %>%
  filter(n_distinct(parameterId) == 1) %>%
  ungroup() %>%
  distinct(
    parameter_stable_id,
    experimental_parameter_name,
    parameterId,
    edit_distance
  ) %>%
  left_join(reference_name_lookup, by = "parameterId")

# Compare experimental names against reference names
unique_best_candidates$name_match <-
  unique_best_candidates$reference_name_count == 1 &
  tolower(trimws(unique_best_candidates$experimental_parameter_name)) ==
  tolower(trimws(unique_best_candidates$reference_name))

print(table(unique_best_candidates$name_match, useNA = "ifany"))

# Inspect mismatches and reference IDs with multiple names
print(
  unique_best_candidates[
    is.na(unique_best_candidates$name_match) |
      !unique_best_candidates$name_match,
  ],
  row.names = FALSE
)

write.csv(
  unique_best_candidates,
  "cleaned_output/validation/unique_best_candidate_name_validation.csv",
  row.names = FALSE
)


unique_best_candidates$name_match <-
  !is.na(unique_best_candidates$reference_name_count) &
  (unique_best_candidates$reference_name_count == 1) &
  !is.na(unique_best_candidates$experimental_parameter_name) &
  !is.na(unique_best_candidates$reference_name) &
  (
    tolower(trimws(unique_best_candidates$experimental_parameter_name)) ==
      tolower(trimws(unique_best_candidates$reference_name))
  )

print(table(unique_best_candidates$name_match, useNA = "ifany"))

print(
  unique_best_candidates[
    unique_best_candidates$name_match == FALSE |
      is.na(unique_best_candidates$name_match),
  ],
  row.names = FALSE
)

write.csv(
  unique_best_candidates,
  "cleaned_output/validation/unique_best_candidate_name_validation.csv",
  row.names = FALSE
)


unique_best_candidates$name_match <- (
  !is.na(unique_best_candidates$reference_name_count) &
    unique_best_candidates$reference_name_count == 1 &
    !is.na(unique_best_candidates$experimental_parameter_name) &
    !is.na(unique_best_candidates$reference_name) &
    (
      tolower(trimws(unique_best_candidates$experimental_parameter_name)) ==
        tolower(trimws(unique_best_candidates$reference_name))
    )
)

print(table(unique_best_candidates$name_match, useNA = "ifany"))

print(
  unique_best_candidates[
    unique_best_candidates$name_match == FALSE |
      is.na(unique_best_candidates$name_match),
  ],
  n = 60
)

unique_best_candidates$name_match <- vapply(
  seq_len(nrow(unique_best_candidates)),
  function(i) {
    
    if (is.na(unique_best_candidates$reference_name_count[i])) {
      return(NA)
    }
    
    if (unique_best_candidates$reference_name_count[i] != 1) {
      return(NA)
    }
    
    if (is.na(unique_best_candidates$experimental_parameter_name[i])) {
      return(NA)
    }
    
    if (is.na(unique_best_candidates$reference_name[i])) {
      return(NA)
    }
    
    identical(
      tolower(trimws(unique_best_candidates$experimental_parameter_name[i])),
      tolower(trimws(unique_best_candidates$reference_name[i]))
    )
  },
  logical(1)
)

print(table(unique_best_candidates$name_match, useNA = "ifany"))

print(
  unique_best_candidates[
    is.na(unique_best_candidates$name_match) |
      unique_best_candidates$name_match == FALSE,
  ],
  n = 60
)



# Count how many distinct reference IDs share each parameter name
reference_name_frequency <- parameter_description %>%
  mutate(
    normalised_name = tolower(trimws(name)),
    parameterId = trimws(parameterId)
  ) %>%
  filter(
    !is.na(normalised_name),
    normalised_name != "",
    !is.na(parameterId),
    parameterId != ""
  ) %>%
  group_by(normalised_name) %>%
  summarise(
    reference_ids_for_name = n_distinct(parameterId),
    reference_ids = paste(
      sort(unique(parameterId)),
      collapse = "; "
    ),
    .groups = "drop"
  )

# Join name specificity information to the 58 candidates
unique_best_candidates <- unique_best_candidates %>%
  mutate(
    normalised_name = tolower(
      trimws(experimental_parameter_name)
    )
  ) %>%
  left_join(
    reference_name_frequency,
    by = "normalised_name"
  )

print(
  table(
    unique_best_candidates$reference_ids_for_name,
    useNA = "ifany"
  )
)

print(
  unique_best_candidates %>%
    arrange(desc(reference_ids_for_name)) %>%
    select(
      parameter_stable_id,
      experimental_parameter_name,
      parameterId,
      reference_ids_for_name,
      edit_distance
    ),
  n = 58
)






library(dplyr)

# Extract family prefixes from experimental and reference IDs
extract_family <- function(x) {
  x <- toupper(gsub("[^A-Z0-9_]", "", x))
  sub("_[0-9].*$", "", x)
}

# Work with all 509 candidate rows
family_candidate_review <- ambiguous_candidates_detailed %>%
  mutate(
    experimental_family = extract_family(parameter_stable_id),
    reference_family = extract_family(parameterId),
    experimental_digits = vapply(
      parameter_stable_id,
      function(x) paste(
        regmatches(x, gregexpr("[0-9]+", x))[[1]],
        collapse = "-"
      ),
      character(1)
    ),
    reference_digits = vapply(
      parameterId,
      function(x) paste(
        regmatches(x, gregexpr("[0-9]+", x))[[1]],
        collapse = "-"
      ),
      character(1)
    )
  )

# Check the result
head(
  family_candidate_review[
    ,
    c(
      "parameter_stable_id",
      "experimental_family",
      "experimental_digits",
      "parameterId",
      "reference_family",
      "reference_digits",
      "experimental_parameter_name",
      "description",
      "edit_distance"
    )
  ],
  20
)


ambiguous_family_summary <- family_candidate_review %>%
  group_by(parameter_stable_id) %>%
  summarise(
    experimental_name = first(experimental_parameter_name),
    
    experimental_family = first(experimental_family),
    
    number_of_candidate_ids = n_distinct(parameterId),
    
    number_of_candidate_families = n_distinct(reference_family),
    
    candidate_families = paste(
      sort(unique(reference_family)),
      collapse = "; "
    ),
    
    candidate_ids = paste(
      sort(unique(parameterId)),
      collapse = "; "
    ),
    
    minimum_edit_distance = min(edit_distance),
    
    best_candidate_ids = paste(
      sort(unique(parameterId[
        edit_distance == min(edit_distance)
      ])),
      collapse = "; "
    ),
    
    .groups = "drop"
  )

print(ambiguous_family_summary, n = 85)

family_distribution <- ambiguous_family_summary %>%
  group_by(experimental_family) %>%
  summarise(
    distinct_experimental_ids = n(),
    candidate_reference_families = paste(
      sort(unique(unlist(strsplit(
        paste(candidate_families, collapse = "; "),
        "; "
      )))),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(distinct_experimental_ids))

print(family_distribution, n = 100)

family_description_review <- family_candidate_review %>%
  arrange(
    experimental_family,
    parameter_stable_id,
    edit_distance
  ) %>%
  select(
    parameter_stable_id,
    experimental_family,
    experimental_parameter_name,
    parameterId,
    reference_family,
    impcParameterOrigId,
    name.y,
    description,
    edit_distance,
    candidate_rank
  )

print(head(family_description_review, 100))

write.csv(
  family_candidate_review,
  "cleaned_output/validation/ambiguous_family_candidate_details.csv",
  row.names = FALSE
)

write.csv(
  ambiguous_family_summary,
  "cleaned_output/validation/ambiguous_family_summary.csv",
  row.names = FALSE
)

write.csv(
  family_distribution,
  "cleaned_output/validation/ambiguous_family_distribution.csv",
  row.names = FALSE
)

write.csv(
  family_description_review,
  "cleaned_output/validation/ambiguous_family_description_review.csv",
  row.names = FALSE
)


library(dplyr)

# 1. Correct the extracted family labels
family_candidate_review <- family_candidate_review %>%
  mutate(
    experimental_family = gsub(
      "^_+|_+$", "", experimental_family
    ),
    reference_family = gsub(
      "^_+|_+$", "", reference_family
    ),
    
    # Extract the final family component
    experimental_family_code = sub(
      "^.*_", "", experimental_family
    ),
    reference_family_code = sub(
      "^.*_", "", reference_family
    ),
    
    # Test whether the family codes correspond
    family_match = experimental_family_code ==
      reference_family_code
  )

# 2. Keep only family-matched candidates
family_matched_candidates <- family_candidate_review %>%
  filter(family_match)

# 3. Summarise the candidates for each experimental ID
family_matched_summary <- family_matched_candidates %>%
  group_by(parameter_stable_id) %>%
  summarise(
    experimental_name = first(experimental_parameter_name),
    experimental_family = first(experimental_family),
    number_of_family_matched_ids = n_distinct(parameterId),
    family_matched_reference_ids = paste(
      sort(unique(parameterId)),
      collapse = "; "
    ),
    minimum_edit_distance = min(edit_distance),
    best_family_matched_ids = paste(
      sort(unique(parameterId[
        edit_distance == min(edit_distance)
      ])),
      collapse = "; "
    ),
    .groups = "drop"
  )

# 4. Identify experimental IDs with no family-matched candidate
family_unmatched_ids <- ambiguous_family_summary %>%
  select(parameter_stable_id, experimental_name) %>%
  anti_join(
    family_matched_summary,
    by = "parameter_stable_id"
  )

# 5. Review the results
print(family_matched_summary, n = 100)
print(family_unmatched_ids, n = 100)

# 6. Count how many IDs have family-matched candidates
cat(
  "Experimental IDs with family-matched candidates:",
  nrow(family_matched_summary),
  "\n"
)

cat(
  "Experimental IDs without family-matched candidates:",
  nrow(family_unmatched_ids),
  "\n"
)

# 7. Save the results
write.csv(
  family_matched_candidates,
  "cleaned_output/validation/family_matched_candidate_details.csv",
  row.names = FALSE
)

write.csv(
  family_matched_summary,
  "cleaned_output/validation/family_matched_candidate_summary.csv",
  row.names = FALSE
)

write.csv(
  family_unmatched_ids,
  "cleaned_output/validation/family_unmatched_ids.csv",
  row.names = FALSE
)

library(dplyr)

# 1. Remove duplicate candidate rows
family_name_review <- family_matched_candidates %>%
  distinct(
    parameter_stable_id,
    parameterId,
    .keep_all = TRUE
  ) %>%
  mutate(
    experimental_name_normalised = tolower(
      gsub(
        "[^a-z0-9]",
        "",
        trimws(experimental_parameter_name)
      )
    ),
    reference_name_normalised = tolower(
      gsub(
        "[^a-z0-9]",
        "",
        trimws(name.y)
      )
    ),
    exact_name_match =
      !is.na(experimental_name_normalised) &
      !is.na(reference_name_normalised) &
      experimental_name_normalised != "" &
      experimental_name_normalised == reference_name_normalised
  )

# 2. Summarise each experimental ID
family_name_summary <- family_name_review %>%
  group_by(parameter_stable_id) %>%
  summarise(
    experimental_name = first(experimental_parameter_name),
    experimental_family = first(experimental_family),
    number_of_reference_candidates = n_distinct(parameterId),
    number_of_exact_name_matches = sum(exact_name_match),
    matching_reference_ids = paste(
      sort(unique(parameterId[exact_name_match])),
      collapse = "; "
    ),
    all_reference_candidates = paste(
      sort(unique(parameterId)),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    evidence_category = case_when(
      number_of_exact_name_matches == 1 ~
        "One family and name supported candidate",
      
      number_of_exact_name_matches > 1 ~
        "Multiple family and name supported candidates",
      
      TRUE ~
        "Family match without exact name match"
    )
  )

# 3. Inspect the classification
print(family_name_summary, n = 100)

# 4. Count the evidence categories
family_name_summary %>%
  count(evidence_category) %>%
  print()

# 5. Save the results
write.csv(
  family_name_review,
  "cleaned_output/validation/family_name_candidate_details.csv",
  row.names = FALSE
)

write.csv(
  family_name_summary,
  "cleaned_output/validation/family_name_candidate_summary.csv",
  row.names = FALSE
)




# ============================================================
# FIX DISPLAY ERROR AND OPEN THE TWO REVIEW TABLES
# ============================================================

# 1. Inspect and reset the NA printing option
options(na.print = "NA")

# Verify the setting without using print()
cat(
  "Current na.print setting:",
  dput(getOption("na.print")),
  "\n"
)

# 2. Check that both result objects exist
if (!exists("multiple_match_details")) {
  stop("multiple_match_details does not exist.")
}

if (!exists("no_name_match_details")) {
  stop("no_name_match_details does not exist.")
}

# 3. Save both tables
dir.create(
  "cleaned_output/validation",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  multiple_match_details,
  "cleaned_output/validation/multiple_family_name_match_review.csv",
  row.names = FALSE,
  na = "NA"
)

write.csv(
  no_name_match_details,
  "cleaned_output/validation/no_exact_name_match_review.csv",
  row.names = FALSE,
  na = "NA"
)

# 4. Display table dimensions without printing data frames
cat(
  "\nMultiple-match table:",
  nrow(multiple_match_details), "rows and",
  ncol(multiple_match_details), "columns\n"
)

cat(
  "Name-mismatch table:",
  nrow(no_name_match_details), "rows and",
  ncol(no_name_match_details), "columns\n"
)

# 5. Open tables in RStudio Data Viewer
View(multiple_match_details)
View(no_name_match_details)

cat("\nBoth CSV files have been saved.\n")

# ============================================================
# INVESTIGATE THE FOUR APPARENT NAME-MATCH DISCREPANCIES
# ============================================================

diagnostic_name_check <- family_name_review %>%
  semi_join(
    no_name_match_ids,
    by = "parameter_stable_id"
  ) %>%
  distinct(
    parameter_stable_id,
    parameterId,
    .keep_all = TRUE
  ) %>%
  mutate(
    experimental_name_normalised = tolower(
      gsub(
        "[^a-z0-9]",
        "",
        trimws(experimental_parameter_name)
      )
    ),
    reference_name_normalised = tolower(
      gsub(
        "[^a-z0-9]",
        "",
        trimws(name.y)
      )
    ),
    recalculated_name_match =
      !is.na(experimental_name_normalised) &
      !is.na(reference_name_normalised) &
      experimental_name_normalised != "" &
      experimental_name_normalised == reference_name_normalised
  ) %>%
  select(
    parameter_stable_id,
    experimental_parameter_name,
    name.y,
    experimental_name_normalised,
    reference_name_normalised,
    exact_name_match,
    recalculated_name_match,
    parameterId,
    description
  )

# Save diagnostic results
write.csv(
  diagnostic_name_check,
  "cleaned_output/validation/four_name_match_discrepancy_diagnostic.csv",
  row.names = FALSE,
  na = "NA"
)

# Open the table in RStudio
View(diagnostic_name_check)

cat(
  "Rows investigated:",
  nrow(diagnostic_name_check),
  "\n"
)

cat(
  "Rows matching after recalculation:",
  sum(diagnostic_name_check$recalculated_name_match, na.rm = TRUE),
  "\n"
)

# Inspect the original parameter names
name_character_check <- family_name_review %>%
  semi_join(
    no_name_match_ids,
    by = "parameter_stable_id"
  ) %>%
  distinct(
    parameter_stable_id,
    experimental_parameter_name,
    name.y
  ) %>%
  mutate(
    experimental_raw = dQuote(experimental_parameter_name),
    reference_raw = dQuote(name.y),
    experimental_characters = vapply(
      experimental_parameter_name,
      function(x) paste(utf8ToInt(x), collapse = "-"),
      character(1)
    ),
    reference_characters = vapply(
      name.y,
      function(x) paste(utf8ToInt(x), collapse = "-"),
      character(1)
    )
  )

View(name_character_check)

write.csv(
  name_character_check,
  "cleaned_output/validation/name_character_inspection.csv",
  row.names = FALSE
)

library(dplyr)

# 1. Correct name normalisation
family_name_review <- family_matched_candidates %>%
  distinct(
    parameter_stable_id,
    parameterId,
    .keep_all = TRUE
  ) %>%
  mutate(
    experimental_name_normalised = gsub(
      "[^a-z0-9]",
      "",
      tolower(trimws(experimental_parameter_name))
    ),
    reference_name_normalised = gsub(
      "[^a-z0-9]",
      "",
      tolower(trimws(name.y))
    ),
    exact_name_match =
      !is.na(experimental_name_normalised) &
      !is.na(reference_name_normalised) &
      experimental_name_normalised != "" &
      experimental_name_normalised == reference_name_normalised
  )

# 2. Recalculate the summary for all family-matched IDs
family_name_summary <- family_name_review %>%
  group_by(parameter_stable_id) %>%
  summarise(
    experimental_name = first(experimental_parameter_name),
    experimental_family = first(experimental_family),
    number_of_reference_candidates = n_distinct(parameterId),
    number_of_exact_name_matches = sum(exact_name_match),
    matching_reference_ids = paste(
      sort(unique(parameterId[exact_name_match])),
      collapse = "; "
    ),
    all_reference_candidates = paste(
      sort(unique(parameterId)),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    evidence_category = case_when(
      number_of_exact_name_matches == 1 ~
        "One family and name supported candidate",
      number_of_exact_name_matches > 1 ~
        "Multiple family and name supported candidates",
      TRUE ~
        "Family match without exact name match"
    )
  )

# 3. Display the corrected category counts
category_counts <- family_name_summary %>%
  count(evidence_category)

cat("\nCORRECTED CATEGORY COUNTS\n")
View(category_counts)

# 4. Identify remaining multiple matches
multiple_match_ids <- family_name_summary %>%
  filter(number_of_exact_name_matches > 1)

multiple_match_details <- family_name_review %>%
  semi_join(
    multiple_match_ids,
    by = "parameter_stable_id"
  ) %>%
  arrange(parameter_stable_id, edit_distance)

# 5. Identify remaining name mismatches
no_name_match_ids <- family_name_summary %>%
  filter(number_of_exact_name_matches == 0)

no_name_match_details <- family_name_review %>%
  semi_join(
    no_name_match_ids,
    by = "parameter_stable_id"
  ) %>%
  arrange(parameter_stable_id, edit_distance)

# 6. Save all corrected results
write.csv(
  family_name_review,
  "cleaned_output/validation/family_name_candidate_details_corrected.csv",
  row.names = FALSE
)

write.csv(
  family_name_summary,
  "cleaned_output/validation/family_name_candidate_summary_corrected.csv",
  row.names = FALSE
)

write.csv(
  category_counts,
  "cleaned_output/validation/family_name_category_counts_corrected.csv",
  row.names = FALSE
)



# ============================================================
# RELOAD AND INSPECT ORIGINAL EXPERIMENTAL METADATA
# ============================================================

options(na.print = "NA")

# 1. Locate the saved RDS file
rds_path <- "cleaned_output/raw_records_imported.rds"

if (!file.exists(rds_path)) {
  stop(
    "File not found: ", rds_path,
    "\nCheck your working directory using getwd()."
  )
}

# 2. Reload the original imported records
raw_records_imported <- readRDS(rds_path)

cat("RDS FILE LOADED SUCCESSFULLY\n")

# 3. Inspect the object class
cat("\nOBJECT CLASS:\n")
cat(paste(class(raw_records_imported), collapse = ", "), "\n")

# 4. Inspect its dimensions or length
cat("\nOBJECT DIMENSIONS / LENGTH:\n")

if (is.data.frame(raw_records_imported)) {
  cat("Rows:", nrow(raw_records_imported), "\n")
  cat("Columns:", ncol(raw_records_imported), "\n")
} else {
  cat("Length:", length(raw_records_imported), "\n")
}

# 5. Inspect its structure
cat("\nOBJECT STRUCTURE:\n")
str(raw_records_imported, max.level = 2)

# 6. Inspect the first element if it is a list
if (is.list(raw_records_imported) &&
    !is.data.frame(raw_records_imported) &&
    length(raw_records_imported) > 0) {
  
  cat("\nFIRST ELEMENT:\n")
  str(raw_records_imported[[1]], max.level = 2)
}

# 7. Display column names if it is a data frame
if (is.data.frame(raw_records_imported)) {
  cat("\nCOLUMN NAMES:\n")
  dput(names(raw_records_imported))
}

cat("\nMETADATA INSPECTION COMPLETED\n")

# ============================================================
# CHECK ORIGINAL SOURCE FILE MAPPING
# ============================================================

source_path <- "cleaned_output/source_file_list.rds"

if (!file.exists(source_path)) {
  stop("Source file list not found: ", source_path)
}

source_file_list <- readRDS(source_path)

cat("SOURCE FILE LIST CLASS:\n")
print(class(source_file_list))

cat("\nSOURCE FILE LIST LENGTH:\n")
print(length(source_file_list))

cat("\nSOURCE FILE LIST STRUCTURE:\n")
str(source_file_list, max.level = 1)

cat("\nNUMBER OF IMPORTED RECORDS:\n")
print(length(raw_records_imported))

cat("\nFIRST FIVE SOURCE FILE ENTRIES:\n")
print(head(source_file_list, 5))

cat("\nFIRST FIVE IMPORTED RECORDS:\n")
for (i in 1:5) {
  cat("\nRecord:", i, "\n")
  print(head(raw_records_imported[[i]], 6))
}

cat("\nSOURCE FILE MAPPING INSPECTION COMPLETED\n")

# ============================================================
# RECONSTRUCT ORIGINAL EXPERIMENTAL METADATA
# AND EXTRACT THE 45 FAMILY-MATCHED IDS
# ============================================================

library(dplyr)

options(na.print = "NA")

# ------------------------------------------------------------
# 1. Verify source objects
# ------------------------------------------------------------

stopifnot(
  length(raw_records_imported) == length(source_file_list)
)

stopifnot(
  exists("family_name_summary")
)

cat("Imported records:", length(raw_records_imported), "\n")
cat("Source filenames:", length(source_file_list), "\n")

# ------------------------------------------------------------
# 2. Convert each key-value record into a wide data frame
# ------------------------------------------------------------

metadata_list <- lapply(seq_along(raw_records_imported), function(i) {
  
  record <- raw_records_imported[[i]]
  
  # Preserve original field names, making duplicates unique
  field_names <- make.unique(as.character(record$field))
  
  record_values <- as.list(as.character(record$value))
  names(record_values) <- field_names
  
  # Retain the original source filename
  record_values$source_file <- source_file_list[i]
  
  # Extract analysis ID from filename
  record_values$source_analysis_id <- sub(
    "\\.csv$",
    "",
    basename(source_file_list[i])
  )
  
  as.data.frame(
    record_values,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
})

# Combine records, retaining fields that occur in only some files
experimental_metadata <- bind_rows(metadata_list)

cat("\nRECONSTRUCTED METADATA\n")
cat("Rows:", nrow(experimental_metadata), "\n")
cat("Columns:", ncol(experimental_metadata), "\n")

# ------------------------------------------------------------
# 3. Check source filename and analysis ID agreement
# ------------------------------------------------------------

if ("analysis_id" %in% names(experimental_metadata)) {
  
  experimental_metadata <- experimental_metadata %>%
    mutate(
      analysis_id_check = ifelse(
        is.na(analysis_id) | trimws(analysis_id) == "",
        "Missing analysis_id",
        ifelse(
          trimws(analysis_id) == source_analysis_id,
          "Match",
          "Mismatch"
        )
      )
    )
  
  cat("\nANALYSIS ID CHECK\n")
  
  print(
    experimental_metadata %>%
      count(analysis_id_check)
  )
  
} else {
  
  cat("\nNo analysis_id field found in reconstructed records.\n")
}

# ------------------------------------------------------------
# 4. Check the parameter ID field
# ------------------------------------------------------------

if (!"parameter_stable_id" %in% names(experimental_metadata)) {
  stop("parameter_stable_id was not found in reconstructed metadata.")
}

# ------------------------------------------------------------
# 5. Extract the 45 family-matched experimental IDs
# ------------------------------------------------------------

target_ids <- unique(family_name_summary$parameter_stable_id)

family_metadata <- experimental_metadata %>%
  filter(parameter_stable_id %in% target_ids)

cat("\n45-ID METADATA EXTRACTION\n")
cat("Target experimental IDs:", length(target_ids), "\n")
cat("Extracted records:", nrow(family_metadata), "\n")
cat(
  "Distinct extracted IDs:",
  n_distinct(family_metadata$parameter_stable_id),
  "\n"
)

# Identify target IDs with no extracted records
missing_metadata_ids <- tibble(
  parameter_stable_id = target_ids
) %>%
  anti_join(
    family_metadata %>%
      distinct(parameter_stable_id),
    by = "parameter_stable_id"
  )

cat(
  "Target IDs without extracted records:",
  nrow(missing_metadata_ids),
  "\n"
)

# ------------------------------------------------------------
# 6. Summarise available metadata fields
# ------------------------------------------------------------

metadata_field_summary <- tibble(
  field = names(family_metadata),
  non_missing_values = vapply(
    family_metadata,
    function(x) sum(!is.na(x) & trimws(as.character(x)) != ""),
    integer(1)
  )
) %>%
  arrange(desc(non_missing_values))

# ------------------------------------------------------------
# 7. Save reconstructed and extracted datasets
# ------------------------------------------------------------

dir.create(
  "cleaned_output/validation",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  experimental_metadata,
  "cleaned_output/experimental_metadata_reconstructed.rds"
)

write.csv(
  family_metadata,
  "cleaned_output/validation/family_matched_original_metadata.csv",
  row.names = FALSE,
  na = "NA"
)

write.csv(
  missing_metadata_ids,
  "cleaned_output/validation/family_matched_ids_missing_metadata.csv",
  row.names = FALSE
)

write.csv(
  metadata_field_summary,
  "cleaned_output/validation/reconstructed_metadata_field_summary.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. Display validation results
# ------------------------------------------------------------

cat("\nMETADATA FIELD SUMMARY\n")
print(metadata_field_summary, n = Inf)

cat("\nFIRST TEN EXTRACTED RECORDS\n")
print(
  family_metadata %>%
    select(
      any_of(c(
        "source_analysis_id",
        "analysis_id",
        "parameter_stable_id",
        "gene_accession_id",
        "gene_symbol",
        "allele_symbol",
        "strain_name",
        "mouse_life_stage",
        "parameter_name",
        "source_file"
      ))
    ) %>%
    head(10),
  width = Inf
)

cat("\nFILES SAVED SUCCESSFULLY\n")
cat("Reconstructed metadata: cleaned_output/experimental_metadata_reconstructed.rds\n")
cat("45-ID metadata: cleaned_output/validation/family_matched_original_metadata.csv\n")
cat("Missing IDs: cleaned_output/validation/family_matched_ids_missing_metadata.csv\n")
cat("Field summary: cleaned_output/validation/reconstructed_metadata_field_summary.csv\n")


write.csv(
  multiple_match_details,
  "cleaned_output/validation/multiple_family_name_match_review_corrected.csv",
  row.names = FALSE
)

write.csv(
  no_name_match_details,
  "cleaned_output/validation/no_exact_name_match_review_corrected.csv",
  row.names = FALSE
)

# 7. Open the corrected results
View(family_name_summary)
View(multiple_match_details)
View(no_name_match_details)

cat("\nCorrected comparison completed and all files saved.\n")


# ============================================================
# VERIFY METADATA EXTRACTION AND OPEN RESULTS
# ============================================================

options(na.print = "NA")

# 1. Check reconstructed metadata
cat(
  "Reconstructed metadata:",
  nrow(experimental_metadata), "records;",
  ncol(experimental_metadata), "fields\n"
)

# 2. Check extracted records
cat(
  "Extracted records:",
  nrow(family_metadata), "\n"
)

cat(
  "Distinct experimental IDs:",
  length(unique(family_metadata$parameter_stable_id)),
  "\n"
)

cat(
  "Missing target IDs:",
  nrow(missing_metadata_ids),
  "\n"
)

# 3. Check analysis ID validation
if ("analysis_id_check" %in% names(experimental_metadata)) {
  analysis_id_counts <- as.data.frame(
    table(experimental_metadata$analysis_id_check)
  )
  names(analysis_id_counts) <- c("validation_status", "records")
  
  View(analysis_id_counts)
}

# 4. Open metadata field summary
View(metadata_field_summary)

# 5. Open extracted original metadata
View(family_metadata)

# 6. Open missing ID list
View(missing_metadata_ids)

# 7. Verify saved files
files_to_check <- c(
  "cleaned_output/experimental_metadata_reconstructed.rds",
  "cleaned_output/validation/family_matched_original_metadata.csv",
  "cleaned_output/validation/family_matched_ids_missing_metadata.csv",
  "cleaned_output/validation/reconstructed_metadata_field_summary.csv"
)

file_check <- data.frame(
  file = files_to_check,
  exists = file.exists(files_to_check)
)

View(file_check)

cat("\nMetadata verification completed.\n")

# ============================================================
# NEXT STEP: INSPECT ORIGINAL METADATA FOR THE 45 IDs
# ============================================================

# 1. Confirm available columns
cat("FAMILY METADATA DIMENSIONS:\n")
print(dim(family_metadata))

cat("\nFAMILY METADATA COLUMN NAMES:\n")
print(names(family_metadata))

# 2. Check how many unique experimental IDs are represented
cat("\nUNIQUE EXPERIMENTAL IDs:\n")
print(length(unique(family_metadata$parameter_stable_id)))

# 3. Check whether any IDs have multiple original records
metadata_id_counts <- family_metadata %>%
  count(parameter_stable_id, name = "metadata_record_count") %>%
  arrange(desc(metadata_record_count))

cat("\nIDs WITH MULTIPLE METADATA RECORDS:\n")
print(
  metadata_id_counts[
    metadata_id_counts$metadata_record_count > 1,
  ],
  row.names = FALSE
)

# 4. Inspect the relevant original metadata fields
metadata_fields_to_inspect <- intersect(
  c(
    "parameter_stable_id",
    "parameter_name",
    "gene_accession_id",
    "gene_symbol",
    "strain_name",
    "allele_symbol",
    "mouse_life_stage",
    "source_file"
  ),
  names(family_metadata)
)

metadata_review <- family_metadata[
  ,
  metadata_fields_to_inspect,
  drop = FALSE
]

# 5. Display and save
View(metadata_review)

write.csv(
  metadata_review,
  "cleaned_output/validation/original_metadata_for_family_matched_ids.csv",
  row.names = FALSE,
  na = "NA"
)

cat("\nOriginal metadata review table saved.\n")

# Restore R's default missing-value display setting
options(na.print = "NA")

# Check the result
getOption("na.print")

# Display the duplicate metadata IDs
print(as.data.frame(metadata_duplicates), row.names = FALSE)
# How many IDs have repeated metadata records?
cat(
  "\nNumber of repeated IDs:",
  nrow(metadata_duplicates),
  "\n"
)

# How many metadata records are involved?
cat(
  "Total records belonging to repeated IDs:",
  sum(metadata_duplicates$number_of_records),
  "\n"
)

# Save the duplicate summary
write.csv(
  metadata_duplicates,
  "cleaned_output/validation/repeated_metadata_id_summary.csv",
  row.names = FALSE,
  na = "NA"
)

# ============================================================
# INSPECT REPEATED ORIGINAL METADATA RECORDS
# ============================================================

options(na.print = "NA")

# Extract all records belonging to duplicated IDs
repeated_metadata_records <- family_metadata %>%
  semi_join(
    metadata_duplicates,
    by = "parameter_stable_id"
  ) %>%
  arrange(parameter_stable_id)

# Display the four records
cat("\nRepeated metadata records:\n")
print(as.data.frame(repeated_metadata_records), row.names = FALSE)

# Open in RStudio Data Viewer
View(repeated_metadata_records)

# ============================================================
# CHECK WHETHER THE DUPLICATED RECORDS ARE IDENTICAL
# ============================================================

duplicate_comparison <- repeated_metadata_records %>%
  group_by(parameter_stable_id) %>%
  summarise(
    number_of_records = n(),
    across(
      everything(),
      ~ n_distinct(.x, na.rm = FALSE),
      .names = "distinct_{.col}"
    ),
    .groups = "drop"
  )

cat("\nDuplicate comparison:\n")
print(as.data.frame(duplicate_comparison), row.names = FALSE)

# ============================================================
# SAVE BOTH REVIEW TABLES
# ============================================================

write.csv(
  repeated_metadata_records,
  "cleaned_output/validation/repeated_original_metadata_records.csv",
  row.names = FALSE,
  na = "NA"
)

write.csv(
  duplicate_comparison,
  "cleaned_output/validation/repeated_metadata_comparison.csv",
  row.names = FALSE,
  na = "NA"
)

cat("\nDuplicate metadata inspection saved successfully.\n")


options(na.print = "NA")

# ============================================================
# 1. SELECT IMPORTANT METADATA FIELDS
# ============================================================

inspection_columns <- intersect(
  c(
    "parameter_stable_id",
    "analysis_id",
    "source_analysis_id",
    "analysis_id_check",
    "source_file",
    "gene_accession_id",
    "gene_symbol",
    "allele_symbol",
    "strain_name",
    "p_value",
    "parameter_name",
    "mouse_life_stage"
  ),
  names(repeated_metadata_records)
)

# ============================================================
# 2. INSPECT THE UCD RECORDS
# ============================================================

ucd_records <- repeated_metadata_records %>%
  filter(parameter_stable_id == "UCD_OWT(_003_001") %>%
  select(all_of(inspection_columns))

cat("\nUCD repeated ID records:\n")
print(as.data.frame(ucd_records), row.names = FALSE)

# ============================================================
# 3. INSPECT THE MGP RECORDS
# ============================================================

mgp_records <- repeated_metadata_records %>%
  filter(parameter_stable_id == "_MGP_ALZ_096_001") %>%
  select(all_of(inspection_columns))

cat("\nMGP repeated ID records:\n")
print(as.data.frame(mgp_records), row.names = FALSE)

# ============================================================
# 4. SAVE THE TWO COMPARISONS
# ============================================================

write.csv(
  ucd_records,
  "cleaned_output/validation/UCD_OWT_duplicate_investigation.csv",
  row.names = FALSE,
  na = "NA"
)

write.csv(
  mgp_records,
  "cleaned_output/validation/MGP_ALZ_duplicate_investigation.csv",
  row.names = FALSE,
  na = "NA"
)

cat("\nBoth source-record comparisons saved.\n")

# ============================================================
# UCD SOURCE FILE DIFFERENCE ANALYSIS
# ============================================================

library(dplyr)

# Compare by field and value
ucd_comparison <- full_join(
  ucd_raw_1 %>%
    rename(value_file_1 = value),
  ucd_raw_2 %>%
    rename(value_file_2 = value),
  by = "field"
) %>%
  mutate(
    value_file_1 = as.character(value_file_1),
    value_file_2 = as.character(value_file_2),
    values_identical = coalesce(
      value_file_1 == value_file_2,
      FALSE
    )
  )

# Show only differing fields
ucd_differences <- ucd_comparison %>%
  filter(!values_identical)

cat("\nNumber of differing fields:\n")
cat(nrow(ucd_differences), "\n")

cat("\nDiffering UCD fields:\n")
print(as.data.frame(ucd_differences), row.names = FALSE)

# Save the complete comparison and differences
write.csv(
  ucd_comparison,
  "cleaned_output/validation/UCD_raw_file_full_comparison.csv",
  row.names = FALSE,
  na = "NA"
)

write.csv(
  ucd_differences,
  "cleaned_output/validation/UCD_raw_file_differences.csv",
  row.names = FALSE,
  na = "NA"
)

cat("\nUCD comparison files saved.\n")
# ============================================================
# UCD SOURCE FILE INVESTIGATION SUMMARY
# ============================================================

ucd_source_summary <- data.frame(
  file_1 = "an3209827559.csv",
  file_2 = "an8513676512.csv",
  same_parameter_id = TRUE,
  same_parameter_name = TRUE,
  same_gene = FALSE,
  same_allele = FALSE,
  same_p_value = FALSE,
  same_analysis_id = FALSE,
  number_of_differing_fields = nrow(ucd_differences),
  classification = "Distinct experimental records",
  action = "Retain both records; flag anomalous LacZ values for review",
  stringsAsFactors = FALSE
)

write.csv(
  ucd_source_summary,
  "cleaned_output/validation/UCD_source_investigation_summary.csv",
  row.names = FALSE,
  na = "NA"
)

print(ucd_source_summary)




# ============================================================
# ANALYSIS ID MISMATCH INVESTIGATION
# ============================================================

# Extract records flagged as mismatches
analysis_id_mismatches <- experimental_metadata %>%
  filter(analysis_id_check == "Mismatch") %>%
  select(
    source_file,
    source_analysis_id,
    analysis_id,
    analysis_id_check
  ) %>%
  mutate(
    filename_without_extension = sub(
      "\\.csv$",
      "",
      basename(source_file)
    ),
    filename_without_duplicate_suffix = sub(
      "_[0-9]+$",
      "",
      filename_without_extension
    ),
    suffix_explains_mismatch =
      source_analysis_id == filename_without_duplicate_suffix
  )

# Summarise the findings
cat("\nTotal mismatches:\n")
cat(nrow(analysis_id_mismatches), "\n")

cat("\nMismatch classification:\n")
print(
  as.data.frame(
    table(
      analysis_id_mismatches$suffix_explains_mismatch,
      useNA = "ifany"
    )
  ),
  row.names = FALSE
)

# Inspect all mismatches
print(
  as.data.frame(analysis_id_mismatches),
  row.names = FALSE
)

# Save the investigation
write.csv(
  analysis_id_mismatches,
  "cleaned_output/validation/analysis_id_mismatch_investigation.csv",
  row.names = FALSE,
  na = "NA"
)

# ============================================================
# INSPECT THE UNRESOLVED SOURCE FILE
# ============================================================

unresolved_file <- "group11_data/an8392160234.csv"

# Read the original file without modifying it
unresolved_raw <- read.csv(
  unresolved_file,
  header = FALSE,
  col.names = c("field", "value"),
  colClasses = "character",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = NULL
)

# Display the analysis ID and surrounding metadata
print(
  as.data.frame(
    unresolved_raw %>%
      filter(
        field %in% c(
          "analysis_id",
          "gene_accession_id",
          "gene_symbol",
          "allele_symbol",
          "parameter_stable_id",
          "parameter_name"
        )
      )
  ),
  row.names = FALSE
)

# Save the source-level inspection
write.csv(
  unresolved_raw,
  "cleaned_output/validation/an8392160234_raw_inspection.csv",
  row.names = FALSE,
  na = "NA"
)

# ============================================================
# RECORD FINAL ANALYSIS ID ANOMALY
# ============================================================

analysis_id_anomaly <- data.frame(
  source_file = "an8392160234.csv",
  original_analysis_id = "8392160234",
  filename_derived_id = "an8392160234",
  gene_symbol = "Cmtr2",
  gene_accession_id = "MGI:2384580",
  parameter_stable_id = "IMPC_GRS_008_001",
  parameter_name = "Forelimb grip strength measurement mean",
  allele_symbol = NA_character_,
  observed_issue = "Internal analysis_id lacks the 'an' prefix present in filename",
  proposed_interpretation = "Possible formatting omission",
  correction_status = "Not corrected",
  action = "Preserve original value and flag for review",
  stringsAsFactors = FALSE
)

write.csv(
  analysis_id_anomaly,
  "cleaned_output/validation/analysis_id_single_anomaly_review.csv",
  row.names = FALSE,
  na = "NA"
)


# ============================================================
# GROUP 11: CONSOLIDATED DATA VALIDATION SUMMARY
# ============================================================

validation_summary <- data.frame(
  
  validation_area = c(
    "Experimental record import",
    "Analysis ID validation",
    "Analysis ID filename suffixes",
    "Analysis ID missing-prefix anomaly",
    "MGP source-file comparison",
    "UCD source-file comparison",
    "UCD LacZ value inconsistencies",
    "Parameter-description reference validation",
    "Procedure reference validation",
    "Stable parameter ID consistency"
  ),
  
  finding = c(
    "28,670 experimental records imported",
    "28,587 matches, 80 mismatches and 3 missing analysis IDs",
    "79 mismatches explained by filenames ending in _1",
    "One internal analysis ID lacks the an prefix",
    "Two source files contain identical 20-row records",
    "Two files represent distinct experiments with 11 differing fields",
    "Six LacZ fields differ between the UCD records",
    "5,325 original rows; 14 duplicate original-ID pairs removed",
    "5,311 procedure records; one-to-one mapping after deduplication",
    "21 stable parameter IDs have differing names or descriptions"
  ),
  
  classification = c(
    "Imported",
    "Investigated",
    "Filename-related discrepancy",
    "Formatting anomaly",
    "Exact duplicate",
    "Distinct experimental records",
    "Requires review",
    "Validated after deduplication",
    "Validated",
    "Requires review"
  ),
  
  action = c(
    "Retain imported records",
    "Preserve original metadata and validation results",
    "Retain source files and distinguish filename suffixes",
    "Preserve original value; do not automatically correct",
    "Retain raw files and flag duplicate for downstream handling",
    "Retain both experimental records",
    "Preserve original observations and flag unusual values",
    "Use original parameter ID as the reference key",
    "Retain validated reference mapping",
    "Preserve records and investigate inconsistencies"
  ),
  
  status = c(
    "Completed",
    "Completed",
    "Completed",
    "Completed - flagged",
    "Completed",
    "Completed",
    "Completed - flagged",
    "Completed",
    "Completed",
    "Completed - flagged"
  ),
  
  stringsAsFactors = FALSE
)

# Save consolidated summary
write.csv(
  validation_summary,
  "cleaned_output/validation/consolidated_validation_summary.csv",
  row.names = FALSE,
  na = "NA"
)

# Display summary
print(
  as.data.frame(validation_summary),
  row.names = FALSE
)

cat("\nConsolidated validation summary saved.\n")



# ============================================================
# EXPERIMENTAL METADATA: MISSING AND MALFORMED VALUE AUDIT
# ============================================================

required_fields <- c(
  "analysis_id",
  "gene_accession_id",
  "gene_symbol",
  "strain_name",
  "mouse_life_stage",
  "parameter_stable_id",
  "parameter_name",
  "p_value",
  "allele_symbol"
)

# Check which SOP fields exist
missing_columns <- setdiff(
  required_fields,
  names(experimental_metadata)
)

cat("\nRequired SOP fields absent from metadata:\n")
print(missing_columns)

# Audit available required fields
available_fields <- intersect(
  required_fields,
  names(experimental_metadata)
)

missing_value_summary <- data.frame(
  field = available_fields,
  missing_values = vapply(
    available_fields,
    function(x) {
      values <- as.character(experimental_metadata[[x]])
      sum(
        is.na(values) |
          trimws(values) == "" |
          toupper(trimws(values)) == "NA"
      )
    },
    integer(1)
  ),
  total_records = nrow(experimental_metadata),
  stringsAsFactors = FALSE
)

missing_value_summary$percentage_missing <-
  round(
    100 * missing_value_summary$missing_values /
      missing_value_summary$total_records,
    3
  )

print(
  as.data.frame(missing_value_summary),
  row.names = FALSE
)

# Save audit
write.csv(
  missing_value_summary,
  "cleaned_output/validation/required_field_missing_value_audit.csv",
  row.names = FALSE,
  na = "NA"
)



# ============================================================
# MISSING VALUE INVESTIGATION
# ============================================================

library(dplyr)

required_fields <- c(
  "analysis_id",
  "gene_accession_id",
  "gene_symbol",
  "strain_name",
  "mouse_life_stage",
  "parameter_stable_id",
  "parameter_name",
  "p_value",
  "allele_symbol"
)

# ------------------------------------------------------------
# 1. Classify missing values by type
# ------------------------------------------------------------

missing_type_summary <- do.call(
  rbind,
  lapply(required_fields, function(field_name) {
    
    values <- as.character(
      experimental_metadata[[field_name]]
    )
    
    data.frame(
      field = field_name,
      actual_R_NA = sum(is.na(values)),
      blank_or_whitespace = sum(
        !is.na(values) & trimws(values) == ""
      ),
      literal_NA = sum(
        !is.na(values) &
          toupper(trimws(values)) == "NA"
      ),
      stringsAsFactors = FALSE
    )
  })
)

print(
  as.data.frame(missing_type_summary),
  row.names = FALSE
)

write.csv(
  missing_type_summary,
  "cleaned_output/validation/missing_value_type_summary.csv",
  row.names = FALSE,
  na = "NA"
)

# ------------------------------------------------------------
# 2. Identify every record with at least one missing field
# ------------------------------------------------------------

missing_matrix <- sapply(
  required_fields,
  function(field_name) {
    
    values <- as.character(
      experimental_metadata[[field_name]]
    )
    
    is.na(values) |
      trimws(values) == "" |
      toupper(trimws(values)) == "NA"
  }
)

missing_record_indices <- which(
  rowSums(missing_matrix) > 0
)

missing_records <- experimental_metadata[
  missing_record_indices,
  ,
  drop = FALSE
]

# Add the list of missing fields for each record
missing_records$missing_fields <- apply(
  missing_matrix[missing_record_indices, , drop = FALSE],
  1,
  function(x) {
    paste(required_fields[x], collapse = "; ")
  }
)

cat("\nRecords with at least one missing required field:\n")
cat(nrow(missing_records), "\n")

write.csv(
  missing_records,
  "cleaned_output/validation/records_with_missing_required_fields.csv",
  row.names = FALSE,
  na = "NA"
)

# ------------------------------------------------------------
# 3. Check whether missing parameter IDs match
#    the previously identified 282 records
# ------------------------------------------------------------

parameter_id_values <- as.character(
  experimental_metadata$parameter_stable_id
)

missing_parameter_id_indices <- which(
  is.na(parameter_id_values) |
    trimws(parameter_id_values) == "" |
    toupper(trimws(parameter_id_values)) == "NA"
)

cat("\nMissing parameter_stable_id records:\n")
cat(length(missing_parameter_id_indices), "\n")

cat("\nMissing parameter IDs represented by literal NA:\n")
cat(sum(
  !is.na(parameter_id_values) &
    toupper(trimws(parameter_id_values)) == "NA"
), "\n")

# ------------------------------------------------------------
# 4. Investigate missing p-values and numeric validity
# ------------------------------------------------------------

pvalue_text <- as.character(
  experimental_metadata$p_value
)

pvalue_numeric <- suppressWarnings(
  as.numeric(trimws(pvalue_text))
)

pvalue_missing <- (
  is.na(pvalue_text) |
    trimws(pvalue_text) == "" |
    toupper(trimws(pvalue_text)) == "NA"
)

pvalue_non_numeric <- (
  !pvalue_missing &
    is.na(pvalue_numeric)
)

pvalue_outside_range <- (
  !pvalue_missing &
    !is.na(pvalue_numeric) &
    (pvalue_numeric < 0 | pvalue_numeric > 1)
)

pvalue_audit <- data.frame(
  category = c(
    "Missing p-values",
    "Non-numeric p-values",
    "Numeric p-values outside [0,1]",
    "Valid numeric p-values within [0,1]"
  ),
  records = c(
    sum(pvalue_missing),
    sum(pvalue_non_numeric),
    sum(pvalue_outside_range),
    sum(
      !pvalue_missing &
        !is.na(pvalue_numeric) &
        pvalue_numeric >= 0 &
        pvalue_numeric <= 1
    )
  )
)

print(
  as.data.frame(pvalue_audit),
  row.names = FALSE
)

write.csv(
  pvalue_audit,
  "cleaned_output/validation/pvalue_quality_audit.csv",
  row.names = FALSE,
  na = "NA"
)

# Save non-numeric and out-of-range p-value records
pvalue_problem_indices <- which(
  pvalue_non_numeric | pvalue_outside_range
)

pvalue_problem_records <- experimental_metadata[
  pvalue_problem_indices,
  ,
  drop = FALSE
]

write.csv(
  pvalue_problem_records,
  "cleaned_output/validation/pvalue_problem_records.csv",
  row.names = FALSE,
  na = "NA"
)

cat("\nMissing-value investigation completed.\n")

# ============================================================
# INVESTIGATE OUT-OF-RANGE P-VALUE
# ============================================================

# Recalculate p-values safely
pvalue_text <- as.character(experimental_metadata$p_value)

pvalue_numeric <- suppressWarnings(
  as.numeric(trimws(pvalue_text))
)

pvalue_missing <- (
  is.na(pvalue_text) |
    trimws(pvalue_text) == "" |
    toupper(trimws(pvalue_text)) == "NA"
)

pvalue_outside_range <- (
  !pvalue_missing &
    !is.na(pvalue_numeric) &
    (pvalue_numeric < 0 | pvalue_numeric > 1)
)

# Extract problematic record
pvalue_problem_indices <- which(pvalue_outside_range)

pvalue_outlier <- experimental_metadata[
  pvalue_problem_indices,
  ,
  drop = FALSE
]

cat("\nNumber of out-of-range records:\n")
cat(nrow(pvalue_outlier), "\n")

# Display relevant metadata
pvalue_outlier_review <- pvalue_outlier %>%
  select(any_of(c(
    "analysis_id",
    "gene_accession_id",
    "gene_symbol",
    "allele_symbol",
    "strain_name",
    "parameter_stable_id",
    "parameter_name",
    "p_value"
  )))

print(
  as.data.frame(pvalue_outlier_review),
  row.names = FALSE
)

# Save for investigation
write.csv(
  pvalue_outlier_review,
  "cleaned_output/validation/pvalue_outlier_review.csv",
  row.names = FALSE,
  na = "NA"
)

# ============================================================
# VERIFY OUT-OF-RANGE P-VALUE AGAINST ORIGINAL SOURCE
# ============================================================

outlier_file <- "group11_data/an1003883757.csv"

outlier_raw <- read.csv(
  outlier_file,
  header = FALSE,
  col.names = c("field", "value"),
  colClasses = "character",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = NULL
)

# Inspect all original fields
print(
  as.data.frame(outlier_raw),
  row.names = FALSE
)

# Specifically inspect the p_value entry
cat("\nOriginal p_value entry:\n")

print(
  as.data.frame(
    outlier_raw %>%
      filter(field == "p_value")
  ),
  row.names = FALSE
)

# Save source inspection
write.csv(
  outlier_raw,
  "cleaned_output/validation/an1003883757_source_inspection.csv",
  row.names = FALSE,
  na = "NA"
)



# ============================================================
# FINAL P-VALUE OUTLIER CLASSIFICATION
# ============================================================

pvalue_outlier_classification <- data.frame(
  source_file = "an1003883757.csv",
  analysis_id = "an1003883757",
  gene_symbol = "Klk14",
  gene_accession_id = "MGI:2447564",
  parameter_stable_id = "IMPC_ECG_002_001",
  parameter_name = "HR",
  original_p_value = "1.66848697",
  source_value_verified = TRUE,
  issue_type = "Source-level p-value range violation",
  expected_range = "0 to 1",
  correction_status = "Not corrected",
  recommended_action = "Retain original value and flag for review",
  stringsAsFactors = FALSE
)

write.csv(
  pvalue_outlier_classification,
  "cleaned_output/validation/pvalue_outlier_classification.csv",
  row.names = FALSE,
  na = "NA"
)

print(
  as.data.frame(pvalue_outlier_classification),
  row.names = FALSE
)



# Missing-value types
print(
  as.data.frame(missing_type_summary),
  row.names = FALSE
)

# Number of records with at least one missing required field
cat(
  "\nRecords with missing required fields:",
  nrow(missing_records),
  "\n"
)

# Missing parameter ID breakdown
cat(
  "\nMissing parameter IDs:",
  length(missing_parameter_id_indices),
  "\n"
)

cat(
  "Literal NA parameter IDs:",
  sum(
    !is.na(parameter_id_values) &
      toupper(trimws(parameter_id_values)) == "NA"
  ),
  "\n"
)

# ============================================================
# INVESTIGATION OF MISSING PARAMETER STABLE IDs
# ============================================================

library(dplyr)

# Identify literal NA and actual missing values
parameter_id_values <- as.character(
  experimental_metadata$parameter_stable_id
)

missing_parameter_id_indices <- which(
  is.na(parameter_id_values) |
    trimws(parameter_id_values) == "" |
    toupper(trimws(parameter_id_values)) == "NA"
)

cat(
  "\nVerified missing parameter IDs:",
  length(missing_parameter_id_indices),
  "\n"
)

# Extract affected experimental records
missing_parameter_records <- experimental_metadata[
  missing_parameter_id_indices,
  ,
  drop = FALSE
]

# Inspect relevant metadata
missing_parameter_review <- missing_parameter_records %>%
  select(any_of(c(
    "source_file",
    "analysis_id",
    "gene_accession_id",
    "gene_symbol",
    "allele_symbol",
    "strain_name",
    "mouse_life_stage",
    "parameter_stable_id",
    "parameter_name",
    "p_value"
  )))

cat("\nMissing parameter ID records:\n")
cat(nrow(missing_parameter_review), "\n")

# Summarise other missing fields within these records
fields_to_check <- c(
  "gene_symbol",
  "allele_symbol",
  "strain_name",
  "mouse_life_stage",
  "parameter_name",
  "p_value"
)

missing_parameter_other_fields <- data.frame(
  field = fields_to_check,
  missing_records = vapply(
    fields_to_check,
    function(field_name) {
      values <- as.character(
        missing_parameter_review[[field_name]]
      )
      
      sum(
        is.na(values) |
          trimws(values) == "" |
          toupper(trimws(values)) == "NA"
      )
    },
    integer(1)
  )
)

print(
  as.data.frame(missing_parameter_other_fields),
  row.names = FALSE
)

# Save detailed records
write.csv(
  missing_parameter_review,
  "cleaned_output/validation/missing_parameter_id_records.csv",
  row.names = FALSE,
  na = "NA"
)

write.csv(
  missing_parameter_other_fields,
  "cleaned_output/validation/missing_parameter_id_other_fields.csv",
  row.names = FALSE,
  na = "NA"
)

cat("\nMissing parameter ID investigation saved.\n")




# ============================================================
# RECOVER CANDIDATE PARAMETER IDs USING REFERENCE NAMES
# ============================================================

library(dplyr)

# Load cleaned reference table
parameter_reference <- readRDS(
  "cleaned_output/parameter_description_clean.rds"
)

# Normalise names for comparison only
normalise_parameter_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  gsub("[^a-z0-9]", "", x)
}

# Prepare reference names and IDs
reference_names <- parameter_reference %>%
  mutate(
    normalised_name = normalise_parameter_name(name)
  ) %>%
  filter(
    !is.na(normalised_name),
    normalised_name != ""
  ) %>%
  distinct(
    normalised_name,
    impcParameterOrigId,
    parameterId,
    name
  )

# Prepare experimental records
experimental_missing_names <- missing_parameter_review %>%
  mutate(
    normalised_name = normalise_parameter_name(parameter_name)
  )

# Compare names with reference table
parameter_name_candidates <- experimental_missing_names %>%
  left_join(
    reference_names,
    by = "normalised_name",
    suffix = c("_experimental", "_reference")
  )

# Count distinct candidate reference IDs per experimental record
parameter_name_candidates <- parameter_name_candidates %>%
  group_by(source_file) %>%
  mutate(
    candidate_count = n_distinct(
      impcParameterOrigId,
      na.rm = TRUE
    ),
    candidate_classification = case_when(
      candidate_count == 0 ~ "No reference match",
      candidate_count == 1 ~ "Single reference candidate",
      candidate_count > 1 ~ "Multiple reference candidates"
    )
  ) %>%
  ungroup()

# Summarise candidate classifications by experimental record
parameter_name_summary <- parameter_name_candidates %>%
  distinct(
    source_file,
    candidate_count,
    candidate_classification
  ) %>%
  count(
    candidate_classification,
    name = "experimental_records"
  )

cat("\nParameter name matching results:\n")

print(
  as.data.frame(parameter_name_summary),
  row.names = FALSE
)

# Save detailed candidates
write.csv(
  parameter_name_candidates,
  "cleaned_output/validation/missing_parameter_id_name_candidates.csv",
  row.names = FALSE,
  na = "NA"
)

write.csv(
  parameter_name_summary,
  "cleaned_output/validation/missing_parameter_id_name_summary.csv",
  row.names = FALSE,
  na = "NA"
)

cat("\nReference name matching completed.\n")



# ============================================================
# INVESTIGATE 63 SINGLE-CANDIDATE PARAMETER MATCHES
# ============================================================

library(dplyr)

# Extract single-candidate records
single_parameter_candidates <- parameter_name_candidates %>%
  filter(candidate_count == 1) %>%
  distinct(
    source_file,
    analysis_id,
    parameter_name,
    normalised_name,
    impcParameterOrigId,
    parameterId,
    name,
    .keep_all = TRUE
  )

cat("\nSingle-candidate records:\n")
cat(nrow(single_parameter_candidates), "\n")

# Add reference descriptions
single_candidate_review <- single_parameter_candidates %>%
  left_join(
    parameter_reference %>%
      select(
        impcParameterOrigId,
        description
      ) %>%
      distinct(),
    by = "impcParameterOrigId"
  ) %>%
  left_join(
    readRDS("cleaned_output/procedure_data.rds") %>%
      select(
        impcParameterOrigId,
        procedure_name = name,
        procedure_description = description,
        isMandatory
      ),
    by = "impcParameterOrigId"
  )

# Display proposed mappings
single_candidate_display <- single_candidate_review %>%
  select(
    source_file,
    analysis_id,
    parameter_name,
    impcParameterOrigId,
    parameterId,
    name,
    description,
    procedure_name,
    procedure_description,
    isMandatory
  )

print(
  as.data.frame(single_candidate_display),
  row.names = FALSE
)

# Save detailed review
write.csv(
  single_candidate_display,
  "cleaned_output/validation/missing_parameter_id_single_candidate_review.csv",
  row.names = FALSE,
  na = "NA"
)

cat("\nSingle-candidate reference review saved.\n")


# ============================================================
# UNIQUE PARAMETER CANDIDATE AND PROCEDURE REVIEW
# ============================================================

library(dplyr)

# One row per distinct proposed reference ID
unique_candidate_mapping_review <- single_candidate_review %>%
  group_by(
    impcParameterOrigId,
    parameterId,
    name
  ) %>%
  summarise(
    experimental_record_count = n_distinct(source_file),
    experimental_names = paste(
      sort(unique(parameter_name)),
      collapse = " | "
    ),
    procedure_names = paste(
      sort(unique(na.omit(procedure_name))),
      collapse = " | "
    ),
    procedure_count = n_distinct(
      procedure_name,
      na.rm = TRUE
    ),
    reference_descriptions = paste(
      sort(unique(na.omit(procedure_description))),
      collapse = " | "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    procedure_review = case_when(
      procedure_count == 0 ~ "No procedure metadata",
      procedure_count > 1 ~ "Multiple procedure associations",
      TRUE ~ "Single procedure association"
    )
  )

cat("\nExperimental records with single candidates:\n")
cat(nrow(single_candidate_review), "\n")

cat("\nDistinct proposed reference IDs:\n")
cat(nrow(unique_candidate_mapping_review), "\n")

cat("\nProcedure association summary:\n")

print(
  as.data.frame(
    count(
      unique_candidate_mapping_review,
      procedure_review,
      name = "reference_IDs"
    )
  ),
  row.names = FALSE
)

# Display unique candidate mappings
print(
  as.data.frame(unique_candidate_mapping_review),
  row.names = FALSE
)

# Save the distinct mapping review
write.csv(
  unique_candidate_mapping_review,
  "cleaned_output/validation/missing_parameter_id_unique_mapping_review.csv",
  row.names = FALSE,
  na = "NA"
)

cat("\nUnique reference mapping review saved.\n")


unique_candidate_mapping_review <- read.csv(
  "cleaned_output/validation/missing_parameter_id_unique_mapping_review.csv",
  stringsAsFactors = FALSE
)

View(unique_candidate_mapping_review)

cat("Unique reference parameter IDs:",
    nrow(unique_candidate_mapping_review), "\n")

cat("IDs associated with multiple procedures:",
    sum(unique_candidate_mapping_review$procedure_count > 1, na.rm = TRUE),
    "\n")


write.csv(
  unique_candidate_mapping_review,
  "cleaned_output/validation/missing_parameter_id_unique_mapping_review.csv",
  row.names = FALSE,
  na = "NA"
)


# Reload the saved record-level review table
missing_parameter_id_single_candidate_review <- read.csv(
  "cleaned_output/validation/missing_parameter_id_single_candidate_review.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Confirm that it loaded correctly
cat("Rows:", nrow(missing_parameter_id_single_candidate_review), "\n")
cat("Columns:", ncol(missing_parameter_id_single_candidate_review), "\n")

# Display column names
print(names(missing_parameter_id_single_candidate_review))

# View the complete table
View(missing_parameter_id_single_candidate_review)

# Display the first 10 records in the Console
print(
  head(as.data.frame(missing_parameter_id_single_candidate_review), 10),
  row.names = FALSE
)



# 1. Load the reconstructed original experimental metadata
experimental_metadata <- readRDS(
  "cleaned_output/experimental_metadata_reconstructed.rds"
)

# 2. Check that source filenames are unique in the metadata
cat(
  "Unique source filenames:",
  length(unique(experimental_metadata$source_file)),
  "\n"
)

# 3. Match each candidate to its original experimental record
record_index <- match(
  missing_parameter_id_single_candidate_review$source_file,
  experimental_metadata$source_file
)

# Check for unmatched source files
cat(
  "Unmatched source files:",
  sum(is.na(record_index)),
  "\n"
)

# 4. Extract the original parameter IDs
original_ids <- experimental_metadata$parameter_stable_id[record_index]

# 5. Build the proposed mapping audit
proposed_mapping_audit <- data.frame(
  source_file = missing_parameter_id_single_candidate_review$source_file,
  analysis_id = missing_parameter_id_single_candidate_review$analysis_id,
  original_parameter_stable_id = original_ids,
  experimental_parameter_name =
    missing_parameter_id_single_candidate_review$parameter_name,
  proposed_impcParameterOrigId =
    missing_parameter_id_single_candidate_review$impcParameterOrigId,
  proposed_parameterId =
    missing_parameter_id_single_candidate_review$parameterId,
  reference_parameter_name =
    missing_parameter_id_single_candidate_review$name,
  reference_description =
    missing_parameter_id_single_candidate_review$description,
  procedure_name =
    missing_parameter_id_single_candidate_review$procedure_name,
  procedure_description =
    missing_parameter_id_single_candidate_review$procedure_description,
  isMandatory =
    missing_parameter_id_single_candidate_review$isMandatory,
  mapping_evidence = "Single exact normalised name candidate",
  mapping_status = "Proposed - pending validation",
  stringsAsFactors = FALSE
)

# 6. Validate the audit
cat("\n--- Proposed mapping audit ---\n")
cat("Total records:", nrow(proposed_mapping_audit), "\n")
cat(
  "Distinct reference IDs:",
  length(unique(proposed_mapping_audit$proposed_parameterId)),
  "\n"
)
cat(
  "Original IDs still missing:",
  sum(
    is.na(proposed_mapping_audit$original_parameter_stable_id) |
      proposed_mapping_audit$original_parameter_stable_id == "NA"
  ),
  "\n"
)
cat(
  "Duplicate source files:",
  sum(duplicated(proposed_mapping_audit$source_file)),
  "\n"
)

# 7. Save without modifying the original dataset
write.csv(
  proposed_mapping_audit,
  "cleaned_output/validation/missing_parameter_id_proposed_mapping_audit.csv",
  row.names = FALSE,
  na = "NA"
)

# 8. View the complete audit
View(proposed_mapping_audit)

# Classify all 62 ambiguous records
multiple_stable_ids$final_mapping_status <-
  "Unresolved - multiple stable ID candidates"

# Add procedure-level evidence
multiple_stable_ids$procedure_evidence <- ifelse(
  multiple_stable_ids$source_file %in%
    seven_single_procedure$source_file,
  "Single procedure, but multiple stable IDs",
  "Multiple candidate procedures and stable IDs"
)

# Save
write.csv(
  multiple_stable_ids,
  "cleaned_output/validation/62_unresolved_stable_id_records.csv",
  row.names = FALSE,
  na = "NA"
)

cat("Unresolved records:", nrow(multiple_stable_ids), "\n")
cat(
  "Single-procedure cases:",
  sum(multiple_stable_ids$procedure_evidence ==
        "Single procedure, but multiple stable IDs"),
  "\n"
)
# Final missing parameter ID classification
missing_id_final_summary <- data.frame(
  classification = c(
    "Single reference candidate - proposed",
    "Multiple candidates, one stable ID - proposed",
    "Multiple stable IDs - unresolved",
    "No reference match - unresolved"
  ),
  records = c(63, 129, 62, 28)
)

missing_id_final_summary$total <- sum(
  missing_id_final_summary$records
)

print(missing_id_final_summary, row.names = FALSE)

# Save final summary
write.csv(
  missing_id_final_summary,
  "cleaned_output/validation/missing_parameter_id_final_summary.csv",
  row.names = FALSE
)

stopifnot(sum(missing_id_final_summary$records) == 282)


# Read existing analysis ID investigation files
analysis_original <- read.csv(
  "cleaned_output/validation/analysis_id_mismatch_investigation.csv",
  stringsAsFactors = FALSE
)

analysis_unresolved <- read.csv(
  "cleaned_output/validation/unresolved_analysis_id_mismatches.csv",
  stringsAsFactors = FALSE
)

# Inspect dimensions and column names
cat("Original investigation:", nrow(analysis_original), "rows\n")
print(names(analysis_original))

cat("\nUnresolved investigation:", nrow(analysis_unresolved), "rows\n")
print(names(analysis_unresolved))


# 1. Start with the original 80 mismatch records
analysis_corrected <- analysis_original

# 2. Add corrected filename comparison
analysis_corrected$internal_id_matches_filename <-
  analysis_corrected$analysis_id ==
  analysis_corrected$filename_without_extension

analysis_corrected$internal_id_matches_suffix_removed_filename <-
  analysis_corrected$analysis_id ==
  analysis_corrected$filename_without_duplicate_suffix

# 3. Classify each discrepancy
analysis_corrected$mismatch_category <- ifelse(
  analysis_corrected$internal_id_matches_filename,
  "No mismatch after filename comparison",
  ifelse(
    analysis_corrected$internal_id_matches_suffix_removed_filename,
    "Explained by duplicate filename suffix",
    "Unresolved formatting anomaly"
  )
)

# 4. Save corrected investigation
write.csv(
  analysis_corrected,
  "cleaned_output/validation/analysis_id_mismatch_investigation_corrected.csv",
  row.names = FALSE,
  na = "NA"
)

# 5. Verify classifications
print(
  as.data.frame(table(analysis_corrected$mismatch_category)),
  row.names = FALSE
)

cat("\nTotal records:", nrow(analysis_corrected), "\n")

cat(
  "Unresolved records:",
  sum(analysis_corrected$mismatch_category ==
        "Unresolved formatting anomaly"),
  "\n"
)

cat(
  "Corrected file saved:",
  file.exists(
    "cleaned_output/validation/analysis_id_mismatch_investigation_corrected.csv"
  ),
  "\n"
)

# 6. Check the unresolved record against the original review
print(analysis_unresolved, row.names = FALSE)


# Verify corrected analysis ID investigation
corrected_check <- read.csv(
  "cleaned_output/validation/analysis_id_mismatch_investigation_corrected.csv",
  stringsAsFactors = FALSE
)

cat("Corrected investigation rows:", nrow(corrected_check), "\n")

cat(
  "Suffix-related discrepancies:",
  sum(corrected_check$mismatch_category ==
        "Explained by duplicate filename suffix"),
  "\n"
)

cat(
  "Unresolved anomalies:",
  sum(corrected_check$mismatch_category ==
        "Unresolved formatting anomaly"),
  "\n"
)

# Verify the unresolved record
print(
  corrected_check[
    corrected_check$mismatch_category ==
      "Unresolved formatting anomaly",
  ],
  row.names = FALSE
)

stopifnot(
  nrow(corrected_check) == 80,
  sum(corrected_check$mismatch_category ==
        "Explained by duplicate filename suffix") == 79,
  sum(corrected_check$mismatch_category ==
        "Unresolved formatting anomaly") == 1
)
# Recheck all core and validation files
file_check <- data.frame(
  file = c(core_files, validation_files),
  exists = file.exists(c(core_files, validation_files))
)

print(file_check, row.names = FALSE)

cat("\nMissing files:", sum(!file_check$exists), "\n")

# Confirm dataset integrity
cat(
  "\nExperimental records:",
  nrow(experimental_metadata), "\n"
)

cat(
  "Metadata columns:",
  ncol(experimental_metadata), "\n"
)

# Confirm consolidated summary
final_summary <- read.csv(
  "cleaned_output/validation/consolidated_validation_summary.csv",
  stringsAsFactors = FALSE
)

cat(
  "Consolidated validation findings:",
  nrow(final_summary), "\n"
)

stopifnot(
  all(file_check$exists),
  nrow(experimental_metadata) == 28670
)


# ============================================================
# GROUP 11 - FINAL DATA CLEANING AND VALIDATION CHECK
# ============================================================

project_root <- "/scratch/grp/msc_appbio/Group11_Project/Group11"

setwd(project_root)

output_dir <- "final_submission"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ------------------------------------------------------------
# 1. Required core datasets
# ------------------------------------------------------------

core_files <- c(
  "cleaned_output/parameter_description_clean.rds",
  "cleaned_output/procedure_data.rds",
  "cleaned_output/raw_records_imported.rds",
  "cleaned_output/source_file_list.rds",
  "cleaned_output/experimental_metadata_reconstructed.rds"
)

# ------------------------------------------------------------
# 2. Required validation outputs
# ------------------------------------------------------------

validation_files <- c(
  "cleaned_output/validation/consolidated_validation_summary.csv",
  "cleaned_output/validation/required_field_missing_value_audit.csv",
  "cleaned_output/validation/missing_value_type_summary.csv",
  "cleaned_output/validation/pvalue_quality_audit.csv",
  "cleaned_output/validation/pvalue_outlier_classification.csv",
  "cleaned_output/validation/analysis_id_mismatch_investigation_corrected.csv",
  "cleaned_output/validation/missing_parameter_id_final_summary.csv",
  "cleaned_output/validation/28_unresolved_parameter_ids_final.csv",
  "cleaned_output/validation/62_unresolved_stable_id_records.csv",
  "cleaned_output/validation/ambiguous_one_stable_id_proposed_audit.csv",
  "cleaned_output/validation/parameter_procedure_review.csv"
)

# ------------------------------------------------------------
# 3. Check files and record their sizes
# ------------------------------------------------------------

all_files <- c(core_files, validation_files)

file_manifest <- data.frame(
  file = all_files,
  exists = file.exists(all_files),
  size_bytes = ifelse(
    file.exists(all_files),
    file.info(all_files)$size,
    NA
  ),
  stringsAsFactors = FALSE
)

write.csv(
  file_manifest,
  file.path(output_dir, "file_manifest.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 4. Verify reconstructed metadata
# ------------------------------------------------------------

experimental_metadata <- readRDS(
  "cleaned_output/experimental_metadata_reconstructed.rds"
)

metadata_check <- data.frame(
  metric = c(
    "Experimental records",
    "Metadata columns",
    "Missing required columns"
  ),
  result = c(
    nrow(experimental_metadata),
    ncol(experimental_metadata),
    NA
  )
)

write.csv(
  metadata_check,
  file.path(output_dir, "metadata_integrity_check.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 5. Verify consolidated validation summary
# ------------------------------------------------------------

final_summary <- read.csv(
  "cleaned_output/validation/consolidated_validation_summary.csv",
  stringsAsFactors = FALSE
)

write.csv(
  final_summary,
  file.path(output_dir, "consolidated_validation_summary.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 6. Record R environment
# ------------------------------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(output_dir, "R_session_information.txt")
)

# ------------------------------------------------------------
# 7. Final checks
# ------------------------------------------------------------

cat("\n========== FINAL VALIDATION ==========\n")

cat("Core files present:",
    sum(file_manifest$exists[seq_along(core_files)]),
    "/",
    length(core_files), "\n")

cat("Validation files present:",
    sum(file_manifest$exists[-seq_along(core_files)]),
    "/",
    length(validation_files), "\n")

cat("Missing files:",
    sum(!file_manifest$exists), "\n")

cat("Experimental records:",
    nrow(experimental_metadata), "\n")

cat("Metadata columns:",
    ncol(experimental_metadata), "\n")

cat("Consolidated findings:",
    nrow(final_summary), "\n")

cat("Final submission folder:",
    normalizePath(output_dir), "\n")

stopifnot(
  all(file_manifest$exists),
  nrow(experimental_metadata) == 28670
)

cat("\nFINAL CHECKS PASSED\n")
# Inspect project output structure
cat("Project:", getwd(), "\n\n")

cat("Cleaned output files:\n")
print(
  list.files(
    "cleaned_output",
    recursive = TRUE
  )
)

cat("\nFinal submission files:\n")
print(
  list.files(
    "final_submission",
    recursive = TRUE
  )
)


# Inspect top-level project files
cat("PROJECT ROOT FILES\n")

print(
  list.files(
    ".",
    recursive = FALSE,
    all.files = FALSE
  )
)

# Locate R scripts in the project
cat("\nR SCRIPTS\n")

r_scripts <- list.files(
  ".",
  pattern = "\\.[Rr]$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

print(r_scripts)

# Check whether a separate final experimental dataset exists
cat("\nPOTENTIAL FINAL DATASETS\n")

print(
  list.files(
    ".",
    pattern = "clean|final|metadata",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
)


# 1. Inspect Group11.zip
cat("GROUP11 ZIP CONTENTS\n")

zip_contents <- unzip("Group11.zip", list = TRUE)

print(
  zip_contents[, c("Name", "Length")],
  row.names = FALSE
)

# 2. Inspect group11_data.zip
cat("\nRAW DATA ZIP SUMMARY\n")

raw_zip_contents <- unzip("group11_data.zip", list = TRUE)

cat("Files in raw data archive:", nrow(raw_zip_contents), "\n")

# 3. Check the visible R script length
cat("\nEXISTING R SCRIPT\n")

script_lines <- readLines(
  "Group11_Final_Validation.R",
  warn = FALSE
)

cat("Lines in validation script:", length(script_lines), "\n")

# 4. Check the reconstructed dataset size
cat("\nRECONSTRUCTED DATASET\n")

cat(
  "File size (MB):",
  round(
    file.info(
      "cleaned_output/experimental_metadata_reconstructed.rds"
    )$size / 1024^2,
    2
  ),
  "\n"
)

























# ============================================================
# GROUP 11 - FINAL SUBMISSION PACKAGING
# ============================================================

setwd("/scratch/grp/msc_appbio/Group11_Project/Group11")

output_dir <- "final_submission"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 1. Define core datasets
# ------------------------------------------------------------

core_files <- c(
  "cleaned_output/parameter_description_clean.rds",
  "cleaned_output/procedure_data.rds",
  "cleaned_output/raw_records_imported.rds",
  "cleaned_output/source_file_list.rds",
  "cleaned_output/experimental_metadata_reconstructed.rds"
)

# ------------------------------------------------------------
# 2. Define important validation reports
# ------------------------------------------------------------

validation_files <- c(
  "consolidated_validation_summary.csv",
  "file_validation.csv",
  "exceptional_file_inspection.csv",
  "experimental_field_sop_comparison.csv",
  "alternative_field_names.csv",
  "reference_validation_summary.csv",
  "stable_id_inconsistency_details.csv",
  "required_field_missing_value_audit.csv",
  "missing_value_type_summary.csv",
  "records_with_missing_required_fields.csv",
  "pvalue_quality_audit.csv",
  "pvalue_outlier_classification.csv",
  "analysis_id_mismatch_investigation_corrected.csv",
  "missing_parameter_id_final_summary.csv",
  "missing_parameter_id_proposed_mapping_audit.csv",
  "ambiguous_one_stable_id_proposed_audit.csv",
  "62_unresolved_stable_id_records.csv",
  "28_unresolved_parameter_ids_final.csv",
  "parameter_procedure_review.csv",
  "UCD_source_investigation_summary.csv",
  "consolidated_validation_summary.csv"
)

validation_paths <- file.path(
  "cleaned_output/validation",
  validation_files
)

# ------------------------------------------------------------
# 3. Check that source files exist
# ------------------------------------------------------------

missing_core <- core_files[!file.exists(core_files)]

missing_validation <- validation_paths[
  !file.exists(validation_paths)
]

cat("\nCORE DATASETS\n")
print(core_files)

cat("\nMISSING CORE DATASETS:\n")
print(missing_core)

cat("\nMISSING VALIDATION REPORTS:\n")
print(missing_validation)

if (length(missing_core) > 0) {
  stop("A required core dataset is missing. Packaging stopped.")
}

# ------------------------------------------------------------
# 4. Copy core datasets into final submission
# ------------------------------------------------------------

for (f in core_files) {
  file.copy(
    from = f,
    to = file.path(output_dir, basename(f)),
    overwrite = TRUE
  )
}

# ------------------------------------------------------------
# 5. Copy available validation reports
# ------------------------------------------------------------

available_validation <- validation_paths[
  file.exists(validation_paths)
]

validation_output_dir <- file.path(
  output_dir,
  "validation"
)

dir.create(
  validation_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

for (f in available_validation) {
  file.copy(
    from = f,
    to = file.path(validation_output_dir, basename(f)),
    overwrite = TRUE
  )
}

# ------------------------------------------------------------
# 6. Verify reconstructed metadata
# ------------------------------------------------------------

experimental_metadata <- readRDS(
  "cleaned_output/experimental_metadata_reconstructed.rds"
)

required_fields <- c(
  "analysis_id",
  "gene_accession_id",
  "gene_symbol",
  "strain_name",
  "mouse_life_stage",
  "parameter_stable_id",
  "parameter_name",
  "p_value",
  "allele_symbol"
)

missing_required_columns <- setdiff(
  required_fields,
  names(experimental_metadata)
)

metadata_check <- data.frame(
  metric = c(
    "Experimental records",
    "Metadata columns",
    "Missing required columns"
  ),
  result = c(
    as.character(nrow(experimental_metadata)),
    as.character(ncol(experimental_metadata)),
    if (length(missing_required_columns) == 0) {
      "0"
    } else {
      paste(missing_required_columns, collapse = "; ")
    }
  ),
  stringsAsFactors = FALSE
)

write.csv(
  metadata_check,
  file.path(output_dir, "metadata_integrity_check.csv"),
  row.names = FALSE
)

if (length(missing_required_columns) > 0) {
  stop("Required metadata columns are missing.")
}

# ------------------------------------------------------------
# 7. Copy consolidated validation summary
# ------------------------------------------------------------

summary_source <- file.path(
  "cleaned_output/validation",
  "consolidated_validation_summary.csv"
)

if (!file.exists(summary_source)) {
  stop("Consolidated validation summary is missing.")
}

final_summary <- read.csv(
  summary_source,
  stringsAsFactors = FALSE
)

write.csv(
  final_summary,
  file.path(output_dir, "consolidated_validation_summary.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. Record R environment
# ------------------------------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(output_dir, "R_session_information.txt")
)

# ------------------------------------------------------------
# 9. Generate final file manifest
# ------------------------------------------------------------

submission_files <- list.files(
  output_dir,
  recursive = TRUE,
  full.names = TRUE
)

submission_files <- submission_files[
  !dir.exists(submission_files)
]

file_manifest <- data.frame(
  file = substring(
    submission_files,
    nchar(normalizePath(output_dir)) + 2
  ),
  exists = file.exists(submission_files),
  size_bytes = file.info(submission_files)$size,
  stringsAsFactors = FALSE
)

write.csv(
  file_manifest,
  file.path(output_dir, "file_manifest.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 10. Create README
# ------------------------------------------------------------

readme <- c(
  "GROUP 11 - DATA CLEANING AND DATA MANAGEMENT",
  "",
  "FINAL SUBMISSION CONTENTS",
  "",
  "Core datasets:",
  "- parameter_description_clean.rds: cleaned parameter reference data.",
  "- procedure_data.rds: procedure reference data.",
  "- raw_records_imported.rds: imported experimental key-value records.",
  "- source_file_list.rds: source filenames corresponding to imported records.",
  "- experimental_metadata_reconstructed.rds: reconstructed experimental metadata.",
  "",
  "Validation folder:",
  "- Contains available structural, reference, metadata and quality-control reports.",
  "",
  "Important data handling decisions:",
  "- Original source CSV files were retained unchanged.",
  "- Exact duplicate reference rows were removed from the cleaned parameter reference.",
  "- Missing and ambiguous parameter identifiers were not automatically overwritten.",
  "- Proposed mappings remain proposals pending validation.",
  "- Source-level anomalies and unresolved records are documented in validation reports.",
  "",
  "Metadata integrity and file inventory are provided separately.",
  "R environment details are recorded in R_session_information.txt."
)

writeLines(
  readme,
  file.path(output_dir, "README.txt")
)

# ------------------------------------------------------------
# 11. Final checks
# ------------------------------------------------------------

cat("\n========== FINAL SUBMISSION CHECK ==========\n")

cat("Core datasets copied:", length(core_files), "\n")
cat("Validation reports copied:", length(available_validation), "\n")
cat("Missing core datasets:", length(missing_core), "\n")
cat("Missing validation reports:", length(missing_validation), "\n")
cat("Experimental records:", nrow(experimental_metadata), "\n")
cat("Metadata columns:", ncol(experimental_metadata), "\n")
cat("Missing required columns:", length(missing_required_columns), "\n")
cat("Consolidated findings:", nrow(final_summary), "\n")

cat("\nFINAL SUBMISSION CONTENTS:\n")

print(
  list.files(
    output_dir,
    recursive = TRUE
  )
)

stopifnot(
  length(missing_core) == 0,
  length(missing_required_columns) == 0,
  nrow(experimental_metadata) == 28670,
  file.exists(file.path(output_dir, "README.txt")),
  file.exists(file.path(output_dir, "file_manifest.csv"))
)

cat("\nFINAL SUBMISSION PACKAGING COMPLETED\n")
output_dir <- "final_submission"

submission_files <- list.files(
  output_dir,
  recursive = TRUE,
  full.names = TRUE
)

submission_files <- submission_files[
  !dir.exists(submission_files) &
    basename(submission_files) != "file_manifest.csv"
]

file_manifest <- data.frame(
  file = substring(
    submission_files,
    nchar(normalizePath(output_dir)) + 2
  ),
  exists = file.exists(submission_files),
  size_bytes = file.info(submission_files)$size,
  stringsAsFactors = FALSE
)

write.csv(
  file_manifest,
  file.path(output_dir, "file_manifest.csv"),
  row.names = FALSE
)

cat("Manifest updated. Files listed:", nrow(file_manifest), "\n")

save.image("Group11_workspace_backup.RData")