# Suppress R CMD check NOTEs about NSE column references inside data.table
# expressions used by the package (e.g. `[, .cohort_status := ...]` in
# CohortPipeline$get_everyone()).
utils::globalVariables(".cohort_status")
