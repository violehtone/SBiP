# load design file

library(readxl)
library(lubridate)

data.proteomics.dir <- file.path(data.raw.dir, "proteomics")
proteomics.counts.file.pattern <- "^(.*/)?([0-9]{8})_counts.csv$"
proteomics.submission.file.pattern <- "^(.*/)?([0-9]{8})_proteomics_submission.xlsx$"
proteomics.ratio.file.pattern <- "^(.*/)?([0-9]{8})_proteomic_quant.xlsx$"

collect_proteomics_submission_files <- function( path = NULL, pattern = NULL ) {
  
  if (is.null(path)) {
    path <- data.proteomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- proteomics.submission.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_proteomics_submission_file <- function( path ) {
  
  read_excel(path)
  
}

load_proteomics_submission <- function ( data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(data.files)) {
    data.files <- collect_proteomics_submission_files(path, pattern)
  }
  
  map_dfr(
    data.files, load_proteomics_submission_file
  )
}


# Load data files

collect_proteomics_ratio_files <- function(path = NULL, pattern = NULL) {
  
  if (is.null(path)) {
    path <- data.proteomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- proteomics.ratio.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_proteomics_ratio_file <- function( path ) {

    read_excel(
      path, sheet = "QuantOutput", na = "NA",
      col_names = c("protein", "description", "log_ratio_1", "log_ratio_2", "log_ratio_3", "avg_ratio", "sd_ratio", "ratio_count"),
      skip = 1
    )
}

load_proteomics_ratio <- function ( data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(data.files)) {
    data.files <- collect_proteomics_ratio_files(path, pattern)
  }
  
  map_dfr(
    data.files, load_proteomics_ratio_file
  )
}

