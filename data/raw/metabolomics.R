# load design file

library(readxl)
library(readr)
library(lubridate)

data.metabolomics.dir <- file.path(data.raw.dir, "metabolomics")
metabolomics.data.file.pattern <- "^(.*/)?([0-9]{8})_metabolomics_([a-z]+)_phase.csv$"
metabolomics.design.file.pattern <- "^(.*/)?([0-9]{8})_metabolomics_design.xlsx$"
metabolomics.submission.file.pattern <- "^(.*/)?([0-9]{8})_metabolomics_submission.xlsx$"
metabolites.model.file.pattern <- "iSynCJ816_chemical_formulas.csv"


# Load submission data

collect_metabolomics_submission_files <- function( path = NULL, pattern = NULL ) {
  
  if (is.null(path)) {
    path <- data.metabolomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- metabolomics.submission.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_metabolomics_submission_file <- function( path ) {
  
  read_excel(path)
  
}

load_metabolomics_submission <- function ( data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(data.files)) {
    data.files <- collect_metabolomics_submission_files(path, pattern)
  }
  
  map_dfr(
    data.files, load_metabolomics_submission_file
  )
}

# Load design

collect_metabolomics_design_files <- function(path = NULL, pattern = NULL) {
  
  if (is.null(path)) {
    path <- data.metabolomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- metabolomics.design.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_metabolomics_design_file <- function( path ) {
  
  read_excel(path) %>%
    pivot_longer(
      cols = -sample_id,
      names_to = "phase",
      names_pattern = "(.*)_id",
      values_to = "metabolomics_id"
    )
  
}

load_metabolomics_design <- function ( data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(pattern)) {
    pattern <- metabolomics.design.file.pattern
  }
  
  if (is.null(data.files)) {
    data.files <- collect_metabolomics_design_files(path, pattern)
  }
  
  map_dfr(
    data.files, load_metabolomics_design_file
  )
}

# Load data files

collect_metabolomics_data_files <- function(path = NULL, pattern = NULL) {
  
  if (is.null(path)) {
    path <- data.metabolomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- metabolomics.data.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_metabolomics_data_file <- function( path, pattern ) {
  
  phase <- sub(pattern, "\\3", path)
  
  suppressMessages(
    read_csv(path) 
  ) %>%
    rename(
      bucket = `Bucket label`,
      mz_ratio = `m/z`
    ) %>%
    mutate(
      phase = phase,
      mass = as.numeric(str_replace(bucket, "(.*) Da .*", "\\1")),
      charge = mass / mz_ratio 
    ) %>%
    filter(!is.na(bucket)) %>%
    pivot_longer(
      cols = starts_with("2020"),
      names_to = "metabolomics_id",
      values_to = "value"
    )
  
}


load_metabolomics <- function (data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(pattern)) {
    pattern <- metabolomics.data.file.pattern
  }

  if (is.null(data.files)) {
    data.files <- collect_metabolomics_data_files(path, pattern)
  }

  map_dfr(
    data.files, load_metabolomics_data_file, pattern = pattern
  )
  
}

# load_metabolomics <- function ( df.data = NULL, df.design = NULL, path = NULL, data.pattern = NULL, design.pattern = NULL ) {
#   
#   if (is.null(df.data)) {
#     df.data <- load_metabolomics_data(path, pattern = data.pattern)
#   }
#   
#   if (is.null(df.design)) {
#     df.design <- load_metabolomics_design(path, pattern = design.pattern)
#   }
#   
#   df.data %>%
#     left_join(
#       df.design,
#       by = c("phase", "metabolomics_id")
#     )
#   
# }


# Load metabolite names from model

collect_metabolites_model_files <- function(path = NULL, pattern = NULL) {
  
  if (is.null(path)) {
    path <- data.metabolomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- metabolites.model.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}  
  
read_metabolites_model_file <- function( path, pattern ) {
  
  suppressMessages(
      read_delim(path, delim = ";", col_names = c("Name_model", "Formula_model"), skip = 1) 
    ) 
    
  }    
  

load_metabolites_model <- function (data.files = NULL, path = NULL, pattern = NULL) {
  
  if (is.null(data.files)) {
    path <- collect_metabolites_model_files(path = path, pattern = pattern)
  }
  
  results <- read_metabolites_model_file(path)
  
  return(results)  
}
