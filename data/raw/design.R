# load design file

library(readxl)
library(lubridate)

data.design.dir <- file.path(data.raw.dir, "design")
design.file.pattern <- "^(.*/)?([0-9]{8})_design.xlsx$"

collect_design_files <- function(path, pattern = NULL) {
  
  if (is.null(path)) {
    path <- data.design.dir
  }
  
  if (is.null(pattern)) {
    pattern <- design.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
}

read_design_conditions <- function(path) {
  
  if ("conditions" %in% readxl::excel_sheets(path)) {
    return(
      read_excel(path, sheet = "conditions")
    )
  }
  
  return(data.frame())
}

read_design_strains <- function(path) {
  result <- read_excel(
    path, sheet = "strains"
  )
  
  return(result)
}

read_design_constants <- function(path) {
  result <- read_excel(
    path, sheet="constants"
  ) 
  return(result)
}

read_sample_design <- function(path) {
  result <- read_excel(
    path, sheet="samples"
  )  %>% 
    mutate(
      dataset = ifelse(str_detect(sample_id, "PC"), "pre-coure", "course")
    )
  return(result)
}

load_design_file <- function(path, pattern) {
  # path <- design.files[1]
  
  dataset <- sub(pattern, "\\2", path)
  
  strains <- read_design_strains(path)
  
  conditions <- read_design_conditions(path)
  
  samples <- read_sample_design(path) 
  
  # join everything
  results <- strains %>% 
    left_join(samples, by = c("multicultivator", "channel"))
  
  if (nrow(conditions) > 0) {
    results <- left_join(
      results, conditions, by = c("multicultivator", "channel")
    )
  }

  return(results)
}

load_design <- function(data.files = NULL, path=NULL, pattern=NULL) {
  if (is.null(pattern)) {
    pattern <- design.file.pattern
  }
  
  if (is.null(data.files)) { 
    data.files <- collect_design_files(path = path, pattern = pattern)  
  }
  
  results <- map_dfr(
    data.files, load_design_file, pattern=pattern
  )
  
  return(results)
}

load_design_conditions <- function(data.files = NULL, path = NULL, pattern = NULL) {
  
  if (is.null(pattern)) {
    pattern <- design.file.pattern
  }
  
  if (is.null(data.files)) { 
    data.files <- collect_design_files(path = path, pattern = pattern)  
  }

  conditions <- map_dfr( 
    data.files, read_design_conditions
  )
  
  return(conditions)
}

load_design_thresholds <- function(data.files = NULL, path = NULL, pattern = NULL) {
  
  if (is.null(pattern)) {
    pattern <- design.file.pattern
  }
  
  if (is.null(data.files)) { 
    data.files <- collect_design_files(path = path, pattern = pattern)  
  }
  
  results <- map_dfr(
    data.files, read_design_thresholds
  )
  
  return(results)
}

