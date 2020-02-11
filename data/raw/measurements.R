# load measurement files

library(readxl)

collect_measurement_files <- function(path = NULL, pattern = NULL ) {
  
  if (is.null(path)) {
    path <- file.path(data.raw.dir, "measurements")
  }
  
  if (is.null(pattern)) {
    pattern <- "(.*/)?([0-9]{8})_measurements.xlsx"
  }
  
  return(
    list.files(
      path, pattern, full.names = TRUE
    )
  )
}

#' Load OD Data from a measurements file
read_measurements_od <- function(path) {
  
  # read excel
  result <- read_excel(
    path, sheet = "od"
  ) %>%
    # correct diltion
    # apply automatically to all columns starting with od_raw_XXX -> od_raw_XXX_value
    mutate_at(
      vars(starts_with("od_raw")),
      funs( value = . * (sample_volume + blank_volume) / (sample_volume)) 
    ) %>%
    # remove redundant columns
    select(-c(sample_volume, blank_volume)) %>%
    # rename od_raw_XXX_value to od_value_XXX
    rename_at(
      vars(ends_with("_value")), 
      funs(str_replace(., "od_raw_(\\d+)_value", "od_value_\\1"))
    ) %>%
    # remove redundant columns
    select(-c(starts_with("od_raw")))
  
  return(result)
}


#' Combines OD, Proteomics and Casy data
load_measurements_file <- function(path) {

  df.measurements.od <- read_measurements_od(path)
  
  # join everything by sample_id
  return(df.measurements.od)
}

#' Generate ready-to-go measurements table
load_measurements <- function(data.files = NULL, path = NULL, pattern = NULL) {
  
  if (is.null(data.files)) { 
    data.files <- collect_measurement_files(path = path, pattern = pattern)  
  }
  
  results <- map_dfr(
    data.files, load_measurements_file
  )
  
  return(results)  
}

