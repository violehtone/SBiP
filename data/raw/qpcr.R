library(tidyverse)
library(broom)
library(kableExtra)
library(readxl)
library(tidyverse)


### CONSTANTS
data.pcr.dir <- "qpcr"
pcr.design.file.pattern <- "^(.*/)?([0-9]{8})_([0-9]{1})_rt_pcr_design\\.xlsx$"
pcr.export.file.pattern <- "component"

transpose.plate <- function(df, column_name){
  out.df <- df %>%
    gather(column, UQ(column_name), 2:13) %>%
    mutate(
      column = as.numeric(column)
    ) %>%
    arrange(`...1`, column) %>%
    mutate(
      well = 1:96
    ) %>%
    rename(row = `...1`)
  return(out.df)
}


collect_pcr_design_files <- function(plate.id = NULL, path = NULL, pattern = NULL) {
  
  if (is.null(plate.id)) {
    plate.id <- ".*"
  }
  
  if (is.null(path)) {
    path <- file.path(data.raw.dir, data.pcr.dir, plate.id)
  }
  
  if (is.null(pattern)) {
    pattern <- pcr.design.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
}

collect_pcr_export_files <- function(plate.id = NULL, path = NULL, pattern = NULL) {
  
  if (is.null(plate.id)) {
    plate.id <- ".*"
  }
  
  if (is.null(path)) {
    path <- file.path(data.raw.dir, data.pcr.dir, plate.id)
  }
  
  if (is.null(pattern)) {
    pattern <- pcr.export.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
}

read_pcr_export <- function(path) {
  result <- read_csv(path)
  return(result)
}

read_assay_samples_pcr_design <- function(path) {
  result <- read_excel(
    path, sheet = "assay_samples"
  ) 
  return(result)
}

read_assay_dilutions_pcr_design <- function(path) {
  result <- read_excel(
    path, sheet = "assay_dilutions"
  ) 
  return(result)
}

read_master_mix_pcr_design <- function(path) {
  result <- read_excel(
    path, sheet = "master_mix"
  ) 
  return(result)
}


read_standard_info_pcr_design <- function(path) {
  result <- read_excel(
    path, sheet = "standard_info"
  ) 
  return(result)
}

read_dil_plate_mQ_vol_pcr_design <- function(path) {
  result <- read_excel(
    path, sheet = "dil_plate_mQ_vol"
  ) 
  return(result)
}

read_dil_plate_sample_vol_pcr_design <- function(path) {
  result <- read_excel(
    path, sheet = "dil_plate_sample_vol"
  ) 
  return(result)
}

read_outliers_pcr_design <- function(path) {
  result <- read_excel(
    path, sheet = "outliers"
  ) 
  return(result)
}

read_sample_class_pcr_design <- function(path) {
  result <- read_excel(
    path, sheet = "sample_class"
  ) 
  return(result)
}

load_plate_design_pcr_file <- function( path, pattern = NULL ){
  
  assay.plate <- read_assay_samples_pcr_design(path)
  assay.dilutions <- read_assay_dilutions_pcr_design(path)
  outliers.info <- read_outliers_pcr_design(path)
  sample.class <- read_sample_class_pcr_design(path)
  
  results <- transpose.plate(
    assay.plate, "sample"
  ) %>% 
    separate(
      sample, 
      into = c("sample_id", "replicate", "sample_dilution"), 
      sep = "\\."
    ) %>% 
    left_join(
      transpose.plate(assay.dilutions, "dilution_factor"), 
      by = c("row", "column", "well")
    ) %>% 
    left_join(
      transpose.plate(outliers.info, "outlier"), 
      by = c("row", "column", "well")
    ) %>% 
    left_join(
      transpose.plate(sample.class, "sample_class"), 
      by = c("row", "column", "well")
    ) %>% 
    filter(!is.na(sample_id)) %>% 
    mutate(
      outlier = ifelse(is.na(outlier), F, T)
    )
}

load_plate_design_pcr <- function(data.files = NULL, plate.id = NULL, path=NULL, pattern=NULL) {
  
  if (is.null(pattern)) {
    pattern <- pcr.design.file.pattern
  }
  
  if (is.null(data.files)) { 
    data.files <- collect_pcr_design_files(plate.id = plate.id, path = path, pattern = pattern)  
  }
  
  results <- map_dfr(
    data.files, load_plate_design_pcr_file, pattern=pattern
  )
  
  return(results)
}



load_protocol_design_pcr_file <-  function( path = NULL, pattern = NULL ){
  
  assay.plate <- read_assay_samples_pcr_design(path)
  mm <- read_master_mix_pcr_design(path)
  dil.mQ <- read_dil_plate_mQ_vol_pcr_design(path)
  dil.samples <- read_dil_plate_sample_vol_pcr_design(path)
  st.info <- read_standard_info_pcr_design(path)

  results <- tibble(
    mm = list(mm), 
    assay_plate = list(assay.plate),
    dil_mQ = list(dil.mQ),
    dil_samples = list(dil.samples),
    st_info = list(st.info)
  )
  return(results)
}

load_protocol_design_pcr <- function(data.files = NULL, plate.id = NULL, path=NULL, pattern=NULL) {

  if (is.null(pattern)) {
    pattern <- pcr.design.file.pattern
  }
  
  if (is.null(data.files)) { 
    data.files <- collect_pcr_design_files(plate.id = plate.id, path = path, pattern = pattern)  
  }
  
  results <- map_dfr(
    data.files, load_protocol_design_pcr_file, pattern=pattern
  )
  
  return(results)
}

load_export_pcr_file <-  function( path = NULL, pattern = NULL ){
  
  results <- read_pcr_export(path)
  
  return(results)
}

load_export_pcr <- function(data.files = NULL, plate.id = NULL, path=NULL, pattern=NULL) {
  
  if (is.null(pattern)) {
    pattern <- pcr.export.file.pattern
  }
  
  if (is.null(data.files)) { 
    data.files <- collect_pcr_export_files(plate.id = plate.id, path = path, pattern = pattern)  
  }
  
  results <- map_dfr(
    data.files, load_export_pcr_file, pattern=pattern
  )
  
  return(results)
}

# rt.pcr.export.file <- list.files(analysis.dir, pattern = "component", full.names = T)

# Read Real-Time PCR export files 
# rt.pcr.raw <- read_csv(rt.pcr.export.file)
# ncols.rt.pcr.raw <- ncol(rt.pcr.raw)
