library(tidyverse)
library(readr)


data.casy.dir <- file.path(data.raw.dir, "casy")
block.casy.dir.pattern <- "^(.*/)?([0-9]{6})_([0-9]{1})"
design.casy.file.pattern <- "^(.*/)?([0-9]{8})_casy_design.xlsx$"


collect_casy_design_files <- function(path, pattern = NULL) {
  
  if (is.null(path)) {
    path <- data.casy.dir
  }
  
  if (is.null(pattern)) {
    pattern <- design.casy.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

read_casy_design <- function(path) {
  
  result <- read_excel(
    path, sheet = "casy"
  ) %>% 
    mutate(
      sample_dilution = as.numeric(sample_dilution)
    )
  return(result)
  
}


load_casy_design <- function( data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(pattern)) {
    pattern <- design.casy.file.pattern
  }
  
  if (is.null(data.files)) { 
    data.files <- collect_casy_design_files(path = path, pattern = pattern)  
  }
  
  results <- map_dfr(
    data.files, read_casy_design
  ) 
  
  return(results)
  
}

collect_casy_output_dirs <- function(path, pattern = NULL) {
  
  if (is.null(path)) {
    path <- data.casy.dir
  }
  
  if (is.null(pattern)) {
    pattern <- block.casy.dir.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
}

read_casy_output_file <- function(path) {
  
  row.n.header <- 2
  
  df.raw <- read_delim(
    path,
    delim = "\t",
    skip = row.n.header,
    col_names = FALSE
  ) %>%
    mutate(row = row_number()) 
  
  row.n.start.counts <- df.raw %>%
    filter(X1 == "Size Channel") %>%
    pull(row)
  
  row.n.end.counts <- df.raw %>%
    filter(X1 == "Sum") %>%
    pull(row)
  
  df.meta <- read_delim(
    path,
    delim = "\t",
    skip = row.n.header,
    n_max = row.n.start.counts - 1,
    col_names = c("param", "value")
  )
  
  result <- read_delim(
    path,
    delim = "\t",
    skip = row.n.start.counts + 3, n_max = row.n.end.counts - row.n.start.counts - 1,
    col_names = c("size_um", "counts_sum", "counts_1", "counts_2", "counts_3"),
    col_types = cols(
      size_um = col_number(),
      counts_sum = col_number(),
      counts_1 = col_number(),
      counts_2 = col_number(),
      counts_3 = col_number()
    )
  ) %>% 
    mutate(
      measurement_name = df.meta %>% filter(param == "Measurement Name") %>% pull(value) %>% as.numeric(),
      sample_name = df.meta %>% filter(param == "Comment") %>% pull(value),
      measurement_volume = df.meta %>% filter(str_detect(param, "Sample Volume")) %>% pull(value) %>% as.numeric(),
      casy_dilution = df.meta %>% filter(param == "Dilution") %>% pull(value) %>% as.numeric()
    )
  
  return(result)         
}

load_casy_output_block <- function(dir){

  files <- list.files(dir, full.names = T)
  
  block.name <- str_split(dir, "/")[[1]] %>% last() 
  
  result <- map_dfr(files, read_casy_output_file) %>% 
    mutate(
      block_name = block.name
    )
  
  return(result)
  
}

load_casy_output_blocks <- function(data.dirs = NULL, path=NULL, pattern=NULL) {
  
  if (is.null(pattern)) {
    pattern <- block.casy.dir.pattern
  }
  
  if (is.null(data.dirs)) { 
    data.dirs <- collect_casy_output_dirs(path = path, pattern = pattern)  
  }
  
  results <- map_dfr(data.dirs, load_casy_output_block) %>% 
    select(-counts_sum) %>% 
    gather(replicate, counts, 2:4) %>% 
    mutate(
      replicate = str_remove(replicate, "counts_")
    )
  
  return(results)
}

load_casy <- function() {
  
  df.casy.raw <- load_casy_output_blocks()
  
  df.results <- left_join(
    df.casy.design, df.casy.raw,
    by = c("measurement_name", "block_name")
  ) %>% 
    select(-sample_name, -casy_dilution)
}

