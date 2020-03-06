# load design file

library(readxl)

data.gene.function.dir <- file.path(data.raw.dir, "gene_function")
cyanobase.function.file.pattern <- "^(.*/)?([0-9]{8})_cyanobase_function.csv$"

# Load submission data

collect_gene_function_files <- function( path = NULL, pattern = NULL ) {
  
  if (is.null(path)) {
    path <- data.gene.function.dir
  }
  
  if (is.null(pattern)) {
    pattern <- cyanobase.function.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_gene_function_file <- function( path, pattern = NULL) {
  
  read_csv(path)
  
}

load_gene_functions <- function ( data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(pattern)) {
    pattern <- cyanobase.function.file.pattern
  }
  
  if (is.null(data.files)) {
    data.files <- collect_gene_function_files(path, pattern)
  }
  
  map_dfr(
    data.files, load_gene_function_file, pattern = pattern
  )
}
