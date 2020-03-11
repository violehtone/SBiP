# load design file

library(readxl)

data.go.terms.dir <- file.path(data.raw.dir, "go_terms")
cyanobase.go.terms.file.pattern <- "^(.*/)?([0-9]{8})_goterm.txt$"

# Load submission data

collect_go_terms_files <- function( path = NULL, pattern = NULL ) {
  
  if (is.null(path)) {
    path <- data.go.terms.dir
  }
  
  if (is.null(pattern)) {
    pattern <- cyanobase.go.terms.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_go_terms_file <- function( path, pattern = NULL) {

    result <- read_delim(path, delim = "\t",
             col_names = c("species", "gene_id", "name", "full_go_term")) %>% 
    mutate(
      go_id = str_extract(full_go_term, "GO:[0-9]{7}")
    ) %>% 
    select(gene_id, full_go_term, go_id) %>% 
    mutate(
      full_go_term = str_remove(full_go_term, "\\(GO:[0-9]{7}\\)")
    ) %>% 
    separate(full_go_term, sep = ":", into = c("category", "description"), extra =  "merge") %>% 
    mutate(
      go_term = str_c(go_id, description, sep = " ") %>% str_trim(.)
    ) 
  return(result)
}

load_go_terms <- function ( data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(pattern)) {
    pattern <- cyanobase.go.terms.file.pattern
  }
  
  if (is.null(data.files)) {
    data.files <- collect_go_terms_files(path, pattern)
  }
  
  map_dfr(
    data.files, load_go_terms_file, pattern = pattern
  )
}
