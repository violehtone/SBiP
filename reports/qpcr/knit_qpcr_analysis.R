library(tidyverse)
source(here::here("settings.R"))

knit_qpcr_analysis <- function( plate.id, rm.outliers = FALSE) {
  
  label <- ifelse(rm.outliers, "processed", "raw")
  
  rmarkdown::render(
    file.path(reports.dir, "qpcr", "qpcr_analysis_template.Rmd"), 
    params = list(
      dataset = plate.id,
      processed = rm.outliers
    ),
    output_file = glue::glue('{plate.id}_qpcr_{label}_analysis')
  )
  
}

batch_knit_qcpr_analysis <- function(plate.ids = NULL, rm.outliers = NULL) {
  
  # collect all plate ids in data raw
  if (is.null(plate.ids)) {
    plate.ids <- list.dirs(file.path(data.raw.dir, "qpcr"), full.names = F, recursive = F)
  }
  
  if (is.null(rm.outliers)) {
    rm.outliers <- c(F, T)
  }
  
  expand_grid(
    plate.id = plate.ids, rm.outliers = rm.outliers
  ) %>%
    pmap(
      ., knit_qpcr_analysis
    )
}

# Example (COPY TO CONSOLE WITHOUT COMMENT)
# knit_qpcr_analysis( "200213_1" )

# Or generate all reports for every plate id in the data (might take a while)
# batch_knit_qcpr_analysis()
# batch_knit_qcpr_analysis(c("200212_1", "200213_1"), T)
