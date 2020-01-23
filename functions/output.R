
#' Function to write files to the processed data dir
#' 
#' @param data Data.frame with the data that should be written
#' @param name character with the name of the data source (will be the directory in output dir)
#' @param file.name optional character with the name of the data file to write (otherwise uses name)
#' @param .output.dir character with the path to the output dir (defaults to `data.processed.dir`)
#' @param .output.precision numeric setting the number of decimal places to print in the CSV
#' @return character with the path of the file that was written
write_output_csv <- function( 
  data, name, file.name=NULL, .output.dir = data.processed.dir, .output.precision = output.precision,
  .return.relative = TRUE
) {
  
  # if no file name is specified use source name
  if(is.null(file.name)) {
    file.name <- name
  }
  
  # if no extension, add .csv
  if (stringr::str_ends( file.name, ".csv", negate = TRUE)) {
    file.name <- paste0(file.name, ".csv")
  }
  
  # generate output dir path
  source.output.dir <- file.path(.output.dir, name)
  
  # if source output dir does not exist, create it
  if (!dir.exists(source.output.dir)) {
    dir.create(source.output.dir)
  }
  
  # generate timestamped filename
  source.output.file <- mmpr::stamp_filename(file.name)
  # generate output file path
  source.output <- file.path(source.output.dir, source.output.file)
  
  # if round precision is given, round
  if (!is.null(.output.precision)) {
    data <- data %>% 
      mutate_if(
        is.numeric,
        round, digits = .output.precision
      )
  }
  
  # write data
  data %>% 
    write_csv(source.output)
  
  # make it a "relative" path if desired
  if (.return.relative) {
    source.output <- str_replace(source.output, here::here(), "")
  }
  
  # return file path
  return(source.output)
}

#' Function to automatically read processed data of the given data source
#' 
#' @param name character with the name of the data source (will be the directory in output dir)
#' @param file.name optional character with the name of the data file to write (otherwise uses name)
#' @param .output.dir character with the path to the output dir (defaults to `data.processed.dir`)
#' 
#' @return data.frame 
read_output_csv <- function( 
  name, file.name=NULL, .output.dir = data.processed.dir,
  .annotate.dataset=FALSE, .filter.recent = TRUE
) {
  
  # if no file name is specified use source name
  if(is.null(file.name)) {
    file.name <- name
  }
  
  # if no extension, add .csv
  if (stringr::str_ends( file.name, ".csv", negate = TRUE)) {
    file.name <- paste0(file.name, ".csv")
  }
  
  # generate output dir path
  source.output.dir <- file.path(.output.dir, name)
  
  # generate pattern
  output.file.pattern <- file.path("(.*", sprintf(")?([0-9]{8})_%s", file.name))
  
  # find files matching
  output.file.paths <- list.files(
    source.output.dir, output.file.pattern, full.names = TRUE
  )
  
  # only keep most recent
  if (.filter.recent) {
    output.file.paths <- data.frame(
      p = output.file.paths, stringsAsFactors = FALSE
    ) %>%
      mutate(
        dataset = lubridate::ymd(sub(output.file.pattern, "\\2", p))
      ) %>%
      filter(dataset == max(dataset)) %>%
      pull(p) 
  }
  
  read_output_csv_file <- function( p) {
    d <- sub(output.file.pattern, "\\2", p)
    
    result <- read_csv(p)
   
    if (.annotate.dataset) { 
      result <- result %>% mutate(file_dataset = d)
    }

    return(result)
  }
  
  result <- suppressMessages(
    map_dfr( output.file.paths, read_output_csv_file)
  )
  
  return(result)
}

