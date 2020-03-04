# load design file

library(readxl)

data.transcriptomics.dir <- file.path(data.raw.dir, "transcriptomics")
transcriptomics.submission.file.pattern <- "^(.*/)?([0-9]{8})_transcriptomics_submission.xlsx$"
transcriptomics.spikes.count.file.pattern <- "^(.*/)?([0-9]{8})_spike_table.tsv"
transcriptomics.spikes.info.file.pattern <- "^(.*/)?([0-9]{8})_spike_info.tsv"
transcriptomics.spikes.sequence.file.pattern <- "^(.*/)?([0-9]{8})_spike_seq.tsv"
transcriptomics.count.file.pattern <- "^(.*/)?([0-9]{8})_counts_(S[0-9]+).tsv$"


# Load submission data

collect_transcriptomics_submission_files <- function( path = NULL, pattern = NULL ) {
  
  if (is.null(path)) {
    path <- data.transcriptomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- transcriptomics.submission.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_transcriptomics_submission_file <- function( path ) {
  
  read_excel(path)
  
}

load_transcriptomics_submission <- function ( data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(data.files)) {
    data.files <- collect_transcriptomics_submission_files(path, pattern)
  }
  
  map_dfr(
    data.files, load_transcriptomics_submission_file
  )
}

# Load Spikes Data

collect_transcriptomics_spikes_count_files <- function( path = NULL, pattern = NULL ) {
  
  if (is.null(path)) {
    path <- data.transcriptomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- transcriptomics.spikes.count.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_transcriptomics_spikes_counts_file <- function(path, pattern) {
  
  dataset <- sub(pattern, "\\2", path)
  
  suppressMessages(
    read_tsv(path)
  ) %>%
    mutate(dataset = dataset) %>%
    rename(spike_id = Transcript) %>%
    pivot_longer(
      cols = matches("S[0-9]{2}"),
      names_to = "sequence_id",
      values_to = "counts"
    )
  
}

load_transcriptomics_spikes_counts <- function(data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(pattern)) {
    pattern <- transcriptomics.spikes.count.file.pattern
  }
  
  if (is.null(data.files)) {
    data.files <- collect_transcriptomics_spikes_count_files(path = path, pattern = pattern)
  }
  
  map_dfr(
    data.files, load_transcriptomics_spikes_counts_file, pattern = pattern
  )
}

collect_transcriptomics_spikes_info_files <- function( path = NULL, pattern = NULL ) {
  
  if (is.null(path)) {
    path <- data.transcriptomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- transcriptomics.spikes.info.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_transcriptomics_spikes_info_file <- function(path, pattern) {
  
  dataset <- sub(pattern, "\\2", path)
  
  suppressMessages(
    read_tsv(
      path, skip=1,
      col_names = c(
        "sort_id", "spike_id", "subgroup",
        "conc_mix_1_attomoles_ul",
        "conc_mix_2_attomoles_ul",
        "expected_fold_change_ratio",
        "log2_mix_1_mix_2"
      )
    )
  ) %>%
    mutate(dataset = dataset)
  
}

load_transcriptomics_spikes_info <- function(data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(pattern)) {
    pattern <- transcriptomics.spikes.info.file.pattern
  }
  
  if (is.null(data.files)) {
    data.files <- collect_transcriptomics_spikes_info_files(path = path, pattern = pattern)
  }
  
  map_dfr(
    data.files, load_transcriptomics_spikes_info_file, pattern = pattern
  )
}

collect_transcriptomics_spikes_sequence_files <- function( path = NULL, pattern = NULL ) {
  
  if (is.null(path)) {
    path <- data.transcriptomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- transcriptomics.spikes.sequence.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_transcriptomics_spikes_sequence_file <- function(path, pattern) {
  
  dataset <- sub(pattern, "\\2", path)
  
  suppressMessages(
    read_tsv(path)
  ) %>%
    rename(spike_id = ERCC_ID) %>%
    mutate(dataset = dataset)
  
}

load_transcriptomics_spikes_sequences <- function(data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(pattern)) {
    pattern <- transcriptomics.spikes.sequence.file.pattern
  }
  
  if (is.null(data.files)) {
    data.files <- collect_transcriptomics_spikes_sequence_files(path = path, pattern = pattern)
  }
  
  map_dfr(
    data.files, load_transcriptomics_spikes_sequence_file, pattern = pattern
  )
}

load_transcriptomics_spikes <- function( df.counts = NULL, df.info = NULL, df.sequences = NULL ) {
  
  if (is.null(df.counts)) {
    df.counts <- load_transcriptomics_spikes_counts()
  }
  
  if (is.null(df.info)) {
    df.info <- load_transcriptomics_spikes_info()
  }
  
  if (is.null(df.sequences)) {
    df.sequences <- load_transcriptomics_spikes_sequences()
  }
  
  left_join(
    df.counts,
    left_join(
      df.info,
      df.sequences, 
      by = c("dataset", "spike_id")
    ),
    by = c("dataset", "spike_id")
  ) %>%
    mutate(
      log2_conc = log2(conc_mix_1_attomoles_ul),
      log2_count = log2(pmax(1000*counts/nchar(sequence_id), 2^-5))
    ) 
  
}

# Load Counts Data

collect_transcriptomics_count_files <- function( path = NULL, pattern = NULL ) {
  
  if (is.null(path)) {
    path <- data.transcriptomics.dir
  }
  
  if (is.null(pattern)) {
    pattern <- transcriptomics.count.file.pattern
  }
  
  list.files(
    path, pattern, full.names = TRUE
  )
  
}

load_transcriptomics_count_file <- function( path, pattern ) {
  
  dataset <- sub(pattern, "\\2", path)
  sequence_id <- sub(pattern, "\\3", path)
  
  suppressMessages(
    read_tsv(path, col_names = c("gene", "counts")) 
  ) %>%
    mutate(dataset = dataset, sequence_id = sequence_id)
  
}

load_transcriptomics <- function ( data.files = NULL, path = NULL, pattern = NULL ) {
  
  if (is.null(pattern)) {
    pattern <- transcriptomics.count.file.pattern
  }
  
  if (is.null(data.files)) {
    data.files <- collect_transcriptomics_count_files(path, pattern)
  }
  
  map_dfr(
    data.files, load_transcriptomics_count_file, pattern = pattern
  )
}

# 
# load_transcriptomics <- function( df.counts = NULL, df.submission = NULL ) {
#   
#   if (is.null(df.counts)) {
#     df.counts <- load_transcriptomics_counts()
#   }
#   
#   if (is.null(df.submission)) {
#     df.submission <- load_transcriptomics_submission()
#   }
#   
#   df.counts %>%
#     left_join(
#       df.submission, by = "sequence_id"
#     )
#   
# }