# load design file

library(readxl)

load_calibration_data <- function() {
  file.pattern <- "(.*/)?([0-9]{8})_MC([0-9])_OD_calibration.xlsx"
  data.files <- list.files(
    file.path(data.raw.dir, "od_calibration"), file.pattern, full.names = TRUE
  )
  
  load_calibration_data_file <- function(path, pattern) {
    
    dataset <- sub(pattern, "\\2", path)
    multicultivator <- as.numeric(sub(pattern, "\\3", path))
    
    return(
      read_excel(path) %>%
        mutate(dataset = dataset, multicultivator = multicultivator)
    )
  }
 
  df.result <- map_dfr(
    data.files, load_calibration_data_file, pattern = file.pattern
  ) %>%
    select(-c(volume_sample, volume_blank, od_lab_raw)) %>% 
    # average the two MC OD values before and after sampling
    group_by_at(vars(-contains("od_mc"))) %>%
    summarise(
      od_mc_raw = mean(od_mc_raw),
      od_mc_predicted = mean(od_mc_predicted)
    )
    
  return(df.result)
}

load_calibration_models <- function() {
  
  file.pattern <- "(.*/)?([0-9]{8})_linear_calibration_models_mc([0-9]).csv"
  
  model_files <- list.files(
    file.path(data.raw.dir, "od_calibration"), file.pattern, full.names = TRUE
  )

  load_calibration_model <- function(path, pattern) {
    
    dataset <- sub(pattern, "\\2", path)
    
    return(
      read_csv(path) %>%
        mutate(dataset = dataset)
    )
  }
    
  
  return(
    map_dfr(model_files, load_calibration_model, pattern=file.pattern)
  )
}
