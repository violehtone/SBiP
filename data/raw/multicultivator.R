# devtools::install_git("https://gitlab.com/mmp-uva/mmpr.git")
library(mmpr)

collect_multicultivator_files <- function(path = NULL) {
  
  if (is.null(path)) {
    path <- file.path(data.raw.dir, "multicultivator")
  }
  
  find_pycultivator_data(path)
}

load_multicultiator_od <- function( data.files = NULL ) {
  
  if (is.null(data.files)) {
    data.files <- collect_multicultivator_files(path = data.dir)
  }
  
  map_dfr(
    data.files, load_pycultivator_sqlite,
  )
  
}

load_multicultiator_pump <- function( data.files = NULL ) {
  
  if (is.null(data.files)) {
    data.files <- collect_multicultivator_files(path = data.dir)
  }
  
  map_dfr(
    data.files, load_turbidostat_sqlite,
  )
  
}

load_multicultivator <- function(data.files = NULL, data.dir = NULL, ...) {
  
  if (is.null(data.files)) {
    data.files <- collect_multicultivator_files(path = data.dir)
  }
  
  df.multicultivator.raw <- data.files %>%
    mutate(
      cultivation = map(path, load_pycultivator_sqlite),
      turbidostat = map(path, load_turbidostat_sqlite),
      data = map2(cultivation, turbidostat, combine_turbidostat),
      data = map(data, na.omit)
    ) %>%
    select(-path, -format) %>% 
    rename(multicultivator_datafile = file_name)
  
  df.multicultivator.pump <- df.multicultivator.raw %>% 
    unnest(data) %>%
    ungroup() %>%
    filter(od_led == 720) %>%
    select(name, channel, time_h, decision, pump)
  
  df.multicultivator <-  left_join(
    df.multicultivator.raw %>% unnest(cultivation) %>% mutate(t = time_h %/% (5 / 60)),
    df.multicultivator.pump %>% mutate(t = time_h %/% (5 / 60)) %>% select(-time_h),
    by = c("name", "channel", "t")
  ) %>%
    mutate(
      multicultivator = as.numeric(sub("MC([0-9]).*", "\\1", name)),
      pump = ifelse(is.na(pump), F, pump),
      pump = as.logical(pump),
      channel = channel + 1
    )  %>%
    select(-t) 
  
  return(df.multicultivator)
}

