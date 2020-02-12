
# function used to calculate growth rates
f_growth <- function(d, .f=turbidostat_growth, method=regression.method, ...) {
  
  f_g <- function(.x) {
    .f(.x, time_h, OD, method=method, ...)
  }
  
  d %>% 
    # remove data from before turbidostat was enabled
    filter(!is.na(decision)) %>%
    # nest so the data of each group is in a separate data.frame
    nest() %>%
    mutate(
      # calculate growth rates per group
      rates = map(data, f_g ),
    ) %>%
    # return data.frame to original shape (i.e. no nested groups)
    unnest(rates)
}

# function used to calculate growth rates (parallel, requires 'future' package)
f_growth_p <- function(d, .f=turbidostat_growth, method=regression.method, ...) {
  
  f_g <- function(.x) {
    .f(.x, time_h, OD, method=method, ...)
  }
  
  d %>% 
    # remove data obtained when turbidostat was not active (i.e. no was decision made)
    filter(!is.na(decision)) %>%
    # nest so the data of each group is in a separate data.frame
    nest() %>%
    mutate(
      # calculate growth rates per group
      rates = map(data, ~future(f_g) ),
      # use values to retrieve the data
      rates = values(rates)
    ) %>%
    # return data.frame to original shape (i.e. no nested groups)
    unnest(rates)
}

f_growth_filter <- function(d) {
  d %>%
    filter(r.squared > r.squared.threshold, slope < slope.cutoff, slope > 0 )
}

f_generations_slope <- function(d) {
  d %>%
    mutate(
      # slope = growth factor = ln(2) / doubling time
      # T doubling  = ln(2) / slope = w_time / number of generations
      # generations (doublings) = w_time / T doubling = w_time / (ln(2) / slope)
      generation_d = w_time / (log(2) / slope),
      generations = accumulate(generation_d, `+`)
    )
}

f_generations_d <- function(d) {
  d %>%
    mutate(
      # dilution_rate = growth factor = ln(2) / doubling time
      # T doubling = ln(2) / dilution_rate = w_time / number of generations
      # generations (doublings) = w_time / T doubling = w_time / (ln(2) / dilution_rate)
      generation_d = time_h_d / (log(2) / dilution_rate),
      generations = accumulate(generation_d, `+`)
    )
}
