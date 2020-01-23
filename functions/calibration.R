# Partition a data.frame in overlapping windows of varying length
# @param d: data.frame to partition 
# @param min_length: minimum length of the windows
partition <- function( d, min_length=4) {
  rows <- nrow(d)
  start_idxs <- seq(1, rows - min_length)
  
  d <- d %>%
    mutate( idx = row_number())
  
  map_dfr(start_idxs, function(s) {
    end_idxs <- seq(s + min_length - 1, rows)
    map_dfr(end_idxs, function(e, s) {
      # partition
      d %>% 
        slice(s:e) %>%
        # filter( idx >= s, idx <= e) %>%
        mutate( start_idx = s, end_idx = e ) %>%
        group_by(start_idx, end_idx) %>%
        nest() %>%
        mutate( n = map_dbl(data, nrow))
    }, s=s)
  })
}

predict_data <- function(model, data, keep.all=TRUE) {
  
  predictions <- predict(model, newdata=data)
  
  if (keep.all) {
    return(data %>% mutate( od_predicted = predictions))
  }
  
  return(predictions)
}

predict_values <- function( model, x, values, keep.all=TRUE) {
  x <- rlang::enquo(x)
  
  d <- data.frame(s = values) %>%
    rename( !!quo_name(x) := "s")
  
  return(
    predict_data(model, d, keep.all = keep.all)
  )
}

predict_range <- function(m, x, from, to, .interval = 0.001, keep.all=TRUE) {
  x <- rlang::enquo(x)
  
  values <- seq(from, to, .interval)
  
  return(
    predict_values(m, !!x, values, keep.all = keep.all)
  )
}

# Extract the Coefficients
extract_coeffs <- function(mod, keep.all = TRUE){
  # browser()
  tidy.model <- mod %>% tidy
  idxs <- seq_len(nrow(tidy.model))
  
  df.out <-  map(idxs, .f = spread_tidy_model, tidy.model = tidy.model, keep.all = keep.all) %>% 
    bind_cols()
  return(df.out)
}

# Extract the accuracy from the predicted data
extract_accuracy <- function( d, keep.all = TRUE) {
  # browser()
  
  result <- d %>%
    filter(od_actual > 0) %>%
    mutate(accuracy = abs(od_predicted / od_actual))
  
  if (keep.all)
    return(result)
  return(result %>% pull(accuracy))
}

extract_mse <-function(d, keep.all =TRUE) {
  
  result <- d %>%
    summarise( mse = mean((d$od_actual - d$od_predicted)^2, na.rm=TRUE) )
  
  if (keep.all)
    return(result)
  return(result %>% pull(mse))
}

spread_tidy_model <- function(idx, tidy.model, keep.all = TRUE){
  # browser()
  # idx <- 3
  if(idx == 1){
    df.out <- tidy.model %>% 
      slice(idx) %>% 
      select(coeff.0 = estimate, std.error.coeff.0 = std.error, p.value.coeff.0 = p.value)
  } else{
    slope.col.name <- paste("coeff", idx - 1, sep = ".")
    std.error.col.name <-paste("std.error.coeff", idx - 1, sep = ".")
    p.value.col.name <-paste("p.value.coeff", idx - 1, sep = ".")
    df.out <- tidy.model %>% 
      slice(idx) %>% 
      # http://stackoverflow.com/questions/26619329/dplyr-rename-standard-evaluation-function-not-working-as-expected
      select_(.dots = c(setNames("estimate", slope.col.name), 
                        setNames("std.error", std.error.col.name),
                        setNames("p.value", p.value.col.name)))
  }
  
  if(keep.all){
    return(df.out)  
  }else{
    return(df.out[1])
  }
  
}

extract_rsq <- function(mod){
  
  df.out <- mod %>% glance %>% 
    select(r.squared, adj.r.squared, p.value) 
  return(df.out)
}

extract_residuals <- function(mod){
  df.out <- mod %>% augment %>% 
    select(.resid) 
  return(df.out)
}

extract_linear_models <- function(d) {
  # linear model
  linear_coeffs <- unname(coef(d$model[[1]]))
  result <- data.frame(
    degree = 0:(length(linear_coeffs) -1),
    value = linear_coeffs,
    from = d %>% pull(start_od) %>% first(),
    to = d %>% pull(end_od) %>% first()
  )
  
  return(
    result %>% 
      mutate_at(
        vars(to, from),
        funs( ifelse(is.na(.), Inf, .) )
      )
  )
}

extract_poly_models <- function(d) {
  # polynomial
  poly_coeffs <- unname(coef(d$poly_model[[1]]))
  result <- data.frame(
    degree = 0:(length(poly_coeffs) -1),
    value = poly_coeffs,
    from = d %>% pull(start_point) %>% first(),
    to = d %>% pull(end_point) %>% first(),
    model = 1
  )
  
  return(
    result %>% 
      mutate_at(
        vars(to, from),
        funs( ifelse(is.na(.), Inf, .) )
      )
  )
}

extract_models <- function(d) {
  
  # linear model
  linear_coeffs <- unname(coef(d$linear_model[[1]]))
  df.linear <- data.frame(
    degree = 0:(length(linear_coeffs) -1),
    value = linear_coeffs,
    from = -Inf,
    to = d %>% pull(transition_point) %>% first(),
    model = 0
  )
  
  # polynomial
  poly_coeffs <- unname(coef(d$poly_model[[1]]))
  df.poly <- data.frame(
    degree = 0:(length(poly_coeffs) -1),
    value = poly_coeffs,
    from = d %>% pull(transition_point) %>% first(),
    to = d %>% pull(limit_point) %>% first(),
    model = 1
  )
  
  result <- bind_rows(
    df.linear, df.poly
  )
  
  # limit
  limit_value <- d %>% pull(limit_value) %>% first()
  
  if (!is.infinite(limit_value) & !is.na(limit_value)) {
    df.limit <- data.frame(
      degree = 0,
      value = limit_value,
      from = d %>% pull(limit_point) %>% first(),
      to = Inf,
      model = 2
    )
    
    result <- bind_rows(
      result, df.limit
    )
  }
  
  return(
    result %>% 
      mutate_at(
        vars(to, from),
        funs( ifelse(is.na(.), Inf, .) )
      )
  )
}