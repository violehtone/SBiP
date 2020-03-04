# function to generate pca

f_pca <- function( .d, counts_from, features_from, samples_from ) {
  
  .d %>%
    nest() %>%
    mutate(
      # turn data into wide format
      data = map(data, function(.d) { 
        .d %>%
          pivot_wider(
            names_from = features_from, 
            values_from = counts_from
          )
      }),
      # perform pca
      pca = map(data, function(.d) { 
        .d %>% column_to_rownames(samples_from) %>% prcomp
      }),
      # extract variance information
      pca_aug = map2(pca, data, ~augment(.x, data = .y)),
      # extract loading information
      pca_loadings = map(pca, function(.d) {
        .d$rotation %>% 
          as_tibble(rownames = features_from) %>%
          pivot_longer(
            cols = starts_with("PC"),
            names_to = "PC",
            names_prefix = "PC",
            values_to = "loading"
          ) %>%
          mutate(PC = as.numeric(PC))
      })
    )
  
}

f_pca_var_exp <- function( .d ) {
  
  .d %>%
    unnest(pca_aug) %>% 
    summarize_at(.vars = vars(starts_with(".fittedPC")), .funs = list(var)) %>% 
    pivot_longer(
      cols = starts_with(".fittedPC"),
      names_to = "pc",
      names_prefix = ".fittedPC",
      values_to = "variance"
    ) %>% 
    mutate(
      pc = as.numeric(pc),
      var_exp = variance/sum(variance),
      cum_var_exp = cumsum(var_exp),
    )
  
}

# Custom function to plot pca results 
f_pca_plot <- function(
  pca.object, data, x.pc = 1, y.pc = 2, design = NULL, design.key = NULL, colour = NULL, label = NULL
){
  
  if(!is.null(design)) {
    stopifnot(!is.null(design.key))
    
    data <- left_join(data, design, by = design.key)
  }
  
  results <- autoplot(
    object = pca.object, data = data, 
    x = x.pc,
    y = y.pc, 
    loadings = FALSE, 
    colour = colour,
    label = !is.null(label),
    label.label = label,
    label.repel = !is.null(label)
  ) +
    theme_bw() +
    labs(
      x = glue::glue("Principal Component {x.pc}"),
      y = glue::glue("Principal Component {y.pc}"),
      title = glue::glue("PCA on PC {x.pc} and PC {y.pc}")
    )

  return(results)
}

f_pca_plot_var_exp <- function(.d) {
  results <- .d %>%
    rename(
      `Variance Explained` = var_exp,
      `Cumulative Variance Explained` = cum_var_exp
    ) %>% 
    pivot_longer(
      cols = `Variance Explained`:`Cumulative Variance Explained`,
      names_to = "key",
      values_to = "value"
    ) %>% 
    ggplot(aes(as.factor(pc), value, group = key)) + 
    geom_point() + 
    geom_line() + 
    facet_wrap(~key, scales = "free_y") +
    theme_bw() +
    lims(y = c(0, 1)) +
    labs(
      x = "Principal Component",
      y = "Variance",
      title = "Variance explained by each principal component"
    )
  
  return(results)
}