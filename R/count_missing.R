# Check data missingness

missing_count <- function(df, var){

  var_quo <- sym(var)

  n_total <- nrow(df)

  n_missing <- df %>%
    filter(is.na(!!var_quo)) %>%
    nrow()

  output <- tibble(
    variable = var,
    n_missing = n_missing,
    freq_missing = n_missing/n_total,
    per_missing = freq_missing*100
  )

  return(output)

}

