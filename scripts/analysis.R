library(tidyverse)

pts <- seq(0, 1, 0.01)

poly_area <- function(group_data) {
  model <- tryCatch(
    langcog::clm(prop_class ~ I(prop_total^3) + I(prop_total^2) + prop_total - 1, 
                 data = group_data),
    error = function(e) return(NULL)
  )
  if (is.null(model)) return(NA)
  return((model$solution %*% c(1/4, 1/3, 1/2) - 0.5)[1]) 
}

sample_areas <- function(vocab_data, nboot = 10000, verbose = FALSE,
                         mode = "bootstrap", group = NULL) {
  
  vocab_wide <- vocab_data |> 
    group_by(data_id) |> 
    select(all_of(setdiff(colnames(vocab_data), 
                          c("produces", "understands", "n")))) |> 
    # pivot wider to sample participants, not administrations
    pivot_wider(names_from = c(language, lex_cat),
                values_from = c(prop_class, prop_total))
  
  sample_area <- function(i) {
    vocab_sampled <- case_when(
      mode == "bootstrap" ~ vocab_wide |> 
        group_by(lang_group) |> 
        slice_sample(prop = 1, replace = TRUE),
      mode == "permute" ~ vocab_wide |> 
        group_by(across(all_of(group))) |> 
        mutate(lang_group = sample(lang_group, replace = FALSE))
    )
    
    areas_sampled <- vocab_sampled |> 
      # pivoting back requires a bit more finesse because of the data structure
      pivot_longer(cols = starts_with("prop_"),
                   names_to = c("prop_type", "language", "lex_cat"),
                   names_pattern = "(prop_[a-z]*)_([A-Z][a-z]*)_([a-z_]*)") |> 
      pivot_wider(names_from = prop_type,
                  values_from = value,
                  values_fn = unique) |> 
      filter(!is.na(prop_total)) |> 
      group_by(language, lang_group, lex_cat) |> 
      nest(data = -c("language", "lang_group", "lex_cat")) |> 
      mutate(area = map_dbl(data, poly_area),
             sample = i) |> 
      select(-data) |> 
      ungroup()
  }
  
  map_df(1:nboot, sample_area)
}

permute_areas <- partial(sample_areas, mode = "permute")

std_bias <- function(treatment, control) {
  if (class(treatment) == "character") {
    dat_levels <- unique(c(treatment, control))
    treatment <- factor(treatment, levels = dat_levels) |> as.numeric()
    control <- factor(control, levels = dat_levels) |> as.numeric()
  }
  
  (mean(treatment, na.rm = TRUE) - mean(control, na.rm = TRUE)) / sd(treatment, na.rm = TRUE)
}

std_bias_check <- function(full_data, matched_data, covariate,
                           treatment, control) {
  sb_full <- std_bias(
    full_data |> 
      filter(!!enexpr(treatment)) |> 
      pull(!!enquo(covariate)),
    full_data |> 
      filter(!!enexpr(control)) |> 
      pull(!!enquo(covariate))
  )
  sb_matched <- std_bias(
    matched_data |> 
      filter(!!enexpr(treatment)) |> 
      pull(!!enquo(covariate)),
    matched_data |> 
      filter(!!enexpr(control)) |> 
      pull(!!enquo(covariate))
  )
  tibble(data = c("full", "matched"),
         std_bias = c(sb_full, sb_matched))
}

