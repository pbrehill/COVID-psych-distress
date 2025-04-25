first_treated <- function(y, start_wave = 1) {
  levels_wave <- levels(did_data$wave_fac)
  level_slicer <- levels_wave[start_wave:length(levels_wave)]
  
  ids <- y$anu_id
  
  comp_data <- y %>%
    select(anu_id, wave_fac, lockdown) %>%
    filter(wave_fac %in% level_slicer)
  
  comp_data <- comp_data %>% filter(lockdown %>% as.logical & !is.na(lockdown))
  
  lookup <- comp_data %>%
    group_by(anu_id) %>%
    summarise(min_wave = min(wave_fac %>% as.numeric())
    ) %>%
    select(anu_id, min_wave)
  
  left_join(y, lookup, by = "anu_id") %>%
    mutate(min_wave = ifelse(is.na(min_wave), 0, min_wave))
}


calculate_z_score <- function(beta1, beta2, SE_beta1, SE_beta2) {
  numerator <- beta1 - beta2
  denominator <- sqrt(SE_beta1^2 + SE_beta2^2)
  z_score <- numerator / denominator
  results <- data.frame(diff = numerator, z_score = z_score)
  mutate(
    results,
    p_value = 2 * (1 - pnorm(abs(z_score)))
    )
}


z_test_coefficients <- function (table, variable_families) {
  comparison_table <- table %>%
    left_join(variable_families, by = c("group" = "var"))
  
  base_cases <- comparison_table %>%
    filter(basecase == 1) %>%
    right_join(comparison_table, by = c("family", "g", "time"))
  
  tabley <- calculate_z_score(base_cases$att.y, base_cases$att.x, base_cases$se.y, base_cases$se.x)
  tabley %>%
    bind_cols(select(base_cases, group.y, g, time, n.y)) %>%
    rename(Group = g, Variable = group.y, Difference = diff, Z = z_score, p = p_value, n = n.y) %>%
    select(Group, Variable, Difference, Z, p, n)
}


one_hot_to_factor <- function(df, selection_criteria) {
  # Correctly apply the selection criteria within a dplyr selection context
  df_selected <- df %>% select({{selection_criteria}})
  selected_cols <- names(df_selected)
  
  # Apply the conversion logic
  factor_vector <- apply(df_selected, 1, function(row) {
    selected <- which(row == 1)
    
    if(length(selected) == 0) {
      return("base case")
    } else {
      return(selected_cols[selected])
    }
  })
  
  return(factor(factor_vector))
}


get_frequencies <- function(data, groups, selector, selected_wave) {
  map(groups, ~data %>% mutate(age = one_hot_to_factor(period2, !!rlang::syms(selector))) %>% filter(!is.na(long_k6), wave_num == selected_wave) %>% with(table(age, wave_num)) %>% as.data.frame() %>% filter(RegionCode %in% .x) %>% group_by(wave_num, age) %>% summarise(sum(Freq)))
}
