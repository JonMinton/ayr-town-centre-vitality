# calculate_indicators.R
# Calculate demographic indicators, rankings, and similarity measures

library(dplyr)
library(tidyr)

#' Calculate median age from single-year-of-age population data
#'
#' Uses linear interpolation on the cumulative distribution.
#'
#' @param pop_vec Named or unnamed numeric vector of population by age (0-90)
#' @param ages Integer vector of ages corresponding to pop_vec
#' @return Numeric median age
calculate_median_age <- function(pop_vec, ages) {
  total <- sum(pop_vec)
  cum_pop <- cumsum(pop_vec)
  half <- total / 2

  # Find the age where cumulative pop crosses 50%
  idx <- which(cum_pop >= half)[1]
  if (idx == 1) return(ages[1])

  # Linear interpolation
  lower_cum <- cum_pop[idx - 1]
  upper_cum <- cum_pop[idx]
  ages[idx - 1] + (half - lower_cum) / (upper_cum - lower_cum)
}

#' Calculate all demographic indicators for each council area
#'
#' @param pop_df Population data from get_population_data(), filtered to one year.
#'   Must have columns: council_area_code, council_area_name, age, sex, population.
#' @return Tibble with one row per area and indicator columns
calculate_all_indicators <- function(pop_df) {
  # Use sex == "all" (Persons) for indicators
  persons <- pop_df |>
    filter(sex == "all") |>
    arrange(council_area_code, age)

  persons |>
    group_by(council_area_code, council_area_name) |>
    summarise(
      total_population = sum(population),
      median_age = calculate_median_age(population, age),
      mean_age = weighted.mean(age, population),
      sd_age = sqrt(weighted.mean((age - weighted.mean(age, population))^2, population)),
      pop_0_15 = sum(population[age <= 15]),
      pop_16_64 = sum(population[age >= 16 & age <= 64]),
      pop_65_plus = sum(population[age >= 65]),
      pop_75_plus = sum(population[age >= 75]),
      child_dep_ratio = pop_0_15 / pop_16_64,
      old_age_dep_ratio = pop_65_plus / pop_16_64,
      total_dep_ratio = (pop_0_15 + pop_65_plus) / pop_16_64,
      working_age_prop = pop_16_64 / total_population,
      prop_65_plus = pop_65_plus / total_population,
      prop_75_plus = pop_75_plus / total_population,
      .groups = "drop"
    )
}

#' Add rank columns for each indicator
#'
#' Rank 1 = highest value for each indicator.
#'
#' @param indicators_df Output of calculate_all_indicators()
#' @param focus_area Council area code to flag (default: South Ayrshire)
#' @return Augmented tibble with rank columns and is_focus flag
rank_indicators <- function(indicators_df, focus_area = "S12000028") {
  rank_cols <- c("median_age", "mean_age", "old_age_dep_ratio", "child_dep_ratio",
                 "total_dep_ratio", "working_age_prop", "prop_65_plus", "prop_75_plus")

  result <- indicators_df |>
    mutate(is_focus = council_area_code == focus_area)

  for (col in rank_cols) {
    rank_name <- paste0(col, "_rank")
    result[[rank_name]] <- rank(-result[[col]], ties.method = "min")
  }

  result
}

#' Calculate demographic similarity to a focus area
#'
#' Standardises indicators (z-scores) and computes Euclidean distance.
#'
#' @param indicators_df Output of calculate_all_indicators()
#' @param focus_area Council area code (default: South Ayrshire)
#' @return Tibble ranked by similarity (smallest distance = most similar)
calculate_demographic_similarity <- function(indicators_df,
                                              focus_area = "S12000028") {
  sim_cols <- c("median_age", "old_age_dep_ratio", "child_dep_ratio",
                "working_age_prop", "sd_age", "prop_75_plus")

  # Exclude Scotland (national aggregate, not a peer)
  df <- indicators_df |>
    filter(council_area_code != "S92000003")

  # Z-score standardisation
  z_df <- df |>
    mutate(across(all_of(sim_cols), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
                  .names = "z_{.col}"))

  z_cols <- paste0("z_", sim_cols)

  # Extract focus area z-scores

  focus_z <- z_df |>
    filter(council_area_code == focus_area) |>
    select(all_of(z_cols)) |>
    unlist()

  # Calculate Euclidean distance for all other areas
  others <- z_df |>
    filter(council_area_code != focus_area)

  other_matrix <- others |>
    select(all_of(z_cols)) |>
    as.matrix()

  distances <- sqrt(rowSums(sweep(other_matrix, 2, focus_z)^2))

  others |>
    select(council_area_code, council_area_name, all_of(sim_cols), total_population) |>
    mutate(
      distance = distances,
      similarity_rank = rank(distance, ties.method = "min")
    ) |>
    arrange(distance)
}
