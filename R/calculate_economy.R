# calculate_economy.R
# Economic indicator calculations using SIMD raw indicators and BRES employment data

library(dplyr)
library(tidyr)
library(sf)
library(here)

#' Coerce SIMD indicator columns from character to numeric
#'
#' Some SIMD indicator columns (crime_rate, crime_count, Attendance, Attainment)
#' are stored as character because they contain '*' suppression markers.
#' This function converts them to numeric, turning '*' into NA.
#'
#' @param df Data frame or sf object with SIMD indicator columns
#' @return Same object with character indicator columns converted to numeric
coerce_simd_indicators <- function(df) {
  char_indicators <- c("crime_rate", "crime_count", "Attendance", "Attainment")
  for (col in intersect(char_indicators, names(df))) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  df
}

#' Summarise SIMD economic indicators by grouping variable
#'
#' Extracts and summarises raw SIMD indicator values (not ranks) relevant
#' to the local economy analysis: income, employment, crime, housing.
#'
#' @param simd_sf sf object with SIMD data and focus_tier column
#' @param group_var Column name to group by (default: "focus_tier")
#' @return Long-format tibble with mean and median per indicator per group
calculate_economy_indicators <- function(simd_sf, group_var = "focus_tier") {
  indicator_cols <- c(
    "Income_rate", "Income_count",
    "Employment_rate", "Employment_count",
    "crime_rate", "crime_count",
    "overcrowded_rate", "nocentralheat_rate",
    "no_qualifications", "not_participating", "University",
    "CIF", "ALCOHOL", "DRUG", "SMR", "DEPRESS", "EMERG"
  )

  available_cols <- intersect(indicator_cols, names(simd_sf))

  simd_sf |>
    coerce_simd_indicators() |>
    st_drop_geometry() |>
    group_by(across(all_of(group_var))) |>
    summarise(
      across(all_of(available_cols),
             list(mean = ~mean(., na.rm = TRUE),
                  median = ~median(., na.rm = TRUE))),
      n_datazones = n(),
      total_population = sum(Population, na.rm = TRUE),
      total_working_age = sum(Working_Age_Population, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = -c(all_of(group_var), n_datazones, total_population, total_working_age),
      names_to = c("indicator", "stat"),
      names_pattern = "(.+)_(mean|median)$",
      values_to = "value"
    )
}

#' Get datazone-level economic indicators for plotting
#'
#' Returns a tidy tibble of key economic indicators per datazone,
#' suitable for dot plots and scatter plots.
#'
#' @param simd_sf sf object with SIMD data and focus_tier column
#' @return Tibble with one row per datazone, key indicator columns
get_datazone_economy <- function(simd_sf) {
  dz_col <- find_datazone_column(simd_sf)

  simd_sf |>
    coerce_simd_indicators() |>
    st_drop_geometry() |>
    select(
      Data_Zone = all_of(dz_col), DZname, focus_tier,
      Population, Working_Age_Population,
      Income_rate, Employment_rate,
      crime_rate, overcrowded_rate, nocentralheat_rate,
      no_qualifications, not_participating,
      CIF, ALCOHOL, DRUG, SMR, DEPRESS, EMERG
    )
}

#' Aggregate BRES employment by tier and sector group
#'
#' @param bres_df BRES tibble from get_bres_data()
#' @param simd_sf sf object with focus_tier classification
#' @return Tibble with employment counts and percentages by tier and sector
calculate_employment_structure <- function(bres_df, simd_sf) {
  source(here("R", "fetch_bres_data.R"))

  dz_col <- find_datazone_column(simd_sf)
  tier_lookup <- simd_sf |>
    st_drop_geometry() |>
    select(Data_Zone = all_of(dz_col), focus_tier)

  bres_df |>
    left_join(tier_lookup, by = "Data_Zone") |>
    mutate(sector = classify_sector(sic_section)) |>
    group_by(focus_tier, sector) |>
    summarise(
      employment = sum(employment, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(focus_tier) |>
    mutate(
      tier_total = sum(employment),
      pct = employment / tier_total * 100
    ) |>
    ungroup() |>
    arrange(focus_tier, desc(employment))
}

#' Calculate jobs per capita by tier
#'
#' @param bres_df BRES tibble from get_bres_data()
#' @param simd_sf sf object with focus_tier and population data
#' @return Tibble with jobs per resident and per working-age person
calculate_jobs_per_capita <- function(bres_df, simd_sf) {
  dz_col <- find_datazone_column(simd_sf)

  pop_by_tier <- simd_sf |>
    st_drop_geometry() |>
    group_by(focus_tier) |>
    summarise(
      population = sum(Population, na.rm = TRUE),
      working_age = sum(Working_Age_Population, na.rm = TRUE),
      .groups = "drop"
    )

  tier_lookup <- simd_sf |>
    st_drop_geometry() |>
    select(Data_Zone = all_of(dz_col), focus_tier)

  jobs_by_tier <- bres_df |>
    left_join(tier_lookup, by = "Data_Zone") |>
    group_by(focus_tier) |>
    summarise(total_jobs = sum(employment, na.rm = TRUE), .groups = "drop")

  pop_by_tier |>
    left_join(jobs_by_tier, by = "focus_tier") |>
    mutate(
      jobs_per_resident = round(total_jobs / population, 2),
      jobs_per_working_age = round(total_jobs / working_age, 2)
    )
}

#' Normalise SIMD indicators to national percentiles
#'
#' Computes where each tier's mean falls in the national distribution
#' of datazone values, expressed as a percentile (0-100).
#' Lower percentile = worse (more deprived / higher crime / etc.).
#'
#' @param simd_all Full national SIMD tibble (all 6,976 datazones)
#' @param simd_sf Local sf object with focus_tier column
#' @param group_var Grouping variable
#' @return Tibble with percentile positions per indicator per tier
calculate_economic_health_profile <- function(simd_all, simd_sf,
                                               group_var = "focus_tier") {
  indicators <- c(
    "Income_rate", "Employment_rate", "crime_rate",
    "overcrowded_rate", "nocentralheat_rate",
    "CIF", "ALCOHOL", "DRUG"
  )
  available <- intersect(indicators, names(simd_all))

  # Coerce character indicator columns to numeric
  simd_all <- coerce_simd_indicators(simd_all)
  simd_sf <- coerce_simd_indicators(simd_sf)

  # Compute national percentile for each indicator value
  # For deprivation indicators, higher value = worse
  tier_means <- simd_sf |>
    st_drop_geometry() |>
    group_by(across(all_of(group_var))) |>
    summarise(across(all_of(available), ~mean(., na.rm = TRUE)), .groups = "drop")

  results <- list()
  for (ind in available) {
    national_vals <- simd_all[[ind]]
    national_vals <- national_vals[!is.na(national_vals)]
    for (i in seq_len(nrow(tier_means))) {
      tier_val <- tier_means[[ind]][i]
      # Percentile: % of national datazones with a WORSE (higher) value
      # Higher percentile = tier is in a worse position
      pctile <- mean(national_vals <= tier_val, na.rm = TRUE) * 100
      results[[length(results) + 1]] <- tibble(
        !!group_var := tier_means[[group_var]][i],
        indicator = ind,
        value = tier_val,
        national_percentile = round(pctile, 1)
      )
    }
  }

  bind_rows(results)
}

#' Summarise SIMD access indicators by tier
#'
#' @param simd_sf sf object with SIMD data and focus_tier column
#' @param group_var Grouping variable
#' @return Tibble with mean access times by tier
calculate_access_summary <- function(simd_sf, group_var = "focus_tier") {
  access_cols <- c(
    "drive_petrol", "drive_GP", "drive_post",
    "drive_primary", "drive_retail", "drive_secondary",
    "PT_GP", "PT_post", "PT_retail", "Broadband"
  )
  available_cols <- intersect(access_cols, names(simd_sf))

  simd_sf |>
    st_drop_geometry() |>
    group_by(across(all_of(group_var))) |>
    summarise(
      across(all_of(available_cols), ~mean(., na.rm = TRUE)),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = -all_of(group_var),
      names_to = "indicator",
      values_to = "mean_value"
    )
}
