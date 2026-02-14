# calculate_simd.R
# Prepare SIMD data for analysis: join to boundaries, define focus areas, compute spatial statistics

library(dplyr)
library(tidyr)
library(sf)
library(spdep)
library(here)

#' Join SIMD data to datazone boundary polygons
#'
#' @param simd_df SIMD tibble from get_simd_data()
#' @param boundaries_sf Boundary sf object from get_dz_boundaries()
#' @param council_area Council area name to filter to (default: "South Ayrshire")
#' @return sf object with SIMD attributes and geometry
prepare_simd_spatial <- function(simd_df, boundaries_sf,
                                  council_area = "South Ayrshire") {
  # Filter SIMD to council area
  simd_ca <- simd_df |>
    filter(Council_area == council_area)

  # Find the datazone code column in the shapefile
  dz_col <- find_datazone_column(boundaries_sf)

  # Join SIMD data to boundaries
  result <- boundaries_sf |>
    inner_join(simd_ca, by = setNames("Data_Zone", dz_col))

  # Validate join
  n_simd <- nrow(simd_ca)
  n_joined <- nrow(result)
  if (n_joined < n_simd * 0.9) {
    warning("Only ", n_joined, " of ", n_simd, " datazones matched boundaries. ",
            "Check datazone code column names.")
  }
  message("Joined ", n_joined, " datazones with geometry for ", council_area)

  result
}

#' Classify datazones into focus tiers
#'
#' Adds a `focus_tier` column based on explicit datazone code vectors.
#'
#' @param simd_sf sf object from prepare_simd_spatial()
#' @param town_centre_dzs Character vector of datazone codes for "Town Centre".
#'   If NULL, no Town Centre tier is assigned.
#' @param wider_ayr_dzs Character vector of datazone codes for "Wider Ayr".
#'   If NULL, no Wider Ayr tier is assigned.
#' @return sf object with added `focus_tier` column
classify_focus_area <- function(simd_sf,
                                 town_centre_dzs = NULL,
                                 wider_ayr_dzs = NULL) {
  dz_col <- find_datazone_column(simd_sf)
  dz_codes <- simd_sf[[dz_col]]

  simd_sf$focus_tier <- case_when(
    !is.null(town_centre_dzs) & dz_codes %in% town_centre_dzs ~ "Town Centre",
    !is.null(wider_ayr_dzs) & dz_codes %in% wider_ayr_dzs ~ "Wider Ayr",
    TRUE ~ "Rest of South Ayrshire"
  )

  simd_sf$focus_tier <- factor(
    simd_sf$focus_tier,
    levels = c("Town Centre", "Wider Ayr", "Rest of South Ayrshire")
  )

  tier_counts <- table(simd_sf$focus_tier)
  message("Focus tiers: ", paste(names(tier_counts), tier_counts, sep = "=", collapse = ", "))

  simd_sf
}

#' Summarise SIMD domain ranks by grouping variable
#'
#' @param simd_sf sf object with SIMD data and a grouping column
#' @param group_var Column name to group by (default: "focus_tier")
#' @return Long-format tibble with mean/median ranks per domain per group
calculate_domain_summary <- function(simd_sf, group_var = "focus_tier") {
  domain_cols <- c(
    "SIMD2020v2_Income_Domain_Rank",
    "SIMD2020_Employment_Domain_Rank",
    "SIMD2020_Health_Domain_Rank",
    "SIMD2020_Education_Domain_Rank",
    "SIMD2020_Access_Domain_Rank",
    "SIMD2020_Crime_Domain_Rank",
    "SIMD2020_Housing_Domain_Rank"
  )

  # Use only columns that exist in the data
  available_cols <- intersect(domain_cols, names(simd_sf))

  simd_sf |>
    st_drop_geometry() |>
    group_by(across(all_of(group_var))) |>
    summarise(
      across(all_of(available_cols),
             list(mean = ~mean(., na.rm = TRUE),
                  median = ~median(., na.rm = TRUE))),
      n_datazones = n(),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = -c(all_of(group_var), n_datazones),
      names_to = c("domain", "stat"),
      names_pattern = "(.+)_(mean|median)$",
      values_to = "value"
    )
}

#' Build spatial weights matrix from datazone polygons
#'
#' Uses queen contiguity (shared boundary or vertex).
#'
#' @param simd_sf sf object with datazone polygons
#' @param style Weight style: "W" (row-standardised) or "B" (binary)
#' @return listw object for use with spdep functions
build_spatial_weights <- function(simd_sf, style = "W") {
  nb <- poly2nb(simd_sf, queen = TRUE)

  # Report any zero-neighbour datazones
  n_islands <- sum(card(nb) == 0)
  if (n_islands > 0) {
    warning(n_islands, " datazone(s) with no contiguous neighbours (islands). ",
            "These will be handled with zero.policy = TRUE.")
  }

  message("Neighbour summary: min=", min(card(nb)), ", max=", max(card(nb)),
          ", mean=", round(mean(card(nb)), 1))

  nb2listw(nb, style = style, zero.policy = TRUE)
}

#' Run global Moran's I test
#'
#' @param simd_sf sf object with the variable to test
#' @param variable Character name of the column to test
#' @param weights listw object from build_spatial_weights()
#' @return Tibble with Moran's I statistic, expected value, variance, z-score, p-value
run_global_morans_i <- function(simd_sf, variable, weights) {
  x <- simd_sf[[variable]]

  if (all(is.na(x))) {
    warning("All values are NA for variable: ", variable)
    return(NULL)
  }

  result <- moran.test(x, weights, zero.policy = TRUE, na.action = na.omit)

  tibble(
    variable = variable,
    morans_i = result$estimate["Moran I statistic"],
    expected = result$estimate["Expectation"],
    variance = result$estimate["Variance"],
    z_score = result$statistic,
    p_value = result$p.value
  )
}

#' Run local Moran's I (LISA) and classify clusters
#'
#' @param simd_sf sf object with the variable to test
#' @param variable Character name of the column to test
#' @param weights listw object from build_spatial_weights()
#' @param p_threshold Significance threshold for cluster classification
#' @return sf object augmented with LISA statistics and cluster classification
run_local_morans_i <- function(simd_sf, variable, weights, p_threshold = 0.05) {
  x <- simd_sf[[variable]]

  lisa <- localmoran(x, weights, zero.policy = TRUE, na.action = na.omit)

  # Add LISA results to the sf object
  simd_sf$lisa_ii <- lisa[, "Ii"]
  simd_sf$lisa_z <- lisa[, "Z.Ii"]
  simd_sf$lisa_p <- lisa[, "Pr(z != E(Ii))"]

  # Classify into cluster types
  x_mean <- mean(x, na.rm = TRUE)
  lagged <- lag.listw(weights, x, zero.policy = TRUE)
  lagged_mean <- mean(lagged, na.rm = TRUE)

  simd_sf$lisa_cluster <- case_when(
    simd_sf$lisa_p > p_threshold ~ "Not Significant",
    x > x_mean & lagged > lagged_mean ~ "High-High",
    x < x_mean & lagged < lagged_mean ~ "Low-Low",
    x > x_mean & lagged < lagged_mean ~ "High-Low",
    x < x_mean & lagged > lagged_mean ~ "Low-High",
    TRUE ~ "Not Significant"
  )

  simd_sf$lisa_cluster <- factor(
    simd_sf$lisa_cluster,
    levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")
  )

  message("LISA clusters for ", variable, ": ",
          paste(names(table(simd_sf$lisa_cluster)),
                table(simd_sf$lisa_cluster), sep = "=", collapse = ", "))

  simd_sf
}
