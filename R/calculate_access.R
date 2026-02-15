# calculate_access.R
# Derived accessibility metrics from SIMD access indicators and OSM infrastructure

library(dplyr)
library(tidyr)
library(sf)
library(here)

#' Classify OSM point features by datazone tier
#'
#' Performs spatial join to assign each infrastructure point to the
#' datazone (and tier) it falls within.
#'
#' @param points_sf sf object with point features (e.g. bus stops)
#' @param simd_sf sf object with datazones and focus_tier column
#' @return sf object with focus_tier column added; points outside
#'   all datazones are dropped
classify_infrastructure_by_tier <- function(points_sf, simd_sf) {
  if (nrow(points_sf) == 0) return(points_sf)

  source(here("R", "fetch_simd_data.R"))
  dz_col <- find_datazone_column(simd_sf)

  # Ensure matching CRS
  if (st_crs(points_sf) != st_crs(simd_sf)) {
    points_sf <- st_transform(points_sf, st_crs(simd_sf))
  }

  # Spatial join: assign each point to the datazone it falls in
  joined <- st_join(points_sf, simd_sf[, c(dz_col, "focus_tier")],
                    join = st_within)

  # Drop points outside all datazones
  joined[!is.na(joined$focus_tier), ]
}

#' Count infrastructure features per datazone
#'
#' @param points_sf sf object with point features
#' @param simd_sf sf object with datazones, focus_tier, and Population
#' @param feature_name Character label for the feature type
#' @return Tibble with Data_Zone, focus_tier, count, population, and per-capita rate
count_infrastructure_by_dz <- function(points_sf, simd_sf, feature_name) {
  source(here("R", "fetch_simd_data.R"))
  dz_col <- find_datazone_column(simd_sf)

  classified <- classify_infrastructure_by_tier(points_sf, simd_sf)

  if (nrow(classified) == 0) {
    # Return empty tibble with expected columns
    return(tibble(
      Data_Zone = character(0), focus_tier = character(0),
      feature = character(0), count = integer(0),
      population = numeric(0), per_1000 = numeric(0)
    ))
  }

  counts <- classified |>
    st_drop_geometry() |>
    group_by(across(all_of(dz_col)), focus_tier) |>
    summarise(count = n(), .groups = "drop") |>
    rename(Data_Zone = all_of(dz_col))

  # Join population data
  pop <- simd_sf |>
    st_drop_geometry() |>
    select(Data_Zone = all_of(dz_col), Population)

  # Ensure all datazones appear (even those with 0 features)
  all_dzs <- simd_sf |>
    st_drop_geometry() |>
    select(Data_Zone = all_of(dz_col), focus_tier, Population)

  result <- all_dzs |>
    left_join(counts |> select(Data_Zone, count), by = "Data_Zone") |>
    mutate(
      count = replace_na(count, 0L),
      feature = feature_name,
      per_1000 = round(count / Population * 1000, 1)
    ) |>
    select(Data_Zone, focus_tier, feature, count, population = Population,
           per_1000)

  result
}

#' Summarise infrastructure density by tier
#'
#' For each infrastructure type, counts features per tier and computes
#' density metrics (features per 1000 population, features per km2).
#'
#' @param infra_list Named list of sf objects from get_osm_infrastructure()
#' @param simd_sf sf object with datazones, focus_tier, and Population
#' @return Tibble with feature type, tier, counts, and density metrics
summarise_infrastructure_by_tier <- function(infra_list, simd_sf) {
  source(here("R", "fetch_simd_data.R"))
  dz_col <- find_datazone_column(simd_sf)

  # Compute tier-level population and area
  tier_stats <- simd_sf |>
    mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
    st_drop_geometry() |>
    group_by(focus_tier) |>
    summarise(
      population = sum(Population, na.rm = TRUE),
      area_km2 = sum(area_km2),
      n_datazones = n(),
      .groups = "drop"
    )

  # Only process point features (not lines)
  point_features <- c("bus_stops", "car_parks", "crossings", "railway_stations")
  available <- intersect(point_features, names(infra_list))

  results <- lapply(available, function(feat_name) {
    feat_sf <- infra_list[[feat_name]]
    if (nrow(feat_sf) == 0) {
      return(tier_stats |>
               mutate(feature = feat_name, count = 0L,
                      per_1000_pop = 0, per_km2 = 0))
    }

    classified <- classify_infrastructure_by_tier(feat_sf, simd_sf)

    counts <- classified |>
      st_drop_geometry() |>
      group_by(focus_tier) |>
      summarise(count = n(), .groups = "drop")

    tier_stats |>
      left_join(counts, by = "focus_tier") |>
      mutate(
        feature = feat_name,
        count = replace_na(count, 0L),
        per_1000_pop = round(count / population * 1000, 1),
        per_km2 = round(count / area_km2, 1)
      )
  })

  bind_rows(results)
}

#' Calculate the public transport penalty (PT time minus drive time)
#'
#' For each service type where both drive and PT access indicators exist
#' in SIMD, computes the gap per datazone and summarises by tier.
#'
#' @param simd_sf sf object with SIMD access indicators and focus_tier
#' @return Tibble with service type, tier, mean/median gap
calculate_access_gap <- function(simd_sf) {
  # Service pairs: drive_X and PT_X
  service_pairs <- list(
    GP     = c(drive = "drive_GP",     pt = "PT_GP"),
    Post   = c(drive = "drive_post",   pt = "PT_post"),
    Retail = c(drive = "drive_retail", pt = "PT_retail")
  )

  available_pairs <- service_pairs[
    sapply(service_pairs, function(p)
      all(p %in% names(simd_sf)))
  ]

  if (length(available_pairs) == 0) {
    warning("No drive/PT pairs found in data")
    return(tibble())
  }

  results <- lapply(names(available_pairs), function(service) {
    pair <- available_pairs[[service]]

    simd_sf |>
      st_drop_geometry() |>
      mutate(
        pt_penalty = .data[[pair["pt"]]] - .data[[pair["drive"]]],
        service = service
      ) |>
      group_by(focus_tier, service) |>
      summarise(
        mean_drive = round(mean(.data[[pair["drive"]]], na.rm = TRUE), 1),
        mean_pt = round(mean(.data[[pair["pt"]]], na.rm = TRUE), 1),
        mean_gap = round(mean(pt_penalty, na.rm = TRUE), 1),
        median_gap = round(median(pt_penalty, na.rm = TRUE), 1),
        .groups = "drop"
      )
  })

  bind_rows(results)
}

#' Calculate distance from each datazone centroid to nearest infrastructure
#'
#' @param simd_sf sf object with datazones
#' @param points_sf sf object with infrastructure points
#' @param feature_name Character label for the feature type
#' @return Tibble with Data_Zone, focus_tier, feature, nearest_distance_m
calculate_nearest_distance <- function(simd_sf, points_sf, feature_name) {
  source(here("R", "fetch_simd_data.R"))
  dz_col <- find_datazone_column(simd_sf)

  if (nrow(points_sf) == 0) {
    return(tibble(
      Data_Zone = character(0), focus_tier = character(0),
      feature = character(0), nearest_distance_m = numeric(0)
    ))
  }

  # Ensure matching CRS
  if (st_crs(points_sf) != st_crs(simd_sf)) {
    points_sf <- st_transform(points_sf, st_crs(simd_sf))
  }

  # Compute centroids of datazones
  centroids <- st_centroid(simd_sf)

  # Distance matrix: each centroid to all points
  dist_matrix <- st_distance(centroids, points_sf)

  # Minimum distance per centroid
  min_dist <- apply(dist_matrix, 1, min)

  tibble(
    Data_Zone = simd_sf[[dz_col]],
    focus_tier = simd_sf$focus_tier,
    feature = feature_name,
    nearest_distance_m = round(as.numeric(min_dist))
  )
}

#' Compute line infrastructure length by tier
#'
#' For line features (cycle paths, footpaths), clips to datazone boundaries
#' and sums total length per tier.
#'
#' @param lines_sf sf object with linestring features
#' @param simd_sf sf object with datazones and focus_tier
#' @param feature_name Character label
#' @return Tibble with focus_tier, feature, total_length_km
calculate_line_length_by_tier <- function(lines_sf, simd_sf, feature_name) {
  if (nrow(lines_sf) == 0) {
    return(tibble(focus_tier = character(0), feature = character(0),
                  total_length_km = numeric(0)))
  }

  # Ensure matching CRS
  if (st_crs(lines_sf) != st_crs(simd_sf)) {
    lines_sf <- st_transform(lines_sf, st_crs(simd_sf))
  }

  # Intersect lines with datazones to clip
  clipped <- st_intersection(lines_sf, simd_sf[, "focus_tier"])

  if (nrow(clipped) == 0) {
    return(tibble(focus_tier = character(0), feature = character(0),
                  total_length_km = numeric(0)))
  }

  clipped |>
    mutate(length_m = as.numeric(st_length(geometry))) |>
    st_drop_geometry() |>
    group_by(focus_tier) |>
    summarise(
      total_length_km = round(sum(length_m) / 1000, 2),
      .groups = "drop"
    ) |>
    mutate(feature = feature_name)
}
