# fetch_population_data.R
# Fetch NRS mid-year population estimates from nrscotland.gov.uk
# Returns clean tibble: council_area_code, council_area_name, year, age, sex, population

library(httr)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# NRS time series Excel file URL
NRS_TIMESERIES_URL <- "https://www.nrscotland.gov.uk/media/lchj5a15/mid-year-population-estimates-time-series-data.xlsx"
NRS_RAW_PATH <- "data/raw/nrs_population_time_series.xlsx"

#' Download NRS population time series Excel file
#'
#' @param dest_path File path to save to
#' @return Invisible dest_path
download_nrs_data <- function(dest_path = NRS_RAW_PATH) {
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(dest_path)) {
    message("NRS Excel file already downloaded: ", dest_path)
    return(invisible(dest_path))
  }

  message("Downloading NRS time series data...")
  response <- GET(NRS_TIMESERIES_URL, write_disk(dest_path, overwrite = TRUE), timeout(120))

  if (status_code(response) != 200) {
    stop("Download failed with status ", status_code(response))
  }

  message("Downloaded to ", dest_path, " (", round(file.size(dest_path) / 1e6, 1), " MB)")
  invisible(dest_path)
}

#' Read and clean Table 1 from the NRS time series Excel file
#'
#' Table 1 contains: council area x sex x single year of age, 1981-2024
#'
#' @param excel_path Path to the downloaded Excel file
#' @param year Optional year to filter to. If NULL, returns all years.
#' @return Clean long-format tibble
read_nrs_council_data <- function(excel_path = NRS_RAW_PATH, year = NULL) {
  # Row 6 is the header row (skip first 5 rows)
  raw <- read_excel(excel_path, sheet = "Table 1", skip = 5)

  # Identify columns: first 4 are Area code, Area name, Sex, Year
  # Then "All Ages" followed by single ages 0..90+
  header_cols <- names(raw)[1:4]
  names(raw)[1:4] <- c("council_area_code", "council_area_name", "sex_raw", "year")

  # Drop "All Ages" column (column 5) — we can recalculate from single ages
  raw <- raw[, !(names(raw) %in% c("All Ages", "All ages"))]

  # The remaining columns are ages 0, 1, 2, ..., 90+
  age_cols <- names(raw)[5:ncol(raw)]

  # Pivot to long format
  long <- raw |>
    pivot_longer(
      cols = all_of(age_cols),
      names_to = "age_raw",
      values_to = "population"
    ) |>
    mutate(
      year = as.integer(year),
      age = clean_age_labels(age_raw),
      sex = clean_sex_labels(sex_raw),
      population = as.numeric(population)
    ) |>
    filter(
      !is.na(age),
      !is.na(sex),
      !is.na(population)
    ) |>
    select(council_area_code, council_area_name, year, age, sex, population) |>
    arrange(council_area_code, year, sex, age)

  if (!is.null(year)) {
    long <- filter(long, .data$year == .env$year)
  }

  long
}

#' Parse age labels from column headers to integer
#'
#' Handles: "0", "1", "90+", "90 and over", etc.
#'
#' @param age_string Character vector of age labels
#' @return Integer vector (90+ collapsed to 90)
clean_age_labels <- function(age_string) {
  age_string <- tolower(trimws(as.character(age_string)))
  age_string <- str_replace_all(age_string, "\\+| and over| years?", "")
  age_string <- trimws(age_string)
  age_int <- suppressWarnings(as.integer(age_string))
  pmin(age_int, 90L, na.rm = FALSE)
}

#' Standardise sex labels to lowercase
#'
#' @param sex_string Character vector
#' @return Character vector: "male", "female", or "all"
clean_sex_labels <- function(sex_string) {
  sex_string <- tolower(trimws(sex_string))
  case_when(
    sex_string %in% c("male", "males") ~ "male",
    sex_string %in% c("female", "females") ~ "female",
    sex_string %in% c("all", "persons", "total", "all persons") ~ "all",
    TRUE ~ NA_character_
  )
}

#' Get population data, using cache if available
#'
#' @param year Integer year or NULL for most recent available
#' @param use_cache Logical, whether to use cached CSV
#' @param cache_path Path to cache file
#' @return Clean tibble of population estimates
get_population_data <- function(year = NULL,
                                use_cache = TRUE,
                                cache_path = "data/processed/population_estimates.csv") {
  if (use_cache && file.exists(cache_path)) {
    message("Reading cached data from ", cache_path)
    df <- read_csv(cache_path, show_col_types = FALSE)

    if (!is.null(year)) {
      df <- filter(df, .data$year == .env$year)
    }
    return(df)
  }

  # Download Excel if needed, then read
  download_nrs_data()
  df <- read_nrs_council_data(year = year)

  # If no year specified, keep only the most recent
  if (is.null(year)) {
    max_year <- max(df$year, na.rm = TRUE)
    message("Using most recent year: ", max_year)
    df <- filter(df, year == max_year)
  }

  # Validate
  n_areas <- length(unique(df$council_area_code))
  message("Loaded data for ", n_areas, " areas")

  # Cache
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(df, cache_path)
  message("Cached data to ", cache_path)

  df
}
