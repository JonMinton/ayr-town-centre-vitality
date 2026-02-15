# fetch_bres_data.R
# Fetch Business Register and Employment Survey (BRES) open access data
# from NOMIS API for South Ayrshire datazones

library(httr)
library(readr)
library(dplyr)
library(here)

# BRES open access dataset on NOMIS (NM_189_1)
# Employment by SIC 2007 section at 2011 Scottish datazone level
BRES_DATASET_ID <- "NM_189_1"
BRES_BASE_URL <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_189_1.data.csv"
BRES_CACHE_PATH <- here("data", "processed", "bres_south_ayrshire.csv")

# SIC 2007 section codes used in NOMIS (A through S)
# These are the broad industry classification
SIC_SECTIONS <- paste0("1509949", 45:63)

#' Download BRES data from NOMIS API for given datazones
#'
#' Queries the BRES open access dataset for employment counts by
#' SIC 2007 section at datazone level. Returns count values only
#' (not percentages). Data is rounded to nearest 5 for disclosure control.
#'
#' @param datazones Character vector of 2011 Scottish datazone codes
#' @param years Character vector of years to fetch, or "latest"
#' @return Tibble with columns: Data_Zone, DZname, sic_section, sic_label,
#'   employment, year
download_bres_data <- function(datazones, years = "latest") {
  message("Downloading BRES data from NOMIS for ", length(datazones), " datazones...")

  # NOMIS has a URL length limit; batch datazones in groups of 50
  batch_size <- 50
  batches <- split(datazones, ceiling(seq_along(datazones) / batch_size))
  all_results <- list()

  for (i in seq_along(batches)) {
    batch <- batches[[i]]
    message("  Batch ", i, "/", length(batches), " (", length(batch), " datazones)...")

    dz_param <- paste(batch, collapse = ",")
    date_param <- if (identical(years, "latest")) "latest" else paste(years, collapse = ",")

    url <- paste0(
      BRES_BASE_URL,
      "?geography=", dz_param,
      "&industry=", paste(SIC_SECTIONS, collapse = ","),
      "&employment_status=4",     # Employment (total)
      "&measures=20100",          # Value
      "&date=", date_param,
      "&select=geography_code,geography_name,industry_code,industry_name,obs_value,date_name"
    )

    response <- GET(url, timeout(120))

    if (status_code(response) != 200) {
      warning("NOMIS API returned status ", status_code(response), " for batch ", i)
      next
    }

    content <- content(response, as = "text", encoding = "UTF-8")
    batch_df <- read_csv(content, show_col_types = FALSE)

    if (nrow(batch_df) > 0) {
      all_results[[i]] <- batch_df
    }

    # Be polite to the API
    if (i < length(batches)) Sys.sleep(1)
  }

  if (length(all_results) == 0) {
    stop("No BRES data returned from NOMIS. Check API availability.")
  }

  result <- bind_rows(all_results)

  # The API returns pairs of rows: count (integer) and percentage (decimal)
  # For each geography + industry + year combination, keep only the count row
  # Count rows have values rounded to nearest 5 (0, 5, 10, 15, ...)
  # Percentage rows have decimal values
  result <- result |>
    group_by(GEOGRAPHY_CODE, INDUSTRY_CODE, DATE_NAME) |>
    slice(1) |>
    ungroup()

  # Clean column names
  result <- result |>
    transmute(
      Data_Zone = GEOGRAPHY_CODE,
      DZname = GEOGRAPHY_NAME,
      sic_section = INDUSTRY_CODE,
      sic_label = INDUSTRY_NAME,
      employment = as.numeric(OBS_VALUE),
      year = as.integer(DATE_NAME)
    )

  message("Downloaded BRES data: ", nrow(result), " rows, ",
          length(unique(result$Data_Zone)), " datazones, ",
          length(unique(result$year)), " year(s)")

  result
}

#' Get BRES data for South Ayrshire, downloading if needed
#'
#' @param use_cache Logical, whether to use cached CSV
#' @param years Character vector of years, or "latest" (default)
#' @return Tibble with BRES employment data
get_bres_data <- function(use_cache = TRUE, years = "latest") {
  if (use_cache && file.exists(BRES_CACHE_PATH)) {
    message("Reading cached BRES data from ", BRES_CACHE_PATH)
    return(read_csv(BRES_CACHE_PATH, show_col_types = FALSE))
  }

  # Get South Ayrshire datazone codes from SIMD data
  source(here("R", "fetch_simd_data.R"))
  simd <- get_simd_data()
  sa_datazones <- simd$Data_Zone[simd$Council_area == "South Ayrshire"]

  bres <- download_bres_data(sa_datazones, years = years)

  # Cache
  dir.create(dirname(BRES_CACHE_PATH), recursive = TRUE, showWarnings = FALSE)
  write_csv(bres, BRES_CACHE_PATH)
  message("Cached BRES data to ", BRES_CACHE_PATH)

  bres
}

#' Map SIC 2007 sections to policy-relevant sector groups
#'
#' @param sic_section Character vector of SIC section codes (A, B, C, ...)
#' @return Character vector of sector group labels
classify_sector <- function(sic_section) {
  dplyr::case_when(
    sic_section %in% c("G") ~ "Retail & wholesale",
    sic_section %in% c("I") ~ "Hospitality",
    sic_section %in% c("Q") ~ "Health & social care",
    sic_section %in% c("P") ~ "Education",
    sic_section %in% c("R", "S") ~ "Arts, recreation & other services",
    sic_section %in% c("O") ~ "Public administration",
    sic_section %in% c("J", "K", "L", "M", "N") ~ "Professional & business services",
    sic_section %in% c("C", "F") ~ "Construction & manufacturing",
    sic_section %in% c("H") ~ "Transport & storage",
    TRUE ~ "Other (agriculture, energy, water, mining)"
  )
}
