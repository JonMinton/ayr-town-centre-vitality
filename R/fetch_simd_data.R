# fetch_simd_data.R
# Fetch SIMD 2020v2 data and 2011 datazone boundary shapefile
# Returns clean tibble and sf object

library(httr)
library(readxl)
library(readr)
library(dplyr)
library(sf)
library(here)

# SIMD 2020v2 Excel files from gov.scot (full dataset with all domains)
SIMD_RANKS_URL <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/documents/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/govscot%3Adocument/SIMD%2B2020v2%2B-%2Branks.xlsx"
SIMD_INDICATORS_URL <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-indicator-data/documents/simd_2020_indicators/simd_2020_indicators/govscot%3Adocument/SIMD%2B2020v2%2B-%2Bindicators.xlsx"
SIMD_LOOKUP_URL <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-data-zone-look-up-file/documents/scottish-index-of-multiple-deprivation-data-zone-look-up/scottish-index-of-multiple-deprivation-data-zone-look-up/govscot%3Adocument/SIMD%2B2020v2%2B-%2Bdatazone%2Blookup%2B-%2Bupdated%2B2025.xlsx"

SIMD_RANKS_RAW <- here("data", "raw", "simd2020v2_ranks.xlsx")
SIMD_INDICATORS_RAW <- here("data", "raw", "simd2020v2_indicators.xlsx")
SIMD_LOOKUP_RAW <- here("data", "raw", "simd2020v2_lookup.xlsx")
SIMD_CACHE_PATH <- here("data", "processed", "simd2020v2.csv")

# 2011 Datazone boundary shapefile from gov.scot
DZ_BOUNDARIES_URL <- "https://maps.gov.scot/ATOM/shapefiles/SG_DataZoneBdry_2011.zip"
DZ_RAW_ZIP_PATH <- here("data", "raw", "SG_DataZoneBdry_2011.zip")
DZ_SHAPEFILE_DIR <- here("data", "raw", "SG_DataZoneBdry_2011")

#' Download a file if not already cached
#'
#' @param url URL to download from
#' @param dest_path Local file path to save to
#' @param description Human-readable label for messages
#' @return Invisible dest_path
download_if_needed <- function(url, dest_path, description = "file") {
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(dest_path)) {
    message(description, " already downloaded: ", dest_path)
    return(invisible(dest_path))
  }

  message("Downloading ", description, "...")
  response <- GET(url, write_disk(dest_path, overwrite = TRUE), timeout(120))

  if (status_code(response) != 200) {
    file.remove(dest_path)
    stop("Download failed with status ", status_code(response))
  }

  message("Downloaded to ", dest_path, " (", round(file.size(dest_path) / 1e6, 1), " MB)")
  invisible(dest_path)
}

#' Download and merge SIMD 2020v2 data from gov.scot
#'
#' Downloads three Excel files (ranks, indicators, lookup), reads and joins them
#' into a single tibble cached as CSV.
#'
#' @return Invisible cache path
download_simd_data <- function() {
  download_if_needed(SIMD_RANKS_URL, SIMD_RANKS_RAW, "SIMD 2020v2 ranks")
  download_if_needed(SIMD_LOOKUP_URL, SIMD_LOOKUP_RAW, "SIMD 2020v2 lookup")
  download_if_needed(SIMD_INDICATORS_URL, SIMD_INDICATORS_RAW, "SIMD 2020v2 indicators")

  message("Reading and merging SIMD data files...")

  # Read ranks (sheet 2 usually has the data, sheet 1 is metadata)
  ranks_sheets <- excel_sheets(SIMD_RANKS_RAW)
  ranks <- read_excel(SIMD_RANKS_RAW, sheet = ranks_sheets[length(ranks_sheets)])

  # Read lookup
  lookup_sheets <- excel_sheets(SIMD_LOOKUP_RAW)
  lookup <- read_excel(SIMD_LOOKUP_RAW, sheet = lookup_sheets[length(lookup_sheets)])

  # Read indicators
  indicators_sheets <- excel_sheets(SIMD_INDICATORS_RAW)
  indicators <- read_excel(SIMD_INDICATORS_RAW, sheet = indicators_sheets[length(indicators_sheets)])

  # Find the datazone column in each (likely "Data_Zone" or "DZ")
  dz_ranks <- find_dz_column_in_df(ranks)
  dz_lookup <- find_dz_column_in_df(lookup)
  dz_indicators <- find_dz_column_in_df(indicators)

  # Standardise the join key name
  ranks <- rename(ranks, Data_Zone = !!sym(dz_ranks))
  lookup <- rename(lookup, Data_Zone = !!sym(dz_lookup))
  indicators <- rename(indicators, Data_Zone = !!sym(dz_indicators))

  # Join: lookup + ranks + indicators
  merged <- lookup |>
    left_join(ranks, by = "Data_Zone", suffix = c("", ".ranks")) |>
    left_join(indicators, by = "Data_Zone", suffix = c("", ".ind"))

  # Remove duplicate columns from joins
  dup_cols <- grep("\\.(ranks|ind)$", names(merged), value = TRUE)
  if (length(dup_cols) > 0) {
    merged <- select(merged, -all_of(dup_cols))
  }

  message("Merged SIMD data: ", nrow(merged), " datazones, ", ncol(merged), " columns")

  # Cache as CSV
  dir.create(dirname(SIMD_CACHE_PATH), recursive = TRUE, showWarnings = FALSE)
  write_csv(merged, SIMD_CACHE_PATH)
  message("Cached merged data to ", SIMD_CACHE_PATH)

  invisible(SIMD_CACHE_PATH)
}

#' Get SIMD 2020v2 data, downloading and merging if needed
#'
#' @param use_cache Logical, whether to use cached CSV
#' @return Tibble with SIMD data (6,976 rows, one per datazone)
get_simd_data <- function(use_cache = TRUE) {
  if (use_cache && file.exists(SIMD_CACHE_PATH)) {
    message("Reading cached SIMD data from ", SIMD_CACHE_PATH)
    return(read_csv(SIMD_CACHE_PATH, show_col_types = FALSE))
  }

  download_simd_data()
  read_csv(SIMD_CACHE_PATH, show_col_types = FALSE)
}

#' Download 2011 datazone boundary shapefile
#'
#' @param dest_zip Path to save the ZIP file
#' @param dest_dir Directory to extract shapefile into
#' @return Invisible dest_dir
download_dz_boundaries <- function(dest_zip = DZ_RAW_ZIP_PATH,
                                   dest_dir = DZ_SHAPEFILE_DIR) {
  dir.create(dirname(dest_zip), recursive = TRUE, showWarnings = FALSE)

  if (dir.exists(dest_dir) && length(list.files(dest_dir, pattern = "\\.shp$", recursive = TRUE)) > 0) {
    message("Datazone shapefile already extracted: ", dest_dir)
    return(invisible(dest_dir))
  }

  if (!file.exists(dest_zip)) {
    message("Downloading 2011 datazone boundary shapefile (~20 MB)...")
    response <- GET(DZ_BOUNDARIES_URL, write_disk(dest_zip, overwrite = TRUE), timeout(300))

    if (status_code(response) != 200) {
      stop("Download failed with status ", status_code(response))
    }
    message("Downloaded to ", dest_zip, " (", round(file.size(dest_zip) / 1e6, 1), " MB)")
  }

  message("Extracting shapefile...")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  unzip(dest_zip, exdir = dest_dir)
  message("Extracted to ", dest_dir)

  invisible(dest_dir)
}

#' Get 2011 datazone boundaries as sf object
#'
#' @param council_area Optional council area name to filter to (e.g. "South Ayrshire").
#'   Requires SIMD data to identify which datazones belong to the council area.
#' @param simd_df Optional SIMD tibble (from get_simd_data()) for council area filtering.
#'   If council_area is specified but simd_df is NULL, SIMD data will be loaded automatically.
#' @param dest_dir Directory containing the extracted shapefile
#' @return sf object with datazone boundaries
get_dz_boundaries <- function(council_area = NULL,
                              simd_df = NULL,
                              dest_dir = DZ_SHAPEFILE_DIR) {
  download_dz_boundaries(dest_dir = dest_dir)

  # Find the .shp file in the extracted directory
  shp_files <- list.files(dest_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(shp_files) == 0) {
    stop("No .shp file found in ", dest_dir)
  }
  shp_path <- shp_files[1]

  message("Reading shapefile: ", shp_path)
  boundaries <- st_read(shp_path, quiet = TRUE)
  message("Loaded ", nrow(boundaries), " datazone boundaries")

  # Filter by council area if requested
  if (!is.null(council_area)) {
    if (is.null(simd_df)) {
      simd_df <- get_simd_data()
    }

    # Get datazone codes for the specified council area
    ca_col <- find_council_area_column(simd_df)
    ca_datazones <- simd_df |>
      filter(.data[[ca_col]] == council_area) |>
      pull(Data_Zone)

    # Find the datazone code column in the shapefile
    dz_col <- find_datazone_column(boundaries)
    boundaries <- boundaries[boundaries[[dz_col]] %in% ca_datazones, ]
    message("Filtered to ", nrow(boundaries), " datazones in ", council_area)
  }

  boundaries
}

# ---- Utility helpers ----

#' Find the datazone code column in an sf object
#'
#' @param sf_obj An sf object
#' @return Character string: the column name containing datazone codes
find_datazone_column <- function(sf_obj) {
  candidates <- c("DataZone", "datazone", "DZ_CODE", "DZ_Code", "DZ_code",
                   "data_zone", "Data_Zone", "DATAZONE", "DataZone2011", "DZ2011")
  cols <- names(sf_obj)
  match <- intersect(candidates, cols)

  if (length(match) > 0) {
    return(match[1])
  }

  # Fallback: look for a column whose values look like datazone codes (S01...)
  for (col in cols) {
    vals <- as.character(sf_obj[[col]])
    if (any(grepl("^S01[0-9]{6}$", vals, perl = TRUE))) {
      return(col)
    }
  }

  stop("Could not identify datazone code column. Available columns: ",
       paste(cols, collapse = ", "))
}

#' Find the datazone column in a plain data frame
#'
#' @param df A data frame or tibble
#' @return Column name
find_dz_column_in_df <- function(df) {
  candidates <- c("Data_Zone", "DataZone", "DZ", "DZ_CODE", "datazone", "data_zone")
  cols <- names(df)
  match <- intersect(candidates, cols)
  if (length(match) > 0) return(match[1])

  # Fallback: first column with S01 codes
  for (col in cols) {
    vals <- as.character(df[[col]])
    if (any(grepl("^S01[0-9]{6}$", vals, perl = TRUE))) {
      return(col)
    }
  }
  stop("No datazone column found. Columns: ", paste(cols, collapse = ", "))
}

#' Find the council area column in SIMD data
#'
#' @param df SIMD tibble
#' @return Column name
find_council_area_column <- function(df) {
  candidates <- c("Council_area", "Council area", "council_area", "CA_Name",
                   "CAName", "Local_Authority")
  cols <- names(df)
  match <- intersect(candidates, cols)
  if (length(match) > 0) return(match[1])
  stop("No council area column found. Columns: ", paste(cols, collapse = ", "))
}
