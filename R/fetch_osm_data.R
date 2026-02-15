# fetch_osm_data.R
# Fetch OpenStreetMap infrastructure data via the Overpass API
# Uses direct HTTP calls (httr + jsonlite) since the osmdata package
# cannot compile on the project's R 4.1.1 anaconda toolchain.

library(httr)
library(jsonlite)
library(sf)
library(dplyr)
library(here)

# Primary and fallback Overpass API endpoints
OVERPASS_URLS <- c(
  "https://overpass.kumi.systems/api/interpreter",
  "https://overpass-api.de/api/interpreter"
)

#' Compute a bounding box for the Ayr study area
#'
#' Uses the datazone boundaries to compute a bbox expanded by a buffer,
#' returned in WGS84 (lat/lon) as required by the Overpass API.
#'
#' @param simd_sf sf object with South Ayrshire datazones
#' @param buffer_m Buffer distance in metres to expand bbox
#' @return Named numeric vector (south, west, north, east) in WGS84
get_ayr_bbox <- function(simd_sf, buffer_m = 500) {
  # Filter to Town Centre + Wider Ayr tiers if focus_tier exists
  if ("focus_tier" %in% names(simd_sf)) {
    study_area <- simd_sf[simd_sf$focus_tier %in% c("Town Centre", "Wider Ayr"), ]
  } else {
    study_area <- simd_sf
  }

  # Buffer in the native CRS (should be BNG / metres)
  buffered <- st_buffer(st_union(study_area), buffer_m)

  # Transform to WGS84 and get bbox
  bbox_wgs <- st_bbox(st_transform(buffered, 4326))

  c(south = as.numeric(bbox_wgs["ymin"]),
    west  = as.numeric(bbox_wgs["xmin"]),
    north = as.numeric(bbox_wgs["ymax"]),
    east  = as.numeric(bbox_wgs["xmax"]))
}

#' Query the Overpass API for OSM features
#'
#' Sends an Overpass QL query and returns the JSON response parsed.
#'
#' @param query Character string of Overpass QL
#' @param timeout_secs API timeout in seconds
#' @param max_retries Number of retry attempts per endpoint
#' @return Parsed JSON list
query_overpass <- function(query, timeout_secs = 120, max_retries = 2) {
  for (url in OVERPASS_URLS) {
    for (attempt in seq_len(max_retries)) {
      response <- tryCatch(
        POST(
          url,
          body = list(data = query),
          encode = "form",
          timeout(timeout_secs)
        ),
        error = function(e) {
          message("  ", url, " attempt ", attempt, " failed: ",
                  conditionMessage(e))
          NULL
        }
      )

      if (!is.null(response) && status_code(response) == 200) {
        return(fromJSON(content(response, as = "text", encoding = "UTF-8"),
                        flatten = TRUE))
      }

      if (!is.null(response)) {
        message("  ", url, " attempt ", attempt, ": status ",
                status_code(response))
      }

      if (attempt < max_retries) {
        Sys.sleep(5)
      }
    }
    message("  Trying next endpoint...")
  }

  stop("Overpass API failed on all endpoints after retries")
}

#' Convert Overpass JSON point elements to an sf object
#'
#' @param elements Data frame of Overpass elements (nodes with lat/lon)
#' @param crs Target CRS (default: BNG EPSG:27700)
#' @return sf object with point geometries, or NULL if no data
overpass_nodes_to_sf <- function(elements, crs = 27700) {
  if (is.null(elements) || nrow(elements) == 0) return(NULL)

  # Filter to nodes with coordinates
  nodes <- elements[elements$type == "node" & !is.na(elements$lat), ]
  if (nrow(nodes) == 0) return(NULL)

  # Extract tag columns (prefixed with "tags.")
  tag_cols <- grep("^tags\\.", names(nodes), value = TRUE)
  tag_data <- nodes[, tag_cols, drop = FALSE]
  names(tag_data) <- gsub("^tags\\.", "", names(tag_data))

  # Build data frame with coordinates included for st_as_sf
  df <- cbind(
    data.frame(osm_id = nodes$id, lon = nodes$lon, lat = nodes$lat),
    tag_data
  )

  result <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  st_transform(result, crs)
}

#' Convert Overpass JSON way elements to an sf linestring object
#'
#' Ways require a second query to resolve node coordinates.
#' We use `out geom;` in the Overpass query to get geometry inline.
#'
#' @param elements Data frame of Overpass elements (ways with geometry)
#' @param crs Target CRS
#' @return sf object with linestring geometries, or NULL if no data
overpass_ways_to_sf <- function(elements, crs = 27700) {
  if (is.null(elements) || nrow(elements) == 0) return(NULL)

  ways <- elements[elements$type == "way", ]
  if (nrow(ways) == 0) return(NULL)

  # Each way has a geometry column (list of lat/lon pairs from `out geom;`)
  geom_list <- lapply(seq_len(nrow(ways)), function(i) {
    coords <- ways$geometry[[i]]
    if (is.null(coords) || nrow(coords) < 2) return(NULL)
    st_linestring(as.matrix(coords[, c("lon", "lat")]))
  })

  # Remove NULL entries
  valid <- !sapply(geom_list, is.null)
  if (!any(valid)) return(NULL)

  ways <- ways[valid, ]
  geom_list <- geom_list[valid]

  # Extract tags
  tag_cols <- grep("^tags\\.", names(ways), value = TRUE)
  tag_data <- ways[, tag_cols, drop = FALSE]
  names(tag_data) <- gsub("^tags\\.", "", names(tag_data))

  sfc <- st_sfc(geom_list, crs = 4326)
  result <- st_sf(
    data.frame(osm_id = ways$id, tag_data, stringsAsFactors = FALSE),
    geometry = sfc
  )

  st_transform(result, crs)
}

#' Fetch OSM point features within a bounding box
#'
#' @param bbox Named vector from get_ayr_bbox() (south, west, north, east)
#' @param key OSM key (e.g. "highway", "amenity")
#' @param value OSM value (e.g. "bus_stop", "parking")
#' @param cache_name Short name for caching (e.g. "bus_stops")
#' @return sf object with point geometries
fetch_osm_points <- function(bbox, key, value, cache_name) {
  cache_path <- here("data", "processed", paste0("osm_", cache_name, ".rds"))

  if (file.exists(cache_path)) {
    message("Reading cached OSM ", cache_name, " from ", cache_path)
    return(readRDS(cache_path))
  }

  message("Fetching OSM ", cache_name, " from Overpass API...")

  bbox_str <- paste0(bbox["south"], ",", bbox["west"], ",",
                     bbox["north"], ",", bbox["east"])

  query <- paste0(
    '[out:json][timeout:60];\n',
    'node["', key, '"="', value, '"](', bbox_str, ');\n',
    'out body;'
  )

  result <- query_overpass(query)
  sf_data <- overpass_nodes_to_sf(result$elements)

  if (is.null(sf_data)) {
    message("  No ", cache_name, " found in area")
    sf_data <- st_sf(
      osm_id = integer(0),
      geometry = st_sfc(crs = 27700)
    )
  } else {
    message("  Found ", nrow(sf_data), " ", cache_name)
  }

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sf_data, cache_path)
  sf_data
}

#' Convert Overpass JSON way elements to polygon centroids (sf points)
#'
#' For features like car parks that are mapped as areas, this extracts
#' the centroid of each closed way as a point feature.
#'
#' @param elements Data frame of Overpass elements (ways with geometry)
#' @param crs Target CRS
#' @return sf object with point geometries (centroids), or NULL if no data
overpass_ways_to_centroids <- function(elements, crs = 27700) {
  if (is.null(elements) || nrow(elements) == 0) return(NULL)

  ways <- elements[elements$type == "way", ]
  if (nrow(ways) == 0) return(NULL)

  geom_list <- lapply(seq_len(nrow(ways)), function(i) {
    coords <- ways$geometry[[i]]
    if (is.null(coords) || nrow(coords) < 3) return(NULL)
    mat <- as.matrix(coords[, c("lon", "lat")])
    # Close the ring if not already closed
    if (!identical(mat[1, ], mat[nrow(mat), ])) {
      mat <- rbind(mat, mat[1, ])
    }
    st_polygon(list(mat))
  })

  valid <- !sapply(geom_list, is.null)
  if (!any(valid)) return(NULL)

  ways <- ways[valid, ]
  geom_list <- geom_list[valid]

  tag_cols <- grep("^tags\\.", names(ways), value = TRUE)
  tag_data <- ways[, tag_cols, drop = FALSE]
  names(tag_data) <- gsub("^tags\\.", "", names(tag_data))

  sfc <- st_sfc(geom_list, crs = 4326)
  polys <- st_sf(
    data.frame(osm_id = ways$id, tag_data, stringsAsFactors = FALSE),
    geometry = sfc
  )

  # Convert to centroids and transform
  centroids <- st_centroid(polys)
  st_transform(centroids, crs)
}

#' Fetch OSM polygon features as centroids within a bounding box
#'
#' Queries for ways (used for area features like car parks) and returns
#' their centroids as point features.
#'
#' @param bbox Named vector from get_ayr_bbox()
#' @param key OSM key
#' @param value OSM value
#' @param cache_name Short name for caching
#' @return sf object with point geometries (centroids)
fetch_osm_polygon_centroids <- function(bbox, key, value, cache_name) {
  cache_path <- here("data", "processed", paste0("osm_", cache_name, ".rds"))

  if (file.exists(cache_path)) {
    message("Reading cached OSM ", cache_name, " from ", cache_path)
    return(readRDS(cache_path))
  }

  message("Fetching OSM ", cache_name, " (polygons) from Overpass API...")

  bbox_str <- paste0(bbox["south"], ",", bbox["west"], ",",
                     bbox["north"], ",", bbox["east"])

  query <- paste0(
    '[out:json][timeout:60];\n',
    'way["', key, '"="', value, '"](', bbox_str, ');\n',
    'out geom;'
  )

  result <- query_overpass(query)
  sf_data <- overpass_ways_to_centroids(result$elements)

  if (is.null(sf_data)) {
    message("  No ", cache_name, " polygons found in area")
    sf_data <- st_sf(
      osm_id = integer(0),
      geometry = st_sfc(crs = 27700)
    )
  } else {
    message("  Found ", nrow(sf_data), " ", cache_name, " polygons")
  }

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sf_data, cache_path)
  sf_data
}

#' Fetch OSM line features (ways) within a bounding box
#'
#' @param bbox Named vector from get_ayr_bbox()
#' @param key OSM key
#' @param value OSM value
#' @param cache_name Short name for caching
#' @return sf object with linestring geometries
fetch_osm_lines <- function(bbox, key, value, cache_name) {
  cache_path <- here("data", "processed", paste0("osm_", cache_name, ".rds"))

  if (file.exists(cache_path)) {
    message("Reading cached OSM ", cache_name, " from ", cache_path)
    return(readRDS(cache_path))
  }

  message("Fetching OSM ", cache_name, " from Overpass API...")

  bbox_str <- paste0(bbox["south"], ",", bbox["west"], ",",
                     bbox["north"], ",", bbox["east"])

  query <- paste0(
    '[out:json][timeout:60];\n',
    'way["', key, '"="', value, '"](', bbox_str, ');\n',
    'out geom;'
  )

  result <- query_overpass(query)
  sf_data <- overpass_ways_to_sf(result$elements)

  if (is.null(sf_data)) {
    message("  No ", cache_name, " found in area")
    sf_data <- st_sf(
      osm_id = integer(0),
      geometry = st_sfc(crs = 27700)
    )
  } else {
    message("  Found ", nrow(sf_data), " ", cache_name)
  }

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sf_data, cache_path)
  sf_data
}

#' Fetch all accessibility infrastructure for the Ayr study area
#'
#' Downloads and caches OSM features: bus stops, car parks, pedestrian
#' crossings, cycle paths, footpaths, and railway stations.
#'
#' @param simd_sf sf object with South Ayrshire datazones and focus_tier
#' @return Named list of sf objects
get_osm_infrastructure <- function(simd_sf) {
  bbox <- get_ayr_bbox(simd_sf)
  message("Bounding box: S=", round(bbox["south"], 4),
          " W=", round(bbox["west"], 4),
          " N=", round(bbox["north"], 4),
          " E=", round(bbox["east"], 4))

  infra <- list()

  # Point features
  infra$bus_stops <- fetch_osm_points(bbox, "highway", "bus_stop", "bus_stops")
  Sys.sleep(1)  # Be polite to the API

  # Car parks: combine point nodes and polygon centroids
  car_park_nodes <- fetch_osm_points(bbox, "amenity", "parking", "car_parks_nodes")
  Sys.sleep(1)
  car_park_polys <- fetch_osm_polygon_centroids(bbox, "amenity", "parking",
                                                  "car_parks_polys")
  # Merge into a single sf of car park points
  common_cols <- intersect(names(car_park_nodes), names(car_park_polys))
  if (nrow(car_park_nodes) > 0 && nrow(car_park_polys) > 0) {
    infra$car_parks <- rbind(car_park_nodes[, common_cols],
                              car_park_polys[, common_cols])
  } else if (nrow(car_park_polys) > 0) {
    infra$car_parks <- car_park_polys
  } else {
    infra$car_parks <- car_park_nodes
  }
  message("  Total car parks (nodes + polygons): ", nrow(infra$car_parks))
  Sys.sleep(1)

  infra$crossings <- fetch_osm_points(bbox, "highway", "crossing", "crossings")
  Sys.sleep(1)

  infra$railway_stations <- fetch_osm_points(bbox, "railway", "station", "railway_stations")
  Sys.sleep(1)

  # Line features
  infra$cycle_paths <- fetch_osm_lines(bbox, "highway", "cycleway", "cycle_paths")
  Sys.sleep(1)

  infra$footpaths <- fetch_osm_lines(bbox, "highway", "footway", "footpaths")

  message("Infrastructure download complete")
  infra
}
