# plot_simd.R
# Visualisation functions for SIMD choropleth maps and charts

library(ggplot2)
library(dplyr)
library(sf)
library(leaflet)

# Reuse project colour palette
FOCUS_COLOUR <- "#D62828"
SCOTLAND_COLOUR <- "#003049"
OTHER_COLOUR <- "#ADB5BD"

# SIMD decile palette (diverging: most deprived = dark red, least = dark blue)
SIMD_PALETTE <- c(
  "1"  = "#67001F",
  "2"  = "#B2182B",
  "3"  = "#D6604D",
  "4"  = "#F4A582",
  "5"  = "#FDDBC7",
  "6"  = "#D1E5F0",
  "7"  = "#92C5DE",
  "8"  = "#4393C3",
  "9"  = "#2166AC",
  "10" = "#053061"
)

# LISA cluster palette
LISA_PALETTE <- c(
  "High-High"       = "#D62828",
  "Low-Low"         = "#003049",
  "High-Low"        = "#F4A582",
  "Low-High"        = "#92C5DE",
  "Not Significant" = "#E0E0E0"
)

# Focus tier palette for study area map
TIER_PALETTE <- c(
  "Town Centre"             = "#D62828",
  "Wider Ayr"               = "#F4A582",
  "Rest of South Ayrshire"  = "#ADB5BD"
)

#' Interactive Leaflet reference map of all datazones
#'
#' For user review: click a datazone to see its code, IZ name, SIMD rank, decile,
#' and population. Hover for the datazone code. Fill by SIMD decile.
#'
#' @param simd_sf sf object from prepare_simd_spatial()
#' @return leaflet htmlwidget
plot_reference_map <- function(simd_sf) {
  dz_col <- find_datazone_column(simd_sf)

  # Transform to WGS84 for Leaflet
  simd_wgs <- st_transform(simd_sf, 4326)

  # Build colour palette for deciles
  decile_col <- find_column_like(simd_wgs, "SIMD2020v2_Decile")
  rank_col <- find_column_like(simd_wgs, "SIMD2020v2_Rank")
  pop_col <- find_column_like(simd_wgs, "^Population$")
  iz_col <- find_column_like(simd_wgs, "IZname")

  pal <- colorFactor(
    palette = unname(SIMD_PALETTE),
    domain = 1:10,
    ordered = TRUE
  )

  # Also look for datazone name
  dzname_col <- find_column_like(simd_wgs, "DZname")

  # Build popup and label content
  popup_content <- paste0(
    "<strong>Datazone: </strong>", simd_wgs[[dz_col]], "<br>",
    if (!is.null(dzname_col)) paste0("<strong>Name: </strong>", simd_wgs[[dzname_col]], "<br>") else "",
    "<strong>IZ: </strong>", if (!is.null(iz_col)) simd_wgs[[iz_col]] else "N/A", "<br>",
    "<strong>SIMD Rank: </strong>", if (!is.null(rank_col)) simd_wgs[[rank_col]] else "N/A",
    " / 6,976<br>",
    "<strong>Decile: </strong>", if (!is.null(decile_col)) simd_wgs[[decile_col]] else "N/A",
    " (1 = most deprived)<br>",
    "<strong>Population: </strong>", if (!is.null(pop_col)) simd_wgs[[pop_col]] else "N/A"
  )

  label_content <- simd_wgs[[dz_col]]

  decile_values <- if (!is.null(decile_col)) simd_wgs[[decile_col]] else rep(5, nrow(simd_wgs))

  leaflet(simd_wgs) |>
    addProviderTiles(providers$OpenStreetMap) |>
    addPolygons(
      fillColor = ~pal(decile_values),
      fillOpacity = 0.6,
      color = "#333333",
      weight = 1,
      opacity = 0.8,
      popup = popup_content,
      label = label_content,
      highlightOptions = highlightOptions(
        weight = 3,
        color = FOCUS_COLOUR,
        fillOpacity = 0.8,
        bringToFront = TRUE
      )
    ) |>
    addLegend(
      position = "bottomright",
      pal = pal,
      values = 1:10,
      title = "SIMD Decile",
      labels = c("1 (most deprived)", "2", "3", "4", "5",
                  "6", "7", "8", "9", "10 (least deprived)")
    )
}

#' Static choropleth map of SIMD variable
#'
#' @param simd_sf sf object with SIMD data
#' @param variable Column name to map (e.g. "SIMD2020v2_Decile")
#' @param focus_extent Optional bounding box (from st_bbox) to zoom to
#' @param highlight_dzs Optional character vector of datazone codes to outline
#' @param title Optional chart title
#' @return ggplot object
plot_simd_choropleth <- function(simd_sf, variable = NULL,
                                  focus_extent = NULL,
                                  highlight_dzs = NULL,
                                  title = NULL) {
  if (is.null(variable)) {
    variable <- find_column_like(simd_sf, "Decile")
  }

  dz_col <- find_datazone_column(simd_sf)

  p <- ggplot(simd_sf) +
    geom_sf(aes(fill = factor(.data[[variable]])), colour = "grey50", linewidth = 0.1) +
    scale_fill_manual(
      values = SIMD_PALETTE,
      name = "SIMD Decile",
      labels = c("1 (most deprived)", "2", "3", "4", "5",
                  "6", "7", "8", "9", "10 (least deprived)"),
      drop = FALSE
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14)
    )

  if (!is.null(title)) {
    p <- p + labs(title = title)
  }

  # Highlight specific datazones with bold outline
 if (!is.null(highlight_dzs)) {
    highlight_sf <- simd_sf[simd_sf[[dz_col]] %in% highlight_dzs, ]
    p <- p + geom_sf(data = highlight_sf, fill = NA, colour = FOCUS_COLOUR,
                      linewidth = 1.2)
  }

  # Zoom to focus extent
  if (!is.null(focus_extent)) {
    p <- p + coord_sf(
      xlim = c(focus_extent["xmin"], focus_extent["xmax"]),
      ylim = c(focus_extent["ymin"], focus_extent["ymax"]),
      expand = FALSE
    )
  }

  p
}

#' Faceted choropleth maps for all 7 SIMD domains
#'
#' @param simd_sf sf object with SIMD data
#' @param focus_extent Optional bounding box to zoom to
#' @return ggplot object
plot_domain_choropleth_facets <- function(simd_sf, focus_extent = NULL) {
  # Identify domain rank columns
  domain_cols <- grep("Domain_Rank$", names(simd_sf), value = TRUE)

  if (length(domain_cols) == 0) {
    stop("No domain rank columns found in data")
  }

  dz_col <- find_datazone_column(simd_sf)

  # Convert domain ranks to deciles (1-6976 → 1-10)
  n_dzs_national <- 6976
  simd_long <- simd_sf |>
    st_drop_geometry() |>
    select(all_of(c(dz_col, domain_cols))) |>
    pivot_longer(
      cols = all_of(domain_cols),
      names_to = "domain",
      values_to = "rank"
    ) |>
    mutate(
      decile = ceiling(rank / n_dzs_national * 10),
      decile = pmin(decile, 10),
      domain = gsub("SIMD2020v2?_|_Domain_Rank", "", domain),
      domain = gsub("_", " ", domain)
    )

  # Rejoin geometry
  geom_only <- simd_sf |>
    select(all_of(dz_col))

  plot_data <- geom_only |>
    inner_join(simd_long, by = dz_col)

  p <- ggplot(plot_data) +
    geom_sf(aes(fill = factor(decile)), colour = "grey60", linewidth = 0.05) +
    scale_fill_manual(
      values = SIMD_PALETTE,
      name = "Decile",
      labels = c("1 (most\ndeprived)", "2", "3", "4", "5",
                  "6", "7", "8", "9", "10 (least\ndeprived)"),
      drop = FALSE
    ) +
    facet_wrap(~ domain, ncol = 4) +
    theme_void() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 14)
    ) +
    labs(title = "SIMD 2020v2 Domain Ranks by Datazone")

  if (!is.null(focus_extent)) {
    p <- p + coord_sf(
      xlim = c(focus_extent["xmin"], focus_extent["xmax"]),
      ylim = c(focus_extent["ymin"], focus_extent["ymax"]),
      expand = FALSE
    )
  }

  p
}

#' Histogram of national SIMD ranks with focus datazones highlighted
#'
#' @param simd_df Full national SIMD tibble
#' @param focus_datazones Character vector of datazone codes to highlight
#' @return ggplot object
plot_simd_national_context <- function(simd_df, focus_datazones) {
  rank_col <- find_column_like(simd_df, "SIMD2020v2_Rank")

  p <- ggplot(simd_df, aes(x = .data[[rank_col]])) +
    geom_histogram(bins = 50, fill = OTHER_COLOUR, colour = "white", linewidth = 0.2) +
    geom_rug(
      data = filter(simd_df, Data_Zone %in% focus_datazones),
      aes(x = .data[[rank_col]]),
      colour = FOCUS_COLOUR, linewidth = 1, alpha = 0.8
    ) +
    labs(
      title = "Where Ayr town centre datazones sit in the national distribution",
      subtitle = "Red marks = town centre datazones | Rank 1 = most deprived",
      x = "SIMD 2020v2 Rank (1 = most deprived)",
      y = "Number of datazones"
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())

  p
}

#' Bar chart comparing domain ranks across tiers
#'
#' @param domain_summary Long-format tibble from calculate_domain_summary()
#' @param stat Which statistic to plot: "mean" or "median"
#' @return ggplot object
plot_domain_profile <- function(domain_summary, stat = "mean") {
  df <- domain_summary |>
    filter(.data$stat == .env$stat) |>
    mutate(
      domain_short = gsub("SIMD2020v2?_|_Domain_Rank", "", domain),
      domain_short = gsub("_", " ", domain_short)
    )

  group_var <- setdiff(names(df), c("domain", "stat", "value", "n_datazones", "domain_short"))

  ggplot(df, aes(x = domain_short, y = value, fill = .data[[group_var[1]]])) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = TIER_PALETTE, name = NULL) +
    labs(
      title = paste0("SIMD domain ranks by area (", stat, ")"),
      subtitle = "Lower rank = more deprived (rank 1 = most deprived nationally)",
      x = NULL,
      y = paste0(tools::toTitleCase(stat), " domain rank")
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}

#' LISA cluster map
#'
#' @param simd_sf sf object with lisa_cluster column (from run_local_morans_i())
#' @param lisa_column Name of the cluster classification column
#' @param focus_extent Optional bounding box to zoom to
#' @return ggplot object
plot_lisa_clusters <- function(simd_sf, lisa_column = "lisa_cluster",
                                focus_extent = NULL) {
  p <- ggplot(simd_sf) +
    geom_sf(aes(fill = .data[[lisa_column]]), colour = "grey50", linewidth = 0.15) +
    scale_fill_manual(values = LISA_PALETTE, name = "Cluster Type", drop = FALSE) +
    labs(
      title = "Local Moran's I (LISA) Clusters",
      subtitle = "High-High = deprivation hot spot | Low-Low = affluence cluster"
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14)
    )

  if (!is.null(focus_extent)) {
    p <- p + coord_sf(
      xlim = c(focus_extent["xmin"], focus_extent["xmax"]),
      ylim = c(focus_extent["ymin"], focus_extent["ymax"]),
      expand = FALSE
    )
  }

  p
}

#' Moran scatterplot
#'
#' Variable vs. spatially lagged variable, with quadrants for HH/LL/HL/LH.
#'
#' @param simd_sf sf object
#' @param variable Column name of the variable
#' @param weights listw object
#' @param highlight_dzs Optional datazone codes to highlight
#' @return ggplot object
plot_moran_scatterplot <- function(simd_sf, variable, weights,
                                    highlight_dzs = NULL) {
  dz_col <- find_datazone_column(simd_sf)
  x <- simd_sf[[variable]]
  lagged <- lag.listw(weights, x, zero.policy = TRUE)

  plot_df <- data.frame(
    dz = simd_sf[[dz_col]],
    x = x,
    lagged = lagged,
    stringsAsFactors = FALSE
  )

  x_mean <- mean(x, na.rm = TRUE)
  lag_mean <- mean(lagged, na.rm = TRUE)

  p <- ggplot(plot_df, aes(x = x, y = lagged)) +
    geom_hline(yintercept = lag_mean, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = x_mean, linetype = "dashed", colour = "grey60") +
    geom_point(colour = OTHER_COLOUR, alpha = 0.6, size = 2)

  if (!is.null(highlight_dzs)) {
    highlight_df <- filter(plot_df, dz %in% highlight_dzs)
    p <- p + geom_point(data = highlight_df, colour = FOCUS_COLOUR, size = 3, alpha = 0.9)
  }

  p <- p +
    geom_smooth(method = "lm", se = FALSE, colour = SCOTLAND_COLOUR, linewidth = 0.8) +
    annotate("text", x = max(x, na.rm = TRUE), y = max(lagged, na.rm = TRUE),
             label = "HH", hjust = 1, vjust = 1, fontface = "bold", colour = "#D62828") +
    annotate("text", x = min(x, na.rm = TRUE), y = min(lagged, na.rm = TRUE),
             label = "LL", hjust = 0, vjust = 0, fontface = "bold", colour = "#003049") +
    annotate("text", x = max(x, na.rm = TRUE), y = min(lagged, na.rm = TRUE),
             label = "HL", hjust = 1, vjust = 0, fontface = "bold", colour = "#F4A582") +
    annotate("text", x = min(x, na.rm = TRUE), y = max(lagged, na.rm = TRUE),
             label = "LH", hjust = 0, vjust = 1, fontface = "bold", colour = "#92C5DE") +
    labs(
      title = paste0("Moran Scatterplot: ", gsub("_", " ", variable)),
      subtitle = "Variable vs. spatially lagged value | Red points = focus datazones",
      x = variable,
      y = paste0("Spatially lagged ", variable)
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())

  p
}

#' Study area map showing focus tiers
#'
#' @param simd_sf sf object with focus_tier column
#' @return ggplot object
plot_study_area <- function(simd_sf) {
  ggplot(simd_sf) +
    geom_sf(aes(fill = focus_tier), colour = "grey50", linewidth = 0.2) +
    scale_fill_manual(values = TIER_PALETTE, name = "Area") +
    labs(title = "Study Area: South Ayrshire Datazones") +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14)
    )
}


# ---- Utility helpers ----

#' Find a column name matching a pattern in an sf object or tibble
#'
#' @param df Data frame or sf object
#' @param pattern Pattern to grep for (case-insensitive)
#' @return Column name (character) or NULL if not found
find_column_like <- function(df, pattern) {
  cols <- names(df)
  match <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
  if (length(match) > 0) return(match[1])
  NULL
}
