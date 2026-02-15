# plot_access.R
# Visualisation functions for urban accessibility analysis

library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(scales)

# Reuse project palette
FOCUS_COLOUR <- "#D62828"
SCOTLAND_COLOUR <- "#003049"
OTHER_COLOUR <- "#ADB5BD"

TIER_PALETTE <- c(
  "Town Centre"             = "#D62828",
  "Wider Ayr"               = "#F4A582",
  "Rest of South Ayrshire"  = "#ADB5BD"
)

# Subdued fills for base map (lighter versions)
TIER_FILL_PALETTE <- c(
  "Town Centre"             = "#F4A0A0",
  "Wider Ayr"               = "#FDDBC7",
  "Rest of South Ayrshire"  = "#E8ECEF"
)

INFRA_PALETTE <- c(
  "Bus stops"            = "#E76F51",
  "Car parks"            = "#264653",
  "Pedestrian crossings" = "#2A9D8F",
  "Cycle paths"          = "#E9C46A",
  "Railway stations"     = "#F4A261"
)

# Pretty labels for infrastructure names
INFRA_LABELS <- c(
  bus_stops         = "Bus stops",
  car_parks         = "Car parks",
  crossings         = "Pedestrian crossings",
  railway_stations  = "Railway stations",
  cycle_paths       = "Cycle paths",
  footpaths         = "Footpaths"
)

#' Base map of study area with datazone boundaries coloured by tier
#'
#' @param simd_sf sf object with focus_tier
#' @param focus_extent Optional bbox for zooming (e.g. from st_bbox())
#' @return ggplot object (base layer)
plot_base_map <- function(simd_sf, focus_extent = NULL) {
  p <- ggplot(simd_sf) +
    geom_sf(aes(fill = focus_tier), colour = "grey60", linewidth = 0.15) +
    scale_fill_manual(values = TIER_FILL_PALETTE, name = "Area") +
    theme_void() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, colour = "grey40")
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

#' Map of infrastructure points overlaid on study area
#'
#' @param simd_sf sf object with datazones
#' @param points_sf sf object with infrastructure features
#' @param feature_name Character label for legend (e.g. "Bus stops")
#' @param point_colour Colour for infrastructure points
#' @param point_size Size of points
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param focus_extent Optional bbox for zooming
#' @return ggplot object
plot_infrastructure_map <- function(simd_sf, points_sf, feature_name,
                                     point_colour = FOCUS_COLOUR,
                                     point_size = 2,
                                     title = NULL, subtitle = NULL,
                                     focus_extent = NULL) {
  p <- plot_base_map(simd_sf, focus_extent)

  if (nrow(points_sf) > 0) {
    p <- p +
      geom_sf(data = points_sf, colour = point_colour,
              size = point_size, alpha = 0.7,
              show.legend = FALSE)
  }

  if (!is.null(title)) p <- p + labs(title = title)
  if (!is.null(subtitle)) p <- p + labs(subtitle = subtitle)

  p
}

#' Map of line features (cycle paths, footpaths) overlaid on study area
#'
#' @param simd_sf sf object with datazones
#' @param lines_sf sf object with line features
#' @param feature_name Character label
#' @param line_colour Colour for lines
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param focus_extent Optional bbox for zooming
#' @return ggplot object
plot_line_infrastructure_map <- function(simd_sf, lines_sf, feature_name,
                                          line_colour = "#2A9D8F",
                                          title = NULL, subtitle = NULL,
                                          focus_extent = NULL) {
  p <- plot_base_map(simd_sf, focus_extent)

  if (nrow(lines_sf) > 0) {
    p <- p +
      geom_sf(data = lines_sf, colour = line_colour,
              linewidth = 0.6, alpha = 0.7,
              show.legend = FALSE)
  }

  if (!is.null(title)) p <- p + labs(title = title)
  if (!is.null(subtitle)) p <- p + labs(subtitle = subtitle)

  p
}

#' Combined infrastructure map showing multiple feature types
#'
#' @param simd_sf sf object with datazones
#' @param infra_list Named list of sf objects
#' @param focus_extent Optional bbox
#' @param title Plot title
#' @return ggplot object
plot_combined_infrastructure_map <- function(simd_sf, infra_list,
                                              focus_extent = NULL,
                                              title = "Transport Infrastructure") {
  p <- plot_base_map(simd_sf, focus_extent)

  # Add point features with colour coding
  point_features <- c("bus_stops", "car_parks", "crossings", "railway_stations")

  for (feat in intersect(point_features, names(infra_list))) {
    feat_sf <- infra_list[[feat]]
    if (nrow(feat_sf) > 0) {
      label <- INFRA_LABELS[feat]
      colour <- INFRA_PALETTE[label]
      feat_sf$feature_type <- label
      p <- p +
        geom_sf(data = feat_sf,
                aes(colour = feature_type),
                size = 1.5, alpha = 0.7,
                show.legend = TRUE)
    }
  }

  # Add line features
  if ("cycle_paths" %in% names(infra_list) && nrow(infra_list$cycle_paths) > 0) {
    infra_list$cycle_paths$feature_type <- "Cycle paths"
    p <- p +
      geom_sf(data = infra_list$cycle_paths,
              aes(colour = feature_type),
              linewidth = 0.5, alpha = 0.6,
              show.legend = TRUE)
  }

  p <- p +
    scale_colour_manual(values = INFRA_PALETTE, name = "Infrastructure") +
    labs(title = title) +
    guides(
      fill = guide_legend(order = 1),
      colour = guide_legend(order = 2, override.aes = list(size = 3))
    )

  p
}

#' Infrastructure density comparison chart
#'
#' @param density_df Tibble from summarise_infrastructure_by_tier()
#' @param metric Which metric to plot: "per_1000_pop" or "per_km2"
#' @param title Plot title
#' @return ggplot object
plot_infrastructure_density <- function(density_df,
                                         metric = "per_1000_pop",
                                         title = "Infrastructure Density by Area") {
  y_lab <- if (metric == "per_1000_pop") {
    "Features per 1,000 population"
  } else {
    "Features per km\u00B2"
  }

  df <- density_df |>
    mutate(feature_label = INFRA_LABELS[feature])

  ggplot(df, aes(x = feature_label, y = .data[[metric]], fill = focus_tier)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    scale_fill_manual(values = TIER_PALETTE) +
    coord_flip() +
    labs(x = NULL, y = y_lab, fill = "Area", title = title) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

#' Access gap (PT penalty) bar chart
#'
#' Shows the extra travel time by public transport compared to driving.
#'
#' @param gap_df Tibble from calculate_access_gap()
#' @param title Plot title
#' @return ggplot object
plot_access_gap <- function(gap_df,
                             title = "The Public Transport Penalty") {
  ggplot(gap_df, aes(x = service, y = mean_gap, fill = focus_tier)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_hline(yintercept = 0, colour = "grey40") +
    scale_fill_manual(values = TIER_PALETTE) +
    coord_flip() +
    labs(
      x = NULL,
      y = "Extra minutes by public transport vs driving",
      fill = "Area",
      title = title,
      subtitle = "Positive values = PT takes longer than driving"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

#' Paired bar chart of drive vs PT times by service and tier
#'
#' Adapted from plot_access_comparison in plot_economy.R for standalone use.
#'
#' @param gap_df Tibble from calculate_access_gap()
#' @param title Plot title
#' @return ggplot object
plot_drive_vs_pt <- function(gap_df,
                              title = "Drive Time vs Public Transport Time") {
  # Reshape to long format
  long_df <- gap_df |>
    select(focus_tier, service, Drive = mean_drive, `Public Transport` = mean_pt) |>
    pivot_longer(cols = c(Drive, `Public Transport`),
                 names_to = "mode", values_to = "minutes")

  ggplot(long_df, aes(x = focus_tier, y = minutes, fill = mode)) +
    geom_col(position = "dodge", width = 0.7) +
    facet_wrap(~ service, scales = "free_y") +
    scale_fill_manual(values = c("Drive" = SCOTLAND_COLOUR,
                                  "Public Transport" = FOCUS_COLOUR)) +
    labs(x = NULL, y = "Mean travel time (minutes)", fill = "Mode",
         title = title) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

#' Access domain choropleth
#'
#' Maps the SIMD Access Domain Rank converted to decile.
#'
#' @param simd_sf sf object with SIMD data
#' @param focus_extent Optional bbox for zooming
#' @param title Plot title
#' @return ggplot object
plot_access_domain_choropleth <- function(simd_sf, focus_extent = NULL,
                                           title = "SIMD Access Domain") {
  # SIMD decile palette
  simd_palette <- c(
    "1"  = "#67001F", "2"  = "#B2182B", "3"  = "#D6604D",
    "4"  = "#F4A582", "5"  = "#FDDBC7", "6"  = "#D1E5F0",
    "7"  = "#92C5DE", "8"  = "#4393C3", "9"  = "#2166AC",
    "10" = "#053061"
  )

  # Find access domain rank column
  access_col <- grep("Access.*Domain.*Rank", names(simd_sf), value = TRUE)
  if (length(access_col) == 0) {
    access_col <- grep("Geographic_Access", names(simd_sf), value = TRUE)
  }

  if (length(access_col) == 0) {
    stop("No access domain rank column found")
  }
  access_col <- access_col[1]

  # Convert rank to decile
  n_dzs_national <- 6976
  simd_sf$access_decile <- ceiling(simd_sf[[access_col]] / n_dzs_national * 10)
  simd_sf$access_decile <- pmin(simd_sf$access_decile, 10)

  p <- ggplot(simd_sf) +
    geom_sf(aes(fill = factor(access_decile)), colour = "grey50",
            linewidth = 0.1) +
    scale_fill_manual(
      values = simd_palette,
      name = "Access Decile",
      labels = c("1 (worst access)", "2", "3", "4", "5",
                  "6", "7", "8", "9", "10 (best access)"),
      drop = FALSE
    ) +
    labs(title = title) +
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

#' Cleveland dot plot of nearest infrastructure distance by datazone
#'
#' @param distance_df Tibble from calculate_nearest_distance()
#' @param title Plot title
#' @return ggplot object
plot_nearest_distance <- function(distance_df,
                                    title = "Distance to Nearest Infrastructure") {
  if (nrow(distance_df) == 0) return(ggplot() + theme_void())

  df <- distance_df |>
    arrange(nearest_distance_m) |>
    mutate(rank_order = row_number())

  ggplot(df, aes(x = nearest_distance_m, y = rank_order)) +
    geom_point(
      aes(colour = focus_tier, size = focus_tier),
      alpha = 0.8
    ) +
    scale_colour_manual(values = TIER_PALETTE) +
    scale_size_manual(values = c("Town Centre" = 3, "Wider Ayr" = 2,
                                  "Rest of South Ayrshire" = 1.5)) +
    scale_x_continuous(labels = comma) +
    labs(
      x = "Distance to nearest feature (metres)",
      y = "Datazones (ordered by distance)",
      colour = "Area",
      size = "Area",
      title = title
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom"
    )
}
