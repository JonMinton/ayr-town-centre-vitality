# plot_economy.R
# Visualisation functions for local economy analysis

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Reuse project palette from plot_simd.R
FOCUS_COLOUR <- "#D62828"
SCOTLAND_COLOUR <- "#003049"
OTHER_COLOUR <- "#ADB5BD"

TIER_PALETTE <- c(
  "Town Centre"             = "#D62828",
  "Wider Ayr"               = "#F4A582",
  "Rest of South Ayrshire"  = "#ADB5BD"
)

# Sector colour palette for employment charts
SECTOR_PALETTE <- c(
  "Retail & wholesale"           = "#E76F51",
  "Hospitality"                  = "#F4A261",
  "Health & social care"         = "#2A9D8F",
  "Education"                    = "#264653",
  "Public administration"        = "#457B9D",
  "Arts, recreation & other services" = "#E9C46A",
  "Professional & business services"  = "#606C38",
  "Construction & manufacturing"      = "#BC6C25",
  "Transport & storage"               = "#8ECAE6",
  "Other (agriculture, energy, water, mining)" = "#ADB5BD"
)

#' Cleveland dot plot of datazone-level indicator values
#'
#' Shows each datazone as a point, ordered by value, with town centre
#' datazones highlighted.
#'
#' @param dz_data Tibble from get_datazone_economy()
#' @param indicator Column name to plot
#' @param label Human-readable label for x-axis
#' @param title Plot title
#' @return ggplot object
plot_indicator_dotplot <- function(dz_data, indicator, label = indicator,
                                   title = NULL) {
  df <- dz_data |>
    filter(!is.na(.data[[indicator]])) |>
    arrange(.data[[indicator]]) |>
    mutate(rank_order = row_number())

  ggplot(df, aes(x = .data[[indicator]], y = rank_order)) +
    geom_point(
      aes(colour = focus_tier, size = focus_tier),
      alpha = 0.8
    ) +
    scale_colour_manual(values = TIER_PALETTE) +
    scale_size_manual(values = c("Town Centre" = 3, "Wider Ayr" = 2,
                                  "Rest of South Ayrshire" = 1.5)) +
    labs(
      x = label,
      y = "Datazones (ordered by value)",
      colour = "Tier",
      size = "Tier",
      title = title
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom"
    )
}

#' Stacked bar chart of employment by sector and tier
#'
#' @param employment_df Tibble from calculate_employment_structure()
#' @param show_pct Logical, whether to show percentages (TRUE) or counts (FALSE)
#' @param title Plot title
#' @return ggplot object
plot_employment_by_sector <- function(employment_df, show_pct = TRUE,
                                       title = "Employment by Sector and Study Area Tier") {
  y_var <- if (show_pct) "pct" else "employment"
  y_lab <- if (show_pct) "Share of employment (%)" else "Employment count"

  # Order sectors by overall size
  sector_order <- employment_df |>
    group_by(sector) |>
    summarise(total = sum(employment), .groups = "drop") |>
    arrange(desc(total)) |>
    pull(sector)

  employment_df$sector <- factor(employment_df$sector, levels = rev(sector_order))

  ggplot(employment_df, aes(x = focus_tier, y = .data[[y_var]], fill = sector)) +
    geom_col(position = "stack", width = 0.7) +
    scale_fill_manual(values = SECTOR_PALETTE) +
    labs(x = NULL, y = y_lab, fill = "Sector", title = title) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right") +
    guides(fill = guide_legend(reverse = TRUE, ncol = 1))
}

#' Horizontal bar chart of top sectors by employment
#'
#' @param employment_df Tibble from calculate_employment_structure()
#' @param tier Focus tier to show (default: shows all)
#' @param top_n Number of sectors to show
#' @param title Plot title
#' @return ggplot object
plot_top_sectors <- function(employment_df, tier = NULL, top_n = 10,
                              title = "Top Sectors by Employment") {
  df <- employment_df
  if (!is.null(tier)) {
    df <- df |> filter(focus_tier == tier)
  } else {
    df <- df |>
      group_by(sector) |>
      summarise(employment = sum(employment), .groups = "drop")
  }

  df <- df |>
    arrange(desc(employment)) |>
    head(top_n) |>
    mutate(sector = reorder(sector, employment))

  ggplot(df, aes(x = sector, y = employment)) +
    geom_col(fill = FOCUS_COLOUR, width = 0.7) +
    geom_text(aes(label = comma(employment)), hjust = -0.1, size = 3.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)), labels = comma) +
    coord_flip() +
    labs(x = NULL, y = "Employment", title = title) +
    theme_minimal(base_size = 12)
}

#' Radar chart of economic health profile
#'
#' Shows normalised indicators as a spider/radar chart using coord_polar.
#'
#' @param health_df Tibble from calculate_economic_health_profile()
#' @param title Plot title
#' @return ggplot object
plot_radar_chart <- function(health_df, title = "Economic Health Profile") {
  # Clean indicator names for display
  label_map <- c(
    "Income_rate" = "Income\nDeprivation",
    "Employment_rate" = "Employment\nDeprivation",
    "crime_rate" = "Crime\nRate",
    "overcrowded_rate" = "Overcrowding",
    "nocentralheat_rate" = "No Central\nHeating",
    "CIF" = "Illness\n(CIF)",
    "ALCOHOL" = "Alcohol\nHospitalisations",
    "DRUG" = "Drug\nHospitalisations"
  )

  df <- health_df |>
    mutate(indicator_label = ifelse(indicator %in% names(label_map),
                                     label_map[indicator], indicator))

  # For radar chart, all tiers need the same indicator order
  ind_order <- unique(df$indicator_label)
  df$indicator_label <- factor(df$indicator_label, levels = ind_order)

  ggplot(df, aes(x = indicator_label, y = national_percentile,
                  group = focus_tier, colour = focus_tier)) +
    geom_polygon(aes(fill = focus_tier), alpha = 0.1) +
    geom_point(size = 2) +
    geom_line() +
    scale_colour_manual(values = TIER_PALETTE) +
    scale_fill_manual(values = TIER_PALETTE) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
    coord_polar() +
    labs(
      title = title,
      subtitle = "National percentile (higher = worse position)",
      colour = "Tier", fill = "Tier"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    )
}

#' Scatter plot of two indicators coloured by tier
#'
#' @param dz_data Tibble from get_datazone_economy()
#' @param x_var Column name for x-axis
#' @param y_var Column name for y-axis
#' @param x_label Human-readable x-axis label
#' @param y_label Human-readable y-axis label
#' @param title Plot title
#' @return ggplot object
plot_deprivation_scatter <- function(dz_data, x_var, y_var,
                                      x_label = x_var, y_label = y_var,
                                      title = NULL) {
  ggplot(dz_data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(aes(colour = focus_tier, size = Population), alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, colour = "grey40",
                linewidth = 0.5, linetype = "dashed") +
    scale_colour_manual(values = TIER_PALETTE) +
    scale_size_continuous(range = c(1.5, 5), guide = "none") +
    labs(x = x_label, y = y_label, colour = "Tier", title = title) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

#' Paired bar chart comparing drive vs public transport times
#'
#' @param access_df Tibble from calculate_access_summary()
#' @param services Character vector of service types to show
#' @param title Plot title
#' @return ggplot object
plot_access_comparison <- function(access_df,
                                    services = c("GP", "post", "retail"),
                                    title = "Access to Services: Drive Time vs Public Transport") {
  # Reshape to get drive and PT side by side
  drive_df <- access_df |>
    filter(grepl("^drive_", indicator)) |>
    mutate(
      service = gsub("^drive_", "", indicator),
      mode = "Drive"
    )

  pt_df <- access_df |>
    filter(grepl("^PT_", indicator)) |>
    mutate(
      service = gsub("^PT_", "", indicator),
      mode = "Public Transport"
    )

  combined <- bind_rows(drive_df, pt_df) |>
    filter(service %in% services)

  # Clean service names
  service_labels <- c(
    "GP" = "GP Surgery",
    "post" = "Post Office",
    "retail" = "Retail Centre"
  )
  combined$service_label <- service_labels[combined$service]

  ggplot(combined, aes(x = focus_tier, y = mean_value, fill = mode)) +
    geom_col(position = "dodge", width = 0.7) +
    facet_wrap(~service_label, scales = "free_y") +
    scale_fill_manual(values = c("Drive" = SCOTLAND_COLOUR,
                                  "Public Transport" = FOCUS_COLOUR)) +
    labs(
      x = NULL,
      y = "Mean travel time (minutes)",
      fill = "Mode",
      title = title
    ) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}

#' Stacked bar chart showing working age vs non-working age population
#'
#' @param simd_sf sf object with Population and Working_Age_Population
#' @param title Plot title
#' @return ggplot object
plot_working_age_composition <- function(simd_sf,
                                          title = "Population Composition by Study Area Tier") {
  df <- simd_sf |>
    st_drop_geometry() |>
    group_by(focus_tier) |>
    summarise(
      `Working Age (16-64)` = sum(Working_Age_Population, na.rm = TRUE),
      `Non-Working Age` = sum(Population, na.rm = TRUE) -
        sum(Working_Age_Population, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = c(`Working Age (16-64)`, `Non-Working Age`),
      names_to = "age_group",
      values_to = "count"
    )

  ggplot(df, aes(x = focus_tier, y = count, fill = age_group)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = comma(count)), position = position_stack(vjust = 0.5),
              size = 3.5, colour = "white", fontface = "bold") +
    scale_fill_manual(values = c("Working Age (16-64)" = SCOTLAND_COLOUR,
                                  "Non-Working Age" = "#F4A582")) +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    labs(x = NULL, y = "Population", fill = NULL, title = title) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
}
