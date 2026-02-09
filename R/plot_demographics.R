# plot_demographics.R
# Visualisation functions for demographic analysis

library(ggplot2)
library(dplyr)

# Colour palette
FOCUS_COLOUR <- "#D62828"    # Red for South Ayrshire
SCOTLAND_COLOUR <- "#003049" # Dark blue for Scotland
OTHER_COLOUR <- "#ADB5BD"    # Grey for other areas
MALE_COLOUR <- "#457B9D"
FEMALE_COLOUR <- "#E76F51"

#' Population pyramid for a single area, optionally overlaid with Scotland
#'
#' @param pop_df Population data (long format with age, sex, population)
#' @param area_code Council area code to plot
#' @param area_name Optional display name (auto-detected if NULL)
#' @param compare_to_scotland Overlay Scotland proportions as outline
#' @return ggplot object
plot_population_pyramid <- function(pop_df, area_code,
                                    area_name = NULL,
                                    compare_to_scotland = TRUE) {
  # Get area data
  area_data <- pop_df |>
    filter(council_area_code == area_code, sex %in% c("male", "female"))

  if (is.null(area_name)) {
    area_name <- unique(area_data$council_area_name)[1]
  }

  # Convert to proportions
  area_total <- sum(pop_df$population[pop_df$council_area_code == area_code & pop_df$sex == "all"])
  area_data <- area_data |>
    mutate(
      prop = population / area_total,
      prop = ifelse(sex == "male", -prop, prop)
    )

  p <- ggplot(area_data, aes(x = age, y = prop, fill = sex)) +
    geom_bar(stat = "identity", width = 1, alpha = 0.8) +
    scale_fill_manual(values = c("male" = MALE_COLOUR, "female" = FEMALE_COLOUR),
                      labels = c("male" = "Male", "female" = "Female")) +
    coord_flip() +
    scale_y_continuous(
      labels = function(x) paste0(abs(round(x * 100, 1)), "%"),
      limits = function(x) c(-max(abs(x)), max(abs(x)))
    ) +
    labs(
      title = area_name,
      x = "Age",
      y = "% of population",
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  # Overlay Scotland proportions
  if (compare_to_scotland && any(pop_df$council_area_code == "S92000003")) {
    scot_data <- pop_df |>
      filter(council_area_code == "S92000003", sex %in% c("male", "female"))
    scot_total <- sum(pop_df$population[pop_df$council_area_code == "S92000003" & pop_df$sex == "all"])
    scot_data <- scot_data |>
      mutate(
        prop = population / scot_total,
        prop = ifelse(sex == "male", -prop, prop)
      )

    p <- p +
      geom_step(data = scot_data, aes(x = age, y = prop, group = sex),
                colour = SCOTLAND_COLOUR, linewidth = 0.5, alpha = 0.7,
                inherit.aes = FALSE) +
      labs(subtitle = paste0("Dark outline = Scotland average"))
  }

  p
}

#' Faceted population pyramids for multiple areas
#'
#' @param pop_df Population data
#' @param area_codes Character vector of council area codes
#' @return ggplot object
plot_pyramid_comparison <- function(pop_df, area_codes) {
  plot_data <- pop_df |>
    filter(council_area_code %in% area_codes, sex %in% c("male", "female"))

  # Calculate proportions within each area
  totals <- pop_df |>
    filter(council_area_code %in% area_codes, sex == "all") |>
    group_by(council_area_code) |>
    summarise(total = sum(population), .groups = "drop")

  plot_data <- plot_data |>
    left_join(totals, by = "council_area_code") |>
    mutate(
      prop = population / total,
      prop = ifelse(sex == "male", -prop, prop)
    )

  ggplot(plot_data, aes(x = age, y = prop, fill = sex)) +
    geom_bar(stat = "identity", width = 1, alpha = 0.8) +
    scale_fill_manual(values = c("male" = MALE_COLOUR, "female" = FEMALE_COLOUR),
                      labels = c("male" = "Male", "female" = "Female")) +
    coord_flip() +
    scale_y_continuous(
      labels = function(x) paste0(abs(round(x * 100, 1)), "%"),
      limits = function(x) c(-max(abs(x)), max(abs(x)))
    ) +
    facet_wrap(~ council_area_name, ncol = 2) +
    labs(x = "Age", y = "% of population", fill = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

#' Horizontal bar chart comparing one indicator across all council areas
#'
#' @param indicators_df Indicators tibble from calculate_all_indicators()
#' @param indicator_col Character name of the indicator column to plot
#' @param focus_area Council area code to highlight
#' @param title Optional chart title
#' @param pct Format as percentage
#' @return ggplot object
plot_indicator_comparison <- function(indicators_df, indicator_col,
                                      focus_area = "S12000028",
                                      title = NULL, pct = FALSE) {
  df <- indicators_df |>
    filter(council_area_code != "S92000003") |>
    mutate(
      fill_group = case_when(
        council_area_code == focus_area ~ "focus",
        TRUE ~ "other"
      )
    )

  # Scotland as reference line
  scot_val <- indicators_df |>
    filter(council_area_code == "S92000003") |>
    pull(!!sym(indicator_col))

  if (is.null(title)) {
    title <- gsub("_", " ", indicator_col) |> tools::toTitleCase()
  }

  p <- ggplot(df, aes(x = reorder(council_area_name, !!sym(indicator_col)),
                       y = !!sym(indicator_col), fill = fill_group)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c("focus" = FOCUS_COLOUR, "other" = OTHER_COLOUR),
                      guide = "none") +
    coord_flip() +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())

  if (length(scot_val) == 1 && !is.na(scot_val)) {
    p <- p + geom_hline(yintercept = scot_val, linetype = "dashed",
                         colour = SCOTLAND_COLOUR, linewidth = 0.6) +
      annotate("text", x = 1, y = scot_val, label = "Scotland",
               hjust = -0.1, vjust = -0.5, colour = SCOTLAND_COLOUR, size = 3)
  }

  if (pct) {
    p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }

  p
}

#' Bar chart of most similar council areas by demographic distance
#'
#' @param similarity_df Output of calculate_demographic_similarity()
#' @param top_n Number of areas to show
#' @return ggplot object
plot_similarity_ranking <- function(similarity_df, top_n = 10) {
  df <- similarity_df |>
    slice_head(n = top_n)

  ggplot(df, aes(x = reorder(council_area_name, -distance), y = distance)) +
    geom_col(fill = MALE_COLOUR, width = 0.7) +
    coord_flip() +
    labs(
      title = "Most demographically similar council areas",
      subtitle = "Euclidean distance in standardised indicator space (smaller = more similar)",
      x = NULL,
      y = "Distance"
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
}
