# School Rating Transition Matrix Visualization
# Clean script to recreate the heatmap plot

library(tidyverse)
library(wisconsink12)
library(cityforwardcollective)
library(glue)

levs <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations",
  "Meets Expectations",
  "Exceeds Expectations",
  "Significantly Exceeds Expectations"
)

# Overall Score change

rating_change <- make_mke_rc() |>
  filter(school_year > "2020-21") |>
  select(
    school_year,
    dpi_true_id,
    accurate_agency_type,
    overall_rating
  ) |>
  mutate(
    or = factor(overall_rating, levels = levs) |>
      as.numeric()
  ) |>
  select(-c(overall_rating)) |>
  pivot_wider(names_from = school_year, values_from = or) |>
  mutate(change = `2025-26` - `2024-25`) |>
  left_join(
    make_mke_rc() |>
      filter(school_year == "2025-26") |>
      select(dpi_true_id, school_name, overall_score, broad_agency_type)
  ) |>
  select(
    dpi_true_id,
    school_name,
    accurate_agency_type,
    broad_agency_type,
    overall_score,
    everything()
  ) |>
  arrange(desc(overall_score))

# Create transition matrix showing school movements between rating categories
transition_matrix <- rating_change |>
  filter(
    !is.na(`2024-25`) &
      !is.na(`2025-26`) &
      accurate_agency_type != "Partnership"
  ) |> # Remove schools with missing ratings
  mutate(
    from_rating = factor(
      `2024-25`,
      levels = 1:5,
      labels = c(
        "Fails",
        "Meets Few",
        "Meets",
        "Exceeds",
        "Significantly Exceeds"
      )
    ),
    to_rating = factor(
      `2025-26`,
      levels = 1:5,
      labels = c(
        "Fails",
        "Meets Few",
        "Meets",
        "Exceeds",
        "Significantly Exceeds"
      )
    )
  ) |>
  count(broad_agency_type, from_rating, to_rating, name = "n_schools") |>
  pivot_wider(names_from = to_rating, values_from = n_schools, values_fill = 0)
# Create numeric rating labels
numeric_labels <- c(
  "Fails" = "1",
  "Meets Few" = "2",
  "Meets" = "3",
  "Exceeds" = "4",
  "Significantly Exceeds" = "5"
)

# Transform and prepare the data
transition_long_direction <- transition_matrix |>
  # Convert to long format
  pivot_longer(
    cols = -c(broad_agency_type, from_rating),
    names_to = "to_rating",
    values_to = "count"
  ) |>
  # Convert to factors to ensure proper ordering
  mutate(
    from_rating = factor(
      from_rating,
      levels = c(
        "Fails",
        "Meets Few",
        "Meets",
        "Exceeds",
        "Significantly Exceeds"
      )
    ),
    to_rating = factor(
      to_rating,
      levels = c(
        "Fails",
        "Meets Few",
        "Meets",
        "Exceeds",
        "Significantly Exceeds"
      )
    )
  ) |>
  # Fill in every from/to combination for every sector so empty rows and
  # columns (e.g. no 1-star public charters) still get tiles
  complete(broad_agency_type, from_rating, to_rating, fill = list(count = 0)) |>
  # Create normalized fill values for each agency type
  group_by(broad_agency_type) |>
  mutate(
    max_count_in_group = max(count),
    normalized_fill = count / max_count_in_group
  ) |>
  ungroup() |>
  # Add numeric labels
  mutate(
    from_rating_numeric = factor(
      numeric_labels[from_rating],
      levels = c("1", "2", "3", "4", "5")
    ),
    to_rating_numeric = factor(
      numeric_labels[to_rating],
      levels = c("1", "2", "3", "4", "5")
    )
  ) |>
  # Determine change direction
  mutate(
    from_numeric = as.numeric(as.character(from_rating_numeric)),
    to_numeric = as.numeric(as.character(to_rating_numeric)),
    change_direction = case_when(
      from_numeric == to_numeric ~ "Same",
      from_numeric < to_numeric ~ "Improved",
      from_numeric > to_numeric ~ "Declined"
    ),
    change_direction = factor(
      change_direction,
      levels = c("Declined", "Same", "Improved")
    ),
    plot_order = case_when(
      change_direction == "Same" ~ "a",
      change_direction == "Declined" ~ "b",
      change_direction == "Improved" ~ "c",
      TRUE ~ "ERROR"
    ),
  ) |>
  # Shading: the no-change diagonal is one flat shade; darker = more schools,
  # scaled only across the tiles where the rating changed, within each sector
  # (so each panel's busiest changer tile is its darkest)
  group_by(broad_agency_type) |>
  mutate(
    max_changers = max(count[change_direction != "Same"], 1),
    tile_alpha = case_when(
      change_direction == "Same" ~ 0.5,
      count == 0 ~ 0.1,
      TRUE ~ 0.25 + 0.75 * count / max_changers
    ),
    # white labels on dark red/blue tiles, black everywhere else
    dark_tile = change_direction != "Same" & tile_alpha > 0.55
  ) |>
  ungroup()

transition_long_direction |> 
  group_by(broad_agency_type, change_direction) |> 
  summarise(total = sum(count))

# Create the plot
transition_long_direction |>
  arrange(plot_order) |>
  rename(bat = broad_agency_type) |>
  mutate(
    bat = case_when(
      bat == "District Operated" ~ "MPS",
      bat == "Independently Operated" ~ "Public Charter",
      TRUE ~ bat
    ),
    bat = factor(
      bat,
      levels = c(
        "MPS",
        "Public Charter",
        "Private"
      )
    )
  ) |>
  ggplot(aes(x = from_rating_numeric, y = to_rating_numeric)) +
  geom_tile(
    aes(
      fill = change_direction,
      color = change_direction,
      alpha = tile_alpha
    ),
    linewidth = 0.5
  ) +
  geom_text(
    data = \(d) filter(d, !dark_tile),
    aes(label = ifelse(count > 0, count, "")),
    color = "black",
    size = 4,
    family = "Trebuchet MS",
    fontface = "bold"
  ) +
  geom_text(
    data = \(d) filter(d, dark_tile),
    aes(label = count),
    color = "white",
    size = 4,
    family = "Trebuchet MS",
    fontface = "bold"
  ) +
  facet_wrap(~bat, nrow = 1) +
  scale_fill_manual(
    values = c("Declined" = "#c0392b", "Same" = "grey75", "Improved" = "#2f6db5"),
    guide = "none"
  ) +
  scale_color_manual(
    values = c(
      "Declined" = "#c0392b",
      "Same" = "transparent",
      "Improved" = "#2f6db5"
    ),
    guide = "none"
  ) +
  scale_alpha_identity() +
  scale_x_discrete(position = "bottom") +
  labs(
    title = "School Rating Transitions by Sector",
    x = "Last Year's Rating",
    y = "This Year's Rating"
  ) +
  theme(
    text = element_text(family = "Trebuchet MS"),
    plot.title = element_text(size = 20, face = "bold"),
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank(),
    strip.text = element_text(size = 14, face = "bold"),
    aspect.ratio = 1
  )

ggsave(
  "plots/transition_matrix_by_bat_25_26.png",
  bg = "white",
  width = 11,
  height = 5
)
