library(tidyverse)
library(wisconsink12)
library(glue)
library(sf)
library(cityforwardcollective)

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

transition_matrix

# Create numeric rating labels
numeric_labels <- c(
  "Fails" = "1",
  "Meets Few" = "2",
  "Meets" = "3",
  "Exceeds" = "4",
  "Significantly Exceeds" = "5"
)

# Transform the data to long format for plotting
transition_long <- transition_matrix |>
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
  # fill in every from/to combination for every sector so empty rows and
  # columns (e.g. no 1-star Independently Operated schools) still get tiles
  complete(broad_agency_type, from_rating, to_rating, fill = list(count = 0))

# Create normalized fill values for each agency type
transition_long_normalized <- transition_long |>
  group_by(broad_agency_type) |>
  mutate(
    max_count_in_group = max(count),
    normalized_fill = count / max_count_in_group
  ) |>
  ungroup()

# Update the data with numeric labels
transition_long_numeric <- transition_long_normalized |>
  mutate(
    from_rating_numeric = factor(
      numeric_labels[from_rating],
      levels = c("1", "2", "3", "4", "5")
    ),
    to_rating_numeric = factor(
      numeric_labels[to_rating],
      levels = c("1", "2", "3", "4", "5")
    )
  )

# Create the plot: grey diagonal = no change (one flat shade), green = rating
# went up, red = rating went down; darker = more schools, scaled only across
# the changer tiles within each agency type
transition_long_numeric <- transition_long_numeric |>
  mutate(
    direction = case_when(
      as.numeric(to_rating) > as.numeric(from_rating) ~ "Rating went up",
      as.numeric(to_rating) < as.numeric(from_rating) ~ "Rating went down",
      TRUE ~ "No change"
    ) |>
      factor(levels = c("Rating went up", "No change", "Rating went down")),
  ) |>
  group_by(broad_agency_type) |>
  mutate(
    max_changers = max(count[direction != "No change"]),
    tile_alpha = case_when(
      direction == "No change" ~ 0.35, # flat shade for the diagonal
      count == 0 ~ 0.06, # faint wash keeps the grid visible
      TRUE ~ 0.25 + 0.75 * count / max_changers
    )
  ) |>
  ungroup()

ggplot(
  transition_long_numeric,
  aes(x = from_rating_numeric, y = to_rating_numeric)
) +
  geom_tile(
    aes(fill = direction, alpha = tile_alpha),
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = ifelse(count > 0, count, ""),
      color = ifelse(tile_alpha > 0.55, "white", "grey15")
    ),
    size = 3,
    fontface = "bold"
  ) +
  facet_wrap(~broad_agency_type, nrow = 1) +
  scale_fill_manual(
    values = c(
      "Rating went up" = "#1a9850",
      "No change" = "grey45",
      "Rating went down" = "#d73027"
    ),
    name = NULL,
    drop = FALSE
  ) +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_x_discrete(position = "bottom") +
  # last year on x, this year on y: 1 at bottom-left on both axes, so
  # improvements sit above the diagonal and declines below
  coord_fixed() + # square tiles
  labs(
    title = "School Rating Transitions by Agency Type",
    subtitle = "Darker green or red = more schools moving between those ratings",
    x = "Last Year's Rating",
    y = "This Year's Rating"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    plot.title.position = "plot",
    legend.position = "none"
  ) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.8)))

ggsave(
  "plots/rating_transitions_by_agency_type.png",
  bg = "white",
  width = 11,
  height = 5
)


# all score changes

these_scores <- c(
  "overall_score",
  "sch_ach",
  "sch_growth",
  "sch_tgo",
  "sch_ot"
)

changes <- map_df(these_scores, function(x) {
  make_mke_rc() |>
    filter(school_year > "2020-21") |>
    select(
      school_year,
      dpi_true_id,
      accurate_agency_type,
      all_of(x)
    ) |>
    pivot_wider(names_from = school_year, values_from = all_of(x)) |>
    mutate(change = `2025-26` - `2024-25`) |>
    # mutate(change = `2024-25` - `2022-23`) |>
    transmute(dpi_true_id, accurate_agency_type, group = x, change) |>
    left_join(
      make_mke_rc() |>
        filter(school_year == "2025-26") |>
        select(dpi_true_id, school_name, overall_rating)
    ) |>
    select(dpi_true_id, school_name, accurate_agency_type, everything())
}) |>
  ungroup() |>
  pivot_wider(names_from = group, values_from = change) |>
  arrange(school_name)

# perc of sector

make_mke_rc() |>
  filter(
    school_year >= "2024-25" &
      accurate_agency_type != "Partnership" &
      str_detect(overall_rating, "Expectations")
  ) |>
  group_by(school_year, broad_agency_type, overall_rating) |>
  summarise(total = sum(school_enrollment, na.rm = TRUE)) |>
  mutate(perc = total / sum(total, na.rm = TRUE)) |>
  select(-total) |>
  pivot_wider(names_from = school_year, values_from = perc)

make_mke_rc() |>
  filter(
    school_year == "2025-26" &
      broad_agency_type == "Independently Operated" &
      overall_rating == "Fails to Meet Expectations"
  )

# perc of rating

make_mke_rc() |>
  filter(
    school_year >= "2024-25" &
      accurate_agency_type != "Partnership" &
      str_detect(overall_rating, "Expectations")
  ) |>
  group_by(school_year, overall_rating, broad_agency_type) |>
  summarise(total = sum(school_enrollment, na.rm = TRUE)) |> View()
  mutate(perc = total / sum(total, na.rm = TRUE)) |>
  select(-total) |>
  pivot_wider(names_from = school_year, values_from = perc)

# show 1 star schools

one_star <- make_mke_rc() |>
  filter(
    school_year == "2025-26" &
      overall_rating == "Fails to Meet Expectations"
  ) |> 
  left_join(geocodes |> 
              select(-school_year))

mke <- tigris::places("WI") |> 
  filter(NAME == "Milwaukee")

one_star |>
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  sf::st_transform(crs = st_crs(mke)) |> 
  ggplot() +
  geom_sf(data = mke, fill = "#feffe5", color = "#395549") +
  geom_sf(aes(size = school_enrollment),
          alpha = .5, color = cfc_darkblue) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(hjust = .5,
                                  margin = margin(0)),
        plot.title.position = "plot") +
  labs(title = "Milwaukee One-Star Schools")

one_star |> 
  summarise(across(
    starts_with("per_"),
    ~ weighted.mean(.x, w = school_enrollment, na.rm = TRUE)
  )) |>
  mutate(across(
    starts_with("per_"),
    ~ scales::label_percent(1)(.x)
  ))

# count of rating

make_mke_rc() |>
  filter(
    school_year >= "2024-25" &
      accurate_agency_type != "Partnership" &
      str_detect(overall_rating, "Expectations")
  ) |>
  group_by(school_year, overall_rating, broad_agency_type) |>
  summarise(total = sum(school_enrollment, na.rm = TRUE)) |>
  mutate(perc = total / sum(total, na.rm = TRUE)) |>
  select(-perc) |>
  pivot_wider(names_from = school_year, values_from = total)

make_mke_rc() |>
  filter(
    school_year == "2025-26" &
      broad_agency_type == "Independently Operated" &
      overall_rating == "Fails to Meet Expectations"
  )

##########

make_wi_rc(exclude_milwaukee = TRUE) |>
  group_by(school_year) |>
  summarise(avg_ach = mean(sch_ach, na.rm = TRUE))

make_mke_rc() |>
  group_by(school_year) |>
  summarise(avg_ach = mean(sch_ach, na.rm = TRUE))


ach_change <- make_wi_rc(exclude_milwaukee = FALSE) |>
  filter(school_year >= "2024-25") |>
  select(school_year, dpi_true_id, sch_ach) |>
  pivot_wider(names_from = school_year, values_from = sch_ach) |>
  mutate(
    group = case_when(
      `2024-25` <= 10 ~ "0-10",
      `2024-25` <= 20 ~ "11-20",
      `2024-25` <= 30 ~ "21-30",
      `2024-25` <= 40 ~ "31-40",
      `2024-25` <= 50 ~ "41-50",
      `2024-25` <= 60 ~ "51-60",
      `2024-25` <= 70 ~ "61-70",
      `2024-25` <= 80 ~ "71-80",
      `2024-25` <= 90 ~ "81-90",
      `2024-25` <= 100 ~ "91-100",
      TRUE ~ "ERROR"
    ),
    change = `2025-26` - `2024-25`
  ) |>
  group_by(group) |>
  mutate(avg = mean(change)) |>
  left_join(
    make_mke_rc() |>
      filter(school_year == "2024-25") |>
      select(dpi_true_id, per_ed)
  )

ach_change |>
  ggplot(aes(group, change)) +
  geom_jitter(width = .15, alpha = .5) +
  geom_segment(aes(
    x = group - .25,
    xend = group + .25,
    y = avg,
    yend = avg
  ))

lm(change ~ per_ed, data = ach_change) |>
  summary()
