# Milwaukee student enrollment by Report Card rating: refreshed version
#
# A restyled take on the citywide chart in 1_rating_distro_comp.R, which is
# left untouched. Same idea (this year next to last year, by rating), with
# percent labels on the bars and styling that matches the other 2025-26
# charts.
#
# Method matches the data brief: Academy of Excellence and partnership
# schools are excluded; only schools with one of the five ratings count.

library(tidyverse)
library(wisconsink12)
library(scales)
library(glue)
library(cityforwardcollective)

this_year <- "2025-26"
last_year <- "2024-25"

levs <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations",
  "Meets Expectations",
  "Exceeds Expectations",
  "Significantly Exceeds Expectations"
)

year_cols <- setNames(c("grey88", cfc_darkblue), c(last_year, this_year))
year_label_cols <- setNames(c("grey55", cfc_darkblue), c(last_year, this_year))

rating_change <- make_mke_rc() |>
  filter(
    school_year %in% c(last_year, this_year) &
      dpi_true_id != "0000_1652" & # Academy of Excellence
      accurate_agency_type != "Partnership"
  ) |>
  mutate(clean_rating = str_remove(overall_rating, "\\^$")) |>
  filter(clean_rating %in% levs & !is.na(overall_score)) |>
  group_by(school_year, clean_rating) |>
  summarise(
    n = n(),
    total_enrollment = sum(school_enrollment, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(school_year) |>
  mutate(
    perc_enr = total_enrollment / sum(total_enrollment),
    clean_rating = factor(clean_rating, levels = levs),
    school_year = factor(school_year, levels = c(last_year, this_year))
  ) |>
  ungroup() |>
  arrange(school_year, clean_rating)

# check the numbers
rating_change |>
  select(school_year, clean_rating, perc_enr) |>
  pivot_wider(names_from = school_year, values_from = perc_enr)

# figures used in the two annotations
share <- function(year, ratings) {
  rating_change |>
    filter(school_year == year & as.numeric(clean_rating) %in% ratings) |>
    summarise(p = sum(perc_enr)) |>
    pull(p)
}

meeting_now <- share(this_year, 3:5)
meeting_prev <- share(last_year, 3:5)
one_star_now <- share(this_year, 1)
one_star_prev <- share(last_year, 1)

# change in the one-star share, in percentage points, for the note
one_star_pts <- number(abs(one_star_now - one_star_prev) * 100, accuracy = .1)
one_star_dir <- ifelse(one_star_now < one_star_prev, "down", "up")

# heights the annotations are placed against
top_bar <- max(rating_change$perc_enr)
one_star_bar <- max(one_star_now, one_star_prev)

rating_change |>
  ggplot(aes(clean_rating, perc_enr, fill = school_year)) +
  # bars overlap, with this year in front
  geom_col(position = position_dodge(width = .5)) +
  # labels are dodged wider than the bars so each sits over the visible
  # part of its own bar
  geom_text(
    aes(label = label_percent(1)(perc_enr), color = school_year),
    position = position_dodge(width = .9),
    vjust = -.6,
    family = "Trebuchet MS",
    fontface = "bold",
    size = 4.5
  ) +
  # bracket over the three ratings that meet expectations
  annotate(
    geom = "segment",
    x = c(2.55, 2.55, 5.45, 4),
    xend = c(5.45, 2.55, 5.45, 4),
    y = c(top_bar + .07, top_bar + .07, top_bar + .07, top_bar + .07),
    yend = c(top_bar + .07, top_bar + .05, top_bar + .05, top_bar + .085),
    lineend = "round",
    color = "grey50"
  ) +
  annotate(
    geom = "text",
    x = 4,
    y = top_bar + .095,
    vjust = 0,
    size = 5,
    family = "Trebuchet MS",
    lineheight = 1,
    color = "grey40",
    label = glue(
      "{label_percent(1)(meeting_now)} of students were in schools\n",
      "meeting or exceeding expectations in {this_year}"
    )
  ) +
  # note on one-star schools
  annotate(
    geom = "text",
    x = 1,
    y = one_star_bar + .115,
    vjust = 0,
    size = 5,
    family = "Trebuchet MS",
    lineheight = 1,
    color = "grey40",
    label = glue(
      "Share of students in\none-star schools {one_star_dir} {one_star_pts} pts"
    )
  ) +
  annotate(
    geom = "segment",
    x = 1,
    xend = 1,
    y = one_star_bar + .105,
    yend = one_star_bar + .045,
    lineend = "round",
    color = "grey50",
    arrow = arrow(length = unit(3, "mm"), type = "closed")
  ) +
  scale_fill_manual(values = year_cols) +
  scale_color_manual(values = year_label_cols, guide = "none") +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  scale_y_continuous(
    breaks = NULL,
    labels = NULL,
    expand = expansion(mult = c(0, .02))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "",
    y = "",
    fill = "School Year",
    title = "Milwaukee Student Enrollment by Report Card Rating"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 20)),
    plot.title.position = "plot",
    legend.position = "top",
    legend.justification = "left",
    axis.text.x = element_text(size = 13)
  )

ggsave(
  "plots/mke_enrollment_by_rc_rating_citywide_25_26_refresh.png",
  bg = "white",
  width = 11,
  height = 7
)
