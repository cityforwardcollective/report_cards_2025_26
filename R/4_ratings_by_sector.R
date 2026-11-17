# Report Card ratings by sector, 2025-26
# Share of students in each rating, by sector (100% stacked bars)
#
# Method matches the data brief: Academy of Excellence and partnership
# schools are excluded; only schools with one of the five ratings count.

library(tidyverse)
library(wisconsink12)
library(scales)
library(glue)
library(cityforwardcollective)

this_year <- "2025-26"

levs <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations",
  "Meets Expectations",
  "Exceeds Expectations",
  "Significantly Exceeds Expectations"
)

short_levs <- c(
  "Fails to Meet",
  "Meets Few",
  "Meets",
  "Exceeds",
  "Significantly Exceeds"
)

rating_cols <- c(
  "Fails to Meet" = "#c0392b",
  "Meets Few" = "#e8a598",
  "Meets" = "#b8cfea",
  "Exceeds" = "#7fa8d9",
  "Significantly Exceeds" = cfc_darkblue
)

# ratings dark enough to need white labels
dark_ratings <- c("Fails to Meet", "Significantly Exceeds")

rated <- make_mke_rc() |>
  filter(
    school_year == this_year &
      dpi_true_id != "0000_1652" & # Academy of Excellence
      accurate_agency_type != "Partnership"
  ) |>
  mutate(clean_rating = str_remove(overall_rating, "\\^$")) |>
  filter(clean_rating %in% levs & !is.na(overall_score)) |>
  mutate(
    rating = factor(clean_rating, levels = levs, labels = short_levs),
    sector = case_when(
      broad_agency_type == "District Operated" ~ "MPS",
      broad_agency_type == "Independently Operated" ~ "Public Charter",
      broad_agency_type == "Private" ~ "Private",
      TRUE ~ "WHOOPS!"
    ),
    sector = factor(sector, levels = c("MPS", "Public Charter", "Private"))
  )

by_sector <- rated |>
  group_by(sector, rating) |>
  summarise(
    schools = n(),
    students = sum(school_enrollment, na.rm = TRUE),
    .groups = "drop"
  ) |>
  complete(sector, rating, fill = list(schools = 0, students = 0)) |>
  group_by(sector) |>
  # bars stack with the lowest rating at the left and the highest at the
  # right, so walk the ratings in that order to find the middle of each
  # segment for its label
  arrange(sector, rating) |>
  mutate(
    total_students = sum(students),
    perc = students / total_students,
    label_x = cumsum(perc) - perc / 2
  ) |>
  ungroup() |>
  mutate(
    perc_label = ifelse(perc < .005, "<1%", label_percent(1)(perc)),
    # slivers under 3.5% get a small boxed label instead of plain text; at
    # either end of the bar the box is pinned to the edge so it stays inside
    sliver = perc > 0 & perc < .035,
    sliver_hjust = case_when(label_x < .03 ~ 0, label_x > .97 ~ 1, TRUE ~ .5),
    sliver_x = case_when(label_x < .03 ~ .004, label_x > .97 ~ .996, TRUE ~ label_x)
  )

sector_totals <- by_sector |>
  distinct(sector, total_students)

# check the numbers
by_sector |>
  select(sector, rating, perc) |>
  pivot_wider(names_from = rating, values_from = perc)

by_sector |>
  ggplot(aes(perc, fct_rev(sector), fill = rating)) +
  geom_col(
    width = .6,
    color = "white",
    linewidth = .5,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    data = \(d) filter(d, perc >= .035),
    aes(
      x = label_x,
      label = perc_label,
      color = rating %in% dark_ratings
    ),
    family = "Trebuchet MS",
    fontface = "bold",
    size = 4.5
  ) +
  geom_label(
    data = \(d) filter(d, sliver),
    aes(x = sliver_x, label = perc_label, hjust = sliver_hjust),
    fill = "white",
    color = "grey15",
    alpha = .9,
    label.size = 0,
    label.padding = unit(.15, "lines"),
    label.r = unit(.2, "lines"),
    family = "Trebuchet MS",
    fontface = "bold",
    size = 3.5
  ) +
  geom_text(
    data = sector_totals,
    aes(
      x = 1.01,
      y = fct_rev(sector),
      label = glue("{comma(total_students)}\nstudents")
    ),
    inherit.aes = FALSE,
    hjust = 0,
    family = "Trebuchet MS",
    color = "grey50",
    lineheight = .9,
    size = 4
  ) +
  scale_fill_manual(
    values = rating_cols,
    guide = guide_legend(nrow = 1)
  ) +
  scale_color_manual(
    values = c("TRUE" = "white", "FALSE" = "grey15"),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = label_percent(),
    breaks = seq(0, 1, .25),
    expand = expansion(mult = c(0, .12))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Percent of Student Enrollment",
    y = "",
    fill = "",
    title = "Report Card Ratings by Sector"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.title.position = "plot",
    legend.position = "top",
    legend.justification = "left",
    axis.text.y = element_text(size = 14, face = "bold")
  )

ggsave(
  "plots/ratings_by_sector_25_26.png",
  bg = "white",
  width = 11,
  height = 6
)
