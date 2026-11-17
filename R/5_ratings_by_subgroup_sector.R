# Report Card ratings of the schools Black, Hispanic and economically
# disadvantaged students attend, by sector, 2025-26 (100% stacked bars)
#
# Method matches the data brief: Academy of Excellence and partnership
# schools are excluded; only schools with one of the five ratings count.
# Subgroup enrollment is estimated as school enrollment x subgroup share.

library(tidyverse)
library(wisconsink12)
library(scales)
library(glue)
library(cityforwardcollective)

this_year <- "2025-26"

# Corrections for misreported private school data in 2025-26. Set
# fix_misreported to FALSE to use the data exactly as DPI published it.
#
# One rule for every correction: where a school's reported figure is clearly
# in error, use that school's most recent valid Report Card figure.
#
# 1. Six private schools report 0% economically disadvantaged after
#    reporting 78-100% in earlier years (five are one-star). Each school's
#    most recent non-zero figure is used.
# 2. Greater Holy Temple Christian Academy reports 0% for every
#    race/ethnicity (about 90% Black last year), and Saint John Paul II
#    School reports 100% white (about 93% Hispanic last year). Each school's
#    prior-year race/ethnicity shares are used.
fix_misreported <- TRUE
last_year <- "2024-25"
race_fix_ids <- c(
  "0000_1345", # Greater Holy Temple Christian Academy
  "0000_2850" # Saint John Paul II School
)

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

group_labs <- c(
  "per_b_aa" = "Black Students",
  "per_hisp_lat" = "Hispanic Students",
  "per_ed" = "Economically Disadvantaged Students"
)

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

if (fix_misreported) {
  all_years <- make_mke_rc()

  # each school's most recent non-zero economically disadvantaged share
  ed_fix <- all_years |>
    filter(school_year < this_year & !is.na(per_ed) & per_ed > 0) |>
    group_by(dpi_true_id) |>
    slice_max(school_year, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(dpi_true_id, fix_ed = per_ed)

  # prior-year race/ethnicity shares for the two schools with bad race data
  race_fix <- all_years |>
    filter(school_year == last_year & dpi_true_id %in% race_fix_ids) |>
    select(
      dpi_true_id,
      fix_b_aa = per_b_aa,
      fix_hisp_lat = per_hisp_lat
    )

  rated <- rated |>
    left_join(ed_fix, by = "dpi_true_id") |>
    left_join(race_fix, by = "dpi_true_id") |>
    mutate(
      per_ed = ifelse(
        sector == "Private" & !is.na(per_ed) & per_ed == 0 & !is.na(fix_ed),
        fix_ed,
        per_ed
      ),
      per_b_aa = coalesce(fix_b_aa, per_b_aa),
      per_hisp_lat = coalesce(fix_hisp_lat, per_hisp_lat)
    ) |>
    select(-c(fix_ed, fix_b_aa, fix_hisp_lat))
}

by_group <- rated |>
  pivot_longer(
    cols = all_of(names(group_labs)),
    names_to = "group",
    values_to = "value"
  ) |>
  mutate(
    group = factor(group_labs[group], levels = group_labs),
    est_enr = school_enrollment * value
  ) |>
  group_by(group, sector, rating) |>
  summarise(students = sum(est_enr, na.rm = TRUE), .groups = "drop") |>
  complete(group, sector, rating, fill = list(students = 0)) |>
  group_by(group, sector) |>
  # bars stack with the highest rating at the left, so walk the ratings in
  # that order to find the middle of each segment for its label
  arrange(group, sector, desc(rating)) |>
  mutate(
    total_students = sum(students),
    perc = students / total_students,
    label_x = cumsum(perc) - perc / 2
  ) |>
  ungroup() |>
  mutate(
    perc_label = ifelse(perc < .005, "<1%", label_percent(1)(perc)),
    # A small boxed label replaces plain text where the text can't be read:
    # anything under 1%, and dark segments (white text) too narrow to hold
    # their label. At either end of the bar, small labels are pinned to the
    # edge so they stay inside the bar.
    sliver = perc > 0 &
      (perc < .01 | (rating %in% dark_ratings & perc < .035)),
    at_left = perc < .035 & label_x < .03,
    at_right = perc < .035 & label_x > .97,
    label_hjust = case_when(at_left ~ 0, at_right ~ 1, TRUE ~ .5),
    label_x = case_when(at_left ~ .004, at_right ~ .996, TRUE ~ label_x)
  )

group_totals <- by_group |>
  distinct(group, sector, total_students)

# check the numbers
by_group |>
  select(group, sector, rating, perc) |>
  pivot_wider(names_from = rating, values_from = perc)

by_group |>
  ggplot(aes(perc, fct_rev(sector), fill = rating)) +
  geom_col(width = .65, color = "white", linewidth = .5) +
  geom_text(
    data = \(d) filter(d, perc > 0 & !sliver),
    aes(
      x = label_x,
      label = perc_label,
      hjust = label_hjust,
      color = rating %in% dark_ratings
    ),
    family = "Trebuchet MS",
    fontface = "bold",
    size = 4
  ) +
  geom_label(
    data = \(d) filter(d, sliver),
    aes(x = label_x, label = perc_label, hjust = label_hjust),
    fill = "white",
    color = "grey15",
    alpha = .9,
    label.size = 0,
    label.padding = unit(.15, "lines"),
    label.r = unit(.2, "lines"),
    family = "Trebuchet MS",
    fontface = "bold",
    size = 3.2
  ) +
  geom_text(
    data = group_totals,
    aes(
      x = 1.01,
      y = fct_rev(sector),
      label = glue("{comma(round(total_students))}\nstudents")
    ),
    inherit.aes = FALSE,
    hjust = 0,
    family = "Trebuchet MS",
    color = "grey50",
    lineheight = .9,
    size = 3.5
  ) +
  facet_wrap(~group, ncol = 1) +
  scale_fill_manual(
    values = rating_cols,
    guide = guide_legend(reverse = TRUE, nrow = 1)
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
    x = "Estimated Percent of Students in Each Group",
    y = "",
    fill = "",
    title = "Access to Quality Schools by Student Group"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.title.position = "plot",
    legend.position = "top",
    legend.justification = "left",
    strip.text = element_text(size = 14, face = "bold", hjust = 0),
    axis.text.y = element_text(size = 12, face = "bold")
  )

ggsave(
  "plots/ratings_by_subgroup_sector_25_26.png",
  bg = "white",
  width = 11,
  height = 10
)
