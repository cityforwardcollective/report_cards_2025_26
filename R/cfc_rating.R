library(tidyverse)
library(wisconsink12)
library(scales)
library(cityforwardcollective)

indc <- c("2r/2x Charter", "Non-Instrumentality Charter")

lq <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations"
)

# make_mke_rc() |>
#   filter(is.na(sch_tgo) &
#            !is.na(sch_ach) &
#            !is.na(sch_growth) &
#            school_year == "2022-23") |>
#   select(school_year,
#          dpi_true_id,
#          sch_ach,
#          sch_growth,
#          sch_tgo,
#          sch_ot,
#          ends_with("weight")) |>
#   mutate(new_agw = (ach_weight + growth_weight) / 2) |>
#   View()
these_years <- c("2022-23", "2023-24", "2024-25", "2025-26")

cfc_scored <- map_df(these_years, function(year) {
  this <- map2_df(
    c("sch_ach", "sch_growth", "sch_tgo", "sch_ot"),
    c("ach_weight", "growth_weight", "tgo_weight", "ot_weight"),
    function(x, y) {
      this <- make_wi_rc(exclude_milwaukee = FALSE) |>
        ungroup() |>
        filter(school_year == year) |>
        select(school_year, dpi_true_id, value = x, weight = y) |>
        mutate(score = x)
      # new_weight = ifelse(aw, half_weight / 2, weight))
    }
  ) |>
    group_by(
      school_year,
      dpi_true_id,
      aw = score %in% c("sch_ach", "sch_growth")
    ) |>
    mutate(
      half_weight = sum(weight, na.rm = TRUE) / 2,
      new_weight = ifelse(aw, half_weight, weight),
      nas = sum(is.na(weight))
    ) |>
    ungroup() |>
    mutate(new_weight = ifelse(nas > 0, weight, half_weight)) |>
    arrange(dpi_true_id) |>
    ungroup() |>
    group_by(school_year, dpi_true_id) |>
    summarise(
      cfc_score = weighted.mean(value, new_weight, na.rm = TRUE) |>
        round(1)
    ) |>
    ungroup()

  # DPI cut scores were reset with the 2024-25 report cards and carried forward
  # unchanged in 2025-26, so every year from 2024-25 on uses the new ranges
  if (year >= "2024-25") {
    this |>
      mutate(
        cfc_rating = case_when(
          cfc_score < 49.0 ~ "Fails to Meet Expectations",
          cfc_score < 60.0 ~ "Meets Few Expectations",
          cfc_score < 71.0 ~ "Meets Expectations",
          cfc_score < 84.0 ~ "Exceeds Expectations",
          cfc_score < 100.0 ~ "Significantly Exceeds Expectations",
          TRUE ~ NA
        )
      )
  } else {
    this |>
      mutate(
        cfc_rating = case_when(
          cfc_score < 48.0 ~ "Fails to Meet Expectations",
          cfc_score < 58.0 ~ "Meets Few Expectations",
          cfc_score < 70.0 ~ "Meets Expectations",
          cfc_score < 83.0 ~ "Exceeds Expectations",
          cfc_score < 100.0 ~ "Significantly Exceeds Expectations",
          TRUE ~ NA
        )
      )
  }
})

all <- cfc_scored |>
  right_join(
    make_mke_rc() |>
      filter(school_year %in% these_years) |>
      select(
        school_year,
        dpi_true_id,
        school_name,
        accurate_agency_type,
        broad_agency_type,
        school_enrollment,
        overall_rating,
        overall_score,
        sch_ach,
        sch_growth
      )
  ) |>
  mutate(
    cfc_rating = factor(
      cfc_rating,
      levels = c(
        "Fails to Meet Expectations",
        "Meets Few Expectations",
        "Meets Expectations",
        "Exceeds Expectations",
        "Significantly Exceeds Expectations"
      )
    )
  )

saveRDS(cfc_scored, "data/cfc_scored.rda")


these <- all |>
  # see R/floor_schools.R to finding schools with same ach score as last year
  filter(school_year == "2025-26") |>
  select(
    school_name,
    accurate_agency_type,
    cfc_score,
    cfc_rating,
    overall_score,
    overall_rating,
    school_enrollment,
    sch_ach,
    sch_growth
  ) |>
  arrange(desc(cfc_score))


these |>
  filter(!is.na(cfc_rating)) |>
  group_by(cfc_rating) |>
  summarise(n = n(), total = sum(school_enrollment)) |>
  mutate(
    group = ifelse(cfc_rating %in% lq, "lq", "hq"),
    perc = total / sum(total)
  ) |>
  ggplot(aes(cfc_rating, n)) +
  geom_col(aes(fill = group)) +
  geom_text(
    aes(label = n, color = group),
    nudge_y = 4,
    size = 10,
    fontface = "bold"
  ) +
  scale_x_discrete(labels = label_wrap(width = 20)) +
  labs(x = "CFC Rating", y = "School Count") +
  theme(legend.position = "none")

ggsave("plots/cfc_ratings_bar_graph.png", bg = "transparent")

##########

all |>
  filter(school_year == "2025-26") |>
  group_by(cfc_rating) |>
  summarise(n = n(), total = sum(school_enrollment, na.rm = TRUE))

all |>
  group_by(overall_rating) |>
  summarise(n = n(), total = sum(school_enrollment, na.rm = TRUE))

# PUPS

all |>
  transmute(
    school_year,
    dpi_true_id,
    school_name,
    accurate_agency_type,
    cfc_rating = as.numeric(cfc_rating)
  ) |>
  pivot_wider(names_from = school_year, values_from = cfc_rating) |>
  filter(!is.na(`2023-24`)) |>
  mutate(
    lp = case_when(
      (`2023-24` %in% c(1, 2) | is.na(`2023-24`)) &
        (`2022-23` %in% c(1, 2) | is.na(`2022-23`)) &
        (`2021-22` %in% c(1, 2) | is.na(`2021-22`)) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  mutate(nas = is.na(`2022-23`) + is.na(`2021-22`)) |>
  filter(nas < 2) |>
  group_by(lp) |>
  count() |>
  ungroup() |>
  mutate(perc = n / sum(n))

all |>
  transmute(
    school_year,
    dpi_true_id,
    school_name,
    accurate_agency_type,
    cfc_rating = as.numeric(cfc_rating)
  ) |>
  pivot_wider(names_from = school_year, values_from = cfc_rating) |>
  filter(!is.na(`2023-24`)) |>
  mutate(
    lp = case_when(
      (`2023-24` == 1) &
        (`2022-23` == 1) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  View()

# mutate(nas = is.na(`2022-23`) + is.na(`2021-22`)) |>
# filter(nas < 2) |>
group_by(lp) |>
  count() |>
  ungroup() |>
  mutate(perc = n / sum(n))


all |>
  group_by(school_year, cfc_rating) |>
  summarise(total = sum(school_enrollment, na.rm = TRUE)) |>
  mutate(perc_enr = total / sum(total, na.rm = TRUE)) |>
  filter(!is.na(cfc_rating)) |>
  ggplot(aes(cfc_rating, perc_enr, fill = school_year)) +
  geom_col(position = position_dodge(.5)) +
  scale_x_discrete(label = function(x) str_wrap(x, 15)) +
  scale_y_continuous(label = "0% of\nenrollment", breaks = 0) +
  geom_text(
    aes(label = percent(perc_enr, 1)),
    vjust = 1.5,
    color = "white",
    position = position_dodge(width = .75),
    fontface = "bold",
    size = 5
  ) +
  coord_cartesian(clip = "off") +
  labs(
    y = "",
    x = "Report Card Rating",
    title = "Milwaukee student enrollment by School Report Card rating",
    # title = glue("Comparing <span style='color:{cfc_darkblue}'>**2021-22 school year**</span> ",
    #                 "with <span style='color:{cfc_orange}'>**2023-24 school year**</span> results"),
    fill = "School Year"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(lineheight = 1.1),
    plot.title = element_text(margin = margin(b = 40), size = 16),
    plot.caption = element_text(
      color = "grey50",
      size = 10,
      margin = margin(t = 20)
    ),
    plot.title.position = "plot",
    legend.position = c(.1, .85),
    axis.text.y = element_text(size = 8, hjust = .5)
  )
