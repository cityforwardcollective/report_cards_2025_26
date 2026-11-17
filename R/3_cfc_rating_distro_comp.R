library(tidyverse)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(scales)
library(wisconsink12)
library(cityforwardcollective)
library(glue)
library(ggtext)

cfc_scored <- read_rds("data/cfc_scored.rda")

left_join(make_mke_rc(), cfc_scored) |>
  filter(school_year == "2024-25") |>
  select(school_name, cfc_score, cfc_rating, accurate_agency_type) |>
  arrange(desc(cfc_score)) |>
  View()

not <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations"
)

# percent of students
by_cfc_rating <- cfc_scored |>
  filter(!is.na(cfc_rating)) |>
  left_join(
    make_mke_rc() |>
      select(school_year, dpi_true_id, school_enrollment)
  ) |>
  group_by(school_year, cfc_rating) %>%
  summarise(
    n = n(),
    total_enrollment = sum(school_enrollment, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(school_year) %>%
  mutate(
    perc = n / sum(n),
    perc_enr = total_enrollment / sum(total_enrollment),
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
  ) %>%
  arrange(school_year, cfc_rating)

by_cfc_rating |>
  filter(school_year >= "2023-24") |>
  ggplot(aes(cfc_rating, perc_enr, fill = school_year)) +
  geom_col(position = position_dodge(.5)) +
  scale_x_discrete(label = function(x) str_wrap(x, 15)) +
  scale_y_continuous(label = percent) +
  geom_text(
    aes(
      label = percent(perc_enr, 1),
      vjust = ifelse(as.numeric(cfc_rating) == 5, -2, 2),
      color = ifelse(as.numeric(cfc_rating) == 5, cfc_darkblue, "white")
    ),
    position = position_dodge(width = .5)
  ) +
  scale_color_identity() +
  labs(
    y = "Percent of Student Enrollment",
    x = "CFC Rating",
    title = "CFC Rating distribution of Milwaukee student enrollment",
    fill = "School Year"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 16),
    legend.position = "bottom"
  )

ggsave(
  "plots/mke_enrollment_by_cfc_rating_citywide_24_25.png",
  bg = "white",
  width = 9,
  height = 7
)

# by sector

by_cfc_rating_bat <- cfc_scored |>
  filter(!is.na(cfc_rating)) |>
  left_join(
    make_mke_rc() |>
      select(
        school_year,
        dpi_true_id,
        school_enrollment,
        accurate_agency_type,
        broad_agency_type
      )
  ) |>
  filter(accurate_agency_type != "Partnership") |>
  group_by(school_year, broad_agency_type, cfc_rating) %>%
  summarise(
    n = n(),
    total_enrollment = sum(school_enrollment, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(school_year, broad_agency_type) %>%
  mutate(
    perc = n / sum(n),
    perc_enr = total_enrollment / sum(total_enrollment),
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
  ) %>%
  arrange(school_year, cfc_rating) |>
  filter(school_year > "2022-23") |>
  select(-c(n:perc)) |>
  ungroup() |>
  group_by(
    school_year,
    broad_agency_type,
    meets = ifelse(cfc_rating %in% not, FALSE, TRUE)
  ) |>
  summarise(total_perc = sum(perc_enr)) |>
  filter(meets) |> 
  pivot_wider(names_from = school_year, values_from = total_perc)
