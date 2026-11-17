library(tidyverse)
library(wisconsink12)
library(glue)
library(scales)
library(cityforwardcollective)


not <- c(
  "Fails to Meet Expectations",
  "Meets Few Expectations"
)

bat_hq <- make_mke_rc() %>%
  filter(
    school_year == "2024-25" &
      str_detect(overall_rating, "Expectations") &
      accurate_agency_type != "Partnership"
  ) %>%
  mutate(
    hq = ifelse(overall_rating %in% not, FALSE, TRUE),
    sector = case_when(
      broad_agency_type == "District Operated" ~ "MPS",
      broad_agency_type == "Independently Operated" ~ "Public Charter",
      broad_agency_type == "Private" ~ "Private",
      TRUE ~ "WHOOPS!"
    )
  ) %>%
  group_by(sector, hq) %>%
  summarise(
    n = n(),
    total_enr = sum(school_enrollment, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(sector) %>%
  mutate(
    n_perc = n / sum(n),
    perc = total_enr / sum(total_enr),
    perc = ifelse(hq, perc, perc * -1),
    sector = factor(sector, levels = rev(c("MPS", "Public Charter", "Private")))
  )

bat_hq %>%
  ggplot(aes(sector, perc, fill = hq)) +
  geom_col(width = .5) +
  geom_text(
    aes(label = label_percent(1)(abs(perc)), hjust = ifelse(hq, 1.5, -.5)),
    color = "white"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  scale_y_continuous(
    breaks = c(-.8, 0, .8),
    limits = c(-.5, .95),
    labels = c("Not Meeting Expectations", "", "Meeting Expectations")
  ) +
  scale_fill_manual(
    values = c(cfc_orange, cfc_darkblue),
    labels = c("Not Meeting Expectations", "Meeting Expectations")
  ) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50", size = 1.5) +
  coord_flip() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(
      color = "grey50",
      arrow = arrow(length = unit(.3, "cm"), ends = "both", type = "closed")
    ),
    axis.text.x = element_text(hjust = c(0, .5, .9)),
    text = element_text(lineheight = 1.1),
    plot.title = element_text(face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_text(color = "grey30", margin = margin(b = 20)),
    plot.caption = element_text(
      color = "grey50",
      size = 10,
      margin = margin(t = 20)
    ),
    plot.title.position = "plot",
    legend.position = "bottom"
  ) +
  labs(
    y = "",
    x = "",
    fill = "",
    title = "Sector Enrollment in Schools Meeting Expecations",
    subtitle = glue(
      "Over three-quarters of students in private and public charter ",
      "schools are in a school meeting expecations, while less than half ",
      "of students in MPS are in a school meeting expecations."
    ) |>
      str_wrap(90)
  )

ggsave("plots/sector_enr_hq.png", bg = "white")
