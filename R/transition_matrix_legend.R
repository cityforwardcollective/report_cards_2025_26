library(geomtextpath)
library(tidyverse)
library(cityforwardcollective)


expand.grid(
  from_rating_numeric = 1:5,
  to_rating_numeric = 1:5
) |> 
  mutate(
    change = to_rating_numeric - from_rating_numeric,
    change_direction = case_when(
      from_rating_numeric == to_rating_numeric ~ "Same",
      from_rating_numeric < to_rating_numeric ~ "Improved",
      from_rating_numeric > to_rating_numeric ~ "Declined"
    ),
    from_rating_numeric = factor(
      from_rating_numeric,
      levels = c("1", "2", "3", "4", "5")
    ),
    to_rating_numeric = factor(
      to_rating_numeric,
      levels = c("1", "2", "3", "4", "5")
    ),
    plot_order = case_when(
      change_direction == "Same" ~ "a",
      change_direction == "Declined" ~ "b",
      change_direction == "Improved" ~ "c",
      TRUE ~ "ERROR"
    )
  ) |> 
  arrange(plot_order) |> 
  ggplot(aes(x = from_rating_numeric, y = to_rating_numeric)) +
  geom_tile(aes(fill = change_direction,
                alpha = abs(change),
                color = change_direction),
            linewidth = .5)   +
  geom_labelsegment(data = tibble(
    x = c(c(1:4, 4.75), rep(1, 3), .75), xend = c(rep(5, 4), 5.25, c(4:2, 1.25)), 
    y = c(rep(1, 4), 0.75, c(2:4, 4.75)), yend = c(c(5:2), 1.25, rep(5, 3), 5.25), 
    label = c(
      "No change in rating",
      "-1",
      "-2",
      "-3",
      "-4",
      "+1",
      "+2",
      "+3",
      "+4"
      )
  ), aes(x = x, xend = xend, y = y, yend = yend, label = label),
  arrow = arrow(length = unit(3, "mm"), ends = "both", type = "closed"),
  fill = alpha("grey95"), size = 5, label.r = .05, linewidth = .25) +
  scale_fill_manual(
    values = c("Declined" = "red", "Same" = "gray", "Improved" = "blue"),
    guide = "none"
  ) +
  scale_color_manual(
    values = c("Declined" = "red", "Same" = "gray", "Improved" = "blue"),
    guide = "none"
  ) +
  scale_alpha_continuous(guide = "none", limits = c(0, 6)) +
  theme(aspect.ratio = 1,
        panel.grid = element_blank(),
        text = element_text(family = "Open Sans")) +
  labs(x = "Last Year's Rating", y = "This Year's Rating")
