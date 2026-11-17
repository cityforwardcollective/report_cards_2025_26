# Report Card scores of every Milwaukee school, by sector, 2025-26
#
# One dot per school (a beeswarm), placed by its overall score. Dots are
# shades of blue if the school meets expectations and shades of red if it
# does not, darker toward either end. A dotted line marks the score where
# "Meets Expectations" begins, and the vertical axis is labeled with the
# five rating levels instead of raw scores, so no legend is needed.
#
# Method matches the data brief: Academy of Excellence and partnership
# schools are excluded; only schools with one of the five ratings count.

library(tidyverse)
library(wisconsink12)
library(ggbeeswarm)
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

# lowest overall score for each rating above the first
rating_cuts <- c(49, 60, 71, 84)

# lowest overall score that earns "Meets Expectations"
meets_cut <- 60

# where each rating's name sits on the vertical axis (middle of its band)
rating_axis <- c(
  "Fails to Meet" = 36,
  "Meets Few" = 54.5,
  "Meets" = 65.5,
  "Exceeds" = 77.5,
  "Significantly\nExceeds" = 92
)

# reds below the line, blues above it, darkest at the two extremes
rating_cols <- c(
  "Fails to Meet Expectations" = "#c0392b",
  "Meets Few Expectations" = "#e8a598",
  "Meets Expectations" = "#b8cfea",
  "Exceeds Expectations" = "#7fa8d9",
  "Significantly Exceeds Expectations" = cfc_darkblue
)

# colors for the "Meets" / "Does Not Meet" labels beside the dotted line
meet_cols <- c("TRUE" = cfc_darkblue, "FALSE" = "#c0392b")

rated <- make_mke_rc() |>
  filter(
    school_year == this_year &
      dpi_true_id != "0000_1652" & # Academy of Excellence
      accurate_agency_type != "Partnership"
  ) |>
  mutate(clean_rating = str_remove(overall_rating, "\\^$")) |>
  filter(clean_rating %in% levs & !is.na(overall_score)) |>
  mutate(
    rating = factor(clean_rating, levels = levs),
    meets = overall_score >= meets_cut,
    sector = case_when(
      broad_agency_type == "District Operated" ~ "MPS",
      broad_agency_type == "Independently Operated" ~ "Public Charter",
      broad_agency_type == "Private" ~ "Private",
      TRUE ~ "WHOOPS!"
    ),
    sector = factor(sector, levels = c("MPS", "Public Charter", "Private"))
  )

# schools meeting expectations in each sector, for the smaller line of text
# under each sector name
sector_counts <- rated |>
  group_by(sector) |>
  summarise(
    schools = n(),
    meeting = sum(meets),
    .groups = "drop"
  ) |>
  mutate(
    count_lab = as.character(
      glue("{meeting} of {schools} schools\nmeet expectations")
    )
  )

sector_counts

# bottom of the plotting area; the count text is drawn just below it, under
# the sector names
y_bottom <- 14
y_top <- 99

rated |>
  ggplot(aes(sector, overall_score)) +
  # faint lines between the rating levels, dotted line at Meets Expectations
  geom_hline(
    yintercept = setdiff(rating_cuts, meets_cut),
    color = "grey88",
    linewidth = .4
  ) +
  geom_hline(
    yintercept = meets_cut,
    linetype = "dotted",
    color = "grey40",
    linewidth = .6
  ) +
  geom_beeswarm(
    aes(fill = rating),
    shape = 21,
    color = "white",
    stroke = .4,
    size = 3.2,
    cex = 2.5
  ) +
  # call out the empty space where one-star charter schools would be:
  # a dashed box over the Fails to Meet band of the charter column
  annotate(
    geom = "rect",
    xmin = 1.64,
    xmax = 2.36,
    ymin = 17,
    ymax = 48.2,
    fill = "#b8cfea",
    alpha = .2,
    color = cfc_darkblue,
    linetype = "dashed",
    linewidth = .4
  ) +
  annotate(
    geom = "text",
    x = 2,
    y = 37,
    label = "No one-star\npublic charter\nschools",
    lineheight = .9,
    family = "Trebuchet MS",
    fontface = "bold",
    size = 4.5,
    color = cfc_darkblue
  ) +
  # labels for the two sides of the line, in the right margin so they
  # don't sit on top of any dots; they double as the color key
  annotate(
    geom = "text",
    x = 3.65,
    y = meets_cut + 1.5,
    label = "Meets\nExpectations",
    hjust = 0,
    vjust = 0,
    lineheight = .9,
    family = "Trebuchet MS",
    fontface = "bold",
    size = 4.2,
    color = meet_cols[["TRUE"]]
  ) +
  annotate(
    geom = "text",
    x = 3.65,
    y = meets_cut - 1.5,
    label = "Does Not Meet\nExpectations",
    hjust = 0,
    vjust = 1,
    lineheight = .9,
    family = "Trebuchet MS",
    fontface = "bold",
    size = 4.2,
    color = meet_cols[["FALSE"]]
  ) +
  scale_fill_manual(values = rating_cols, guide = "none") +
  scale_y_continuous(
    breaks = unname(rating_axis),
    labels = names(rating_axis),
    expand = expansion(mult = .01)
  ) +
  scale_x_discrete(expand = expansion(add = .5)) +
  # smaller count text under each sector name (drawn below the plot area)
  annotate(
    geom = "text",
    x = as.numeric(sector_counts$sector),
    y = y_bottom - 7,
    label = sector_counts$count_lab,
    vjust = 1,
    lineheight = .95,
    family = "Trebuchet MS",
    size = 3.7,
    color = "grey40"
  ) +
  coord_cartesian(ylim = c(y_bottom, y_top), clip = "off") +
  labs(
    x = "",
    y = "",
    title = "School Report Card Scores by Sector"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.title.position = "plot",
    plot.margin = margin(10, 115, 42, 10),
    axis.text.x = element_text(size = 14, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(size = 13, lineheight = .9)
  )

ggsave(
  "plots/school_scores_by_sector_25_26.png",
  bg = "white",
  width = 9.5,
  height = 6.5
)
