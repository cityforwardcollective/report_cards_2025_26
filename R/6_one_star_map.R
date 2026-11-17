# Heat map of where Milwaukee's one-star students go to school, 2025-26
#
# Shows where one-star schools are concentrated, with no split by sector.
# Each school is a dot; the shading is a smoothed density of those schools,
# with every school counting the same. Method matches the data brief: Academy of
# Excellence and partnership schools are excluded.

library(tidyverse)
library(wisconsink12)
library(sf)
library(cityforwardcollective)

this_year <- "2025-26"

# Smoothing distance in meters. Larger = broader, softer hot spots.
bandwidth <- 6000

# FALSE: every one-star school counts the same, so the shading shows where
# one-star schools cluster. TRUE: schools count in proportion to enrollment,
# which lets a few large high schools create a hot spot on their own.
weight_by_enrollment <- FALSE

# projected coordinate system in meters (UTM zone 16N), so distances are
# the same north-south and east-west
map_crs <- 26916

one_star <- make_mke_rc() |>
  filter(
    school_year == this_year &
      dpi_true_id != "0000_1652" & # Academy of Excellence
      accurate_agency_type != "Partnership" &
      str_remove(overall_rating, "\\^$") == "Fails to Meet Expectations"
  ) |>
  left_join(geocodes |> select(-school_year))

# check: any schools that could not be placed on the map
one_star |>
  filter(is.na(lat) | is.na(long) | is.na(school_enrollment)) |>
  select(school_name, school_enrollment, lat, long)

mke <- tigris::places("WI") |>
  filter(NAME == "Milwaukee") |>
  st_transform(map_crs)

one_star_sf <- one_star |>
  filter(!is.na(lat) & !is.na(long)) |>
  mutate(
    # a school with no enrollment figure is given the smallest enrollment
    # so it still appears on the map
    map_enr = coalesce(school_enrollment, min(school_enrollment, na.rm = TRUE))
  ) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  st_transform(map_crs)

# points the shading is built from: one per school, or one per 10 students
# when weighting by enrollment
students <- one_star_sf |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2],
    copies = if (weight_by_enrollment) pmax(round(map_enr / 10), 1) else 1
  ) |>
  st_drop_geometry() |>
  uncount(copies)

# area the shading is calculated over: the whole city plus a margin, so it
# is not limited to the box around the schools
shade_box <- st_bbox(st_buffer(mke, 5000))

density <- MASS::kde2d(
  students$x,
  students$y,
  h = c(bandwidth, bandwidth),
  n = 300,
  lims = c(
    shade_box[["xmin"]],
    shade_box[["xmax"]],
    shade_box[["ymin"]],
    shade_box[["ymax"]]
  )
)

density_grid <- expand_grid(y = density$y, x = density$x) |>
  mutate(z = as.vector(density$z))

# everything outside the city limits, used to hide shading that spills over
outside_city <- st_difference(
  st_as_sfc(shade_box),
  st_union(mke)
)

city_box <- st_bbox(mke)

# Note explaining the dots. It sits in the white space to the upper right of
# the city and points, with a horizontal arrow, at the more easterly of the
# two northernmost one-star schools. note_gap (meters) is how far to the
# right of that school the text starts.
note_gap <- 3000

dot_coords <- as_tibble(st_coordinates(one_star_sf))
note_target <- dot_coords |>
  slice_max(Y, n = 2) |>
  slice_max(X, n = 1, with_ties = FALSE)
note_x <- note_target$X + note_gap
note_y <- note_target$Y

ggplot() +
  # city first so the plot covers the whole city, not just where schools are
  geom_sf(data = mke, fill = "white", color = NA) +
  geom_contour_filled(
    data = density_grid,
    aes(x, y, z = z),
    bins = 8,
    show.legend = FALSE
  ) +
  geom_sf(data = outside_city, fill = "white", color = NA) +
  geom_sf(data = mke, fill = NA, color = "grey50", linewidth = .2) +
  # one dot per school, all the same size
  geom_sf(
    data = one_star_sf,
    shape = 21,
    fill = "grey15",
    color = "white",
    stroke = .3,
    size = 1.4,
    alpha = .65
  ) +
  # note with a horizontal arrow to one of the dots
  annotate(
    geom = "text",
    x = note_x,
    y = note_y,
    label = "Each dot is a\none-star school",
    hjust = 0,
    family = "Trebuchet MS",
    size = 3.5,
    lineheight = .95,
    color = "grey30"
  ) +
  annotate(
    geom = "segment",
    x = note_x - 300,
    y = note_y,
    xend = note_target$X + 400,
    yend = note_y,
    color = "grey40",
    linewidth = .3,
    arrow = arrow(length = unit(1.5, "mm"), type = "closed")
  ) +
  scale_fill_brewer(palette = "Reds") +
  # crop to the city limits (plus a small margin) so the shading area
  # around the city doesn't leave empty space
  coord_sf(
    crs = map_crs,
    datum = NA,
    xlim = c(city_box[["xmin"]] - 300, city_box[["xmax"]] + 300),
    ylim = c(city_box[["ymin"]] - 300, city_box[["ymax"]] + 300),
    expand = FALSE
  ) +
  labs(title = "Milwaukee One-Star Schools") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.title.position = "plot",
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "plots/one_star_map_25_26.png",
  bg = "white",
  width = 5.5,
  height = 9.5
)
