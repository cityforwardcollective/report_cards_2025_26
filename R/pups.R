library(tidyverse)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(scales)
library(wisconsink12)
library(cityforwardcollective)
library(glue)
library(ggtext)
library(sf)

# geos <- read_rds("../southside_enrollment/data/geo_resources_northside.rda")
# 
# geo_school <- geocodes |> 
#   st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
#   st_transform(crs = st_crs(geos$i94))
# 
# these_schools <- st_intersection(geo_school, geos$north_mke) |> 
#   filter(dpi_true_id != "0000_1652")

mke_rc <- make_mke_rc() |>
  filter(dpi_true_id != "0000_1652")

mke_rc |>
  filter(!is.na(overall_score)) |>
  group_by(school_year) |>
  count()

rated_schools <- mke_rc %>%
  ungroup() %>%
  mutate(
    clean_rating = case_when(
      str_detect(overall_rating, "\\^$") ~ str_remove_all(
        overall_rating,
        "\\^$"
      ),
      TRUE ~ overall_rating
    )
  ) %>%
  filter(!is.na(clean_rating))

# last five years
lq_schools <- rated_schools %>%
  filter(school_year >= "2019-20" & !is.na(clean_rating)) %>%
  mutate(
    clean_rating = factor(
      clean_rating,
      levels = c(
        "Fails to Meet Expectations",
        "Meets Few Expectations",
        "Meets Expectations",
        "Exceeds Expectations",
        "Significantly Exceeds Expectations"
      )
    ),
    rating_score = as.numeric(clean_rating),
    one_two = ifelse(rating_score < 3, TRUE, FALSE),
    one = ifelse(rating_score < 2, TRUE, FALSE)
  ) |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         one_two,
         one) |> 
  group_by(dpi_true_id, school_name, accurate_agency_type) |> 
  summarise(one_two_total = sum(one_two),
            one_total = sum(one)) |> 
  arrange(desc(one_total), school_name)

write_csv(lq_schools, "data/cf_support_data/lq_last_five_years.csv")


# one star last three years
one_stars <- rated_schools %>%
  filter(school_year > "2021-22" & !is.na(clean_rating)) %>%
  mutate(
    clean_rating = factor(
      clean_rating,
      levels = c(
        "Fails to Meet Expectations",
        "Meets Few Expectations",
        "Meets Expectations",
        "Exceeds Expectations",
        "Significantly Exceeds Expectations"
      )
    ),
    rating_score = as.numeric(clean_rating),
    one_two = ifelse(rating_score < 2, TRUE, FALSE)
  ) |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         one_two) |> 
  group_by(dpi_true_id, school_name, accurate_agency_type) |> 
  summarise(total = sum(one_two)) |> 
  arrange(desc(total), school_name) |> 
  filter(total == 3)
  
write_csv(one_stars, "data/cf_support_data/one_star_last_three_years.csv")


one_stars_five <- rated_schools %>%
  filter(school_year >= "2019-20" & !is.na(clean_rating)) %>%
  mutate(
    clean_rating = factor(
      clean_rating,
      levels = c(
        "Fails to Meet Expectations",
        "Meets Few Expectations",
        "Meets Expectations",
        "Exceeds Expectations",
        "Significantly Exceeds Expectations"
      )
    ),
    rating_score = as.numeric(clean_rating),
    one_two = ifelse(rating_score < 2, TRUE, FALSE)
  ) |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         one_two) |> 
  group_by(dpi_true_id, school_name, accurate_agency_type) |> 
  summarise(total = sum(one_two)) |> 
  left_join(
    mke_rc |> 
      filter(school_year == "2024-25") |> 
      select(dpi_true_id, 
             school_enrollment)
  ) |> 
  arrange(desc(total), school_name) |> 
  filter(total == 5)

lq_schools |> 
  mutate(all_one_star = ifelse(dpi_true_id %in% one_stars$dpi_true_id, TRUE, FALSE),
         ns_school = ifelse(dpi_true_id %in% these_schools$dpi_true_id, TRUE, FALSE)) |> 
  filter(total == 3) |> 
  arrange(desc(all_one_star), school_name) |> 
  write_csv("data/lq_past_three_years.csv")
