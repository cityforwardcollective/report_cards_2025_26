library(tidyverse)
library(wisconsink12)

d <- read_rds("data/cfc_scored.rda")

make_mke_schools() |> 
  filter(school_year == "2024-25") |> 
  left_join(d) |> 
  left_join(make_mke_rc() |> 
              select(school_year,
                     dpi_true_id,
                     overall_score,
                     overall_rating)) |> 
  select(school_year,
         dpi_true_id,
         school_name,
         accurate_agency_type,
         cfc_score,
         cfc_rating,
         wi_overall_score = overall_score,
         wi_overall_rating = overall_rating) |> 
  write_csv("data/Milwaukee Schools CFC Scored.csv")
