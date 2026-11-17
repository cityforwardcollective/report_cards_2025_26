library(tidyverse)
library(wisconsink12)

d <- read_rds("data/cfc_scored.rda")

dd <- d |> 
  left_join(schools |> 
              select(school_year,
                     dpi_true_id,
                     school_name,
                     accurate_agency_type,
                     broad_agency_type,
                     milwaukee_indicator))

mke <- dd |> 
  filter(milwaukee_indicator &
           school_year == "2024-25" &
           (broad_agency_type == "Independently Operated" |
              accurate_agency_type == "Instrumentality Charter")) |> 
  arrange(school_name)

ssm <- read_rds("../similar_schools_metric/data/wi_ssm_ratings_2024_25.rda")

mke_ssm <- ssm |> 
  left_join(schools |> 
              select(school_year,
                     dpi_true_id,
                     school_name,
                     accurate_agency_type,
                     broad_agency_type,
                     milwaukee_indicator)) |> 
  filter(milwaukee_indicator &
           school_year == "2024-25" &
           (broad_agency_type == "Independently Operated" |
              accurate_agency_type == "Instrumentality Charter")) |> 
  arrange(school_name)

all <- mke |> 
  left_join(mke_ssm |> 
              select(dpi_true_id,
                     ssm_score,
                     ssm_rating)) |> 
  left_join(make_mke_rc() |> 
              select(school_year, 
                     dpi_true_id,
                     per_ed,
                     per_swd,
                     per_lep)) |> 
  select(dpi_true_id,
    school_name,
         cfc_rating,
         ssm_rating,
         cfc_score,
         ssm_score,
         per_ed,
         per_swd,
         per_lep,
         accurate_agency_type)

all |> 
  mutate(match = case_when(
    cfc_rating %in% c("Fails to Meet Expectations", "Meets Few Expectations") &
      ssm_rating %in% c("Well Below Average", "Below Average") ~ TRUE,
    cfc_rating == "Meets Expectations" &
      ssm_rating == "Average" ~ TRUE,
    cfc_rating %in% c("Exceeds Expectations", "Significantly Exceeds Expectatiosn") &
      ssm_rating %in% c("Above Average", "Well Above Average") ~ TRUE,
    is.na(cfc_rating) + is.na(ssm_rating) > 0 ~ NA,
    TRUE ~ FALSE
  )) |> View()

# enrollment

all |> 
  left_join(enrollment |> 
              filter(group_by == "All Students")) |> 
  select(school_year,
         dpi_true_id,
         school_name,
         student_count) |> 
  pivot_wider(names_from = school_year, values_from = student_count) |> 
  View()
