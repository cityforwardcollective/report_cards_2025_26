# Exports the 2025-26 data needed to draft this year's Report Card data brief.
# Run from the project root; writes one CSV to data/.

library(tidyverse)
library(wisconsink12)

make_mke_rc() |>
  filter(school_year >= "2022-23") |>
  write_csv("data/mke_rc_for_brief.csv")
