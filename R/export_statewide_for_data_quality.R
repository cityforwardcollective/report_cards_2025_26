# Exports statewide report card data, all years, so the data quality check
# (missing or implausible enrollment and demographic fields) can be run for
# every Wisconsin school, not only Milwaukee.
# Run from the project root; writes one CSV to data/.

library(tidyverse)
library(wisconsink12)

make_wi_rc(exclude_milwaukee = FALSE) |>
  write_csv("data/wi_rc_all_years.csv")
