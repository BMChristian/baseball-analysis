library(tidyverse)                     # turn on tidyverse's functions
library(Lahman)                        # turn on Lahman's data tables
library(here)                          # turn on here() for file paths

batting_rates <- function(df) {
  df |>
    group_by(playerID, yearID) |>
    summarise(across(c(AB, H, X2B, X3B, HR, BB, HBP, SF, SH, SO), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") |>
    mutate(
      PA  = AB + BB + HBP + SF + SH,
      X1B = H - X2B - X3B - HR,
      AVG = H / AB,
      OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
      SLG = (X1B + 2 * X2B + 3 * X3B + 4 * HR) / AB,
      OPS = OBP + SLG
    )
}


#na.rm = true -> this skips missing values, if there is a stat that wasnt recorded in older seasons