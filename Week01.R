# Week 1: slash lines

# Setup 
library(tidyverse)
library(Lahman)
library(here)

# Combine stints for players in the batting table -> a player traded mid season has one row per team 
# Only looking at 2025 season -> Store the new table as 'Season', look only at 2025, group by the identifiers
# Summarize collapses many rows into one row -> after group by it gives one row per group (in this case one row per player ID)
 
season <- Batting |>
  filter(yearID == 2025) |>
  group_by(playerID, yearID) |>
  summarise(across(c(AB, H, X2B, X3B, HR, BB, HBP, SF, SH, SO), sum),.groups = "drop")

# Adding columns to the data set 'Season' with calculated stats

season <- season |>
  mutate(
    PA = AB + BB + HBP + SF + SH,
    X1B = H - X2B - X3B - HR,
    AVG = H / AB,
    OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
    SLG = (X1B + 2 * X2B + 3 * X3B + 4 * HR) / AB,
    OPS = OBP + SLG
  ) |> 
  filter(PA >= 300)

# Add names to the tidy output -> looks at People table to find FistLast by playerID.  Mutate rounds the stats, /(x) makes each input 'x' and then round those by 3
# Storing the following leaderboards as different variables using the arrange function, doesnt mess up season table but pulls stats from it

ops_leaderboard <- 
  season |>
  left_join(People |> select(playerID, nameFirst, nameLast), by = "playerID") |>
  select(nameFirst, nameLast, PA, AVG, OBP, SLG, OPS) |>
  mutate(across(AVG:OPS, \(x) round(x, 3))) |>
  arrange(desc(OPS))

obp_leaderboard <- 
  season |>
  left_join(People |> select(playerID, nameFirst, nameLast), by = "playerID") |>
  select(nameFirst, nameLast, PA, AVG, OBP, SLG, OPS) |>
  mutate(across(AVG:OPS, \(x) round(x, 3))) |>
  arrange(desc(OBP))

