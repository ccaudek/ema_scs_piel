# SCL-90

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

scl90_items <- rio::import(
  here::here("data", "prep", "quest_scales", "scl90_items.csv")
)

# Source scl90.R on GitHub, which includes the function scoring_scl90().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/scl90.R"
)

scl90_subscales <- scoring_scl90(scl90_items)

rio::export(
  scl90_subscales, 
  here::here("data", "prep", "quest_scales", "scl90_scores.csv")
)

# eof ----

