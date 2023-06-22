# DERS

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

# ders_items <- rio::import(
#   here::here("data", "prep", "quest_scales", "ders_items.csv")
# )

ders_items <- rio::import(
  snakemake@input[["ders_cols"]]
)

# Source ders.R on GitHub, which includes the function scoring_ders().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/ders.R"
)

ders_subscales <- scoring_ders(ders_items)

# rio::export(
#   ders_subscales, 
#   here::here("data", "prep", "quest_scales", "ders_subscales_scores.csv")
# )

rio::export(
  ders_subscales, 
  snakemake@output[["ders_scores"]]
)

# eof ----

