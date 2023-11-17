# Relational Self-Compassion Scale

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

rscs_items <- rio::import(
  # here::here("data", "prep", "quest_scales", "rscs_items.csv")
  snakemake@input[["rscs_cols"]]
)

# Source scs.R on GitHub, which includes the function scoring_scs().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/rscs.R"
)

rscs_subscales <- scoring_rscs(rscs_items)

rio::export(
  rscs_subscales,
  # here::here("data", "prep", "quest_scales", "rscs_scores.csv")
  snakemake@output[["rscs_scores"]]
)

# eof ----
