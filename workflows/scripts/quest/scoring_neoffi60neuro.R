# DASS-21

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

neoffi60_neuro_items <- rio::import(
  here::here("data", "prep", "quest_scales", "neoffi60_neuro_items.csv")
)

# Source neoffi60_neuro.R on GitHub, which includes the function neoffi60_neuro().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/neoffi60_neuro.R"
)

neoffi60_neuro_subscales <- scoring_neoffi60_neuro(neoffi60_neuro_items)

rio::export(
  neoffi60_neuro_subscales, 
  here::here("data", "prep", "quest_scales", "neoffi60_neuro_subscale_scores.csv")
)

# eof ----

