# NEO-FFI-60-NEURO

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
  library("devtools")
})

# neoffi60_neuro_items <- rio::import(
#   here::here("data", "prep", "quest_scales", "neoffi60_neuro_items.csv")
# )
neoffi60_neuro_items <- rio::import(
  snakemake@input[["neoffi60neuro_cols"]]
)

# Source neoffi60_neuro.R on GitHub, which includes the function 
# scoring_neoffi60_neuro().
source_url(
  "https://raw.githubusercontent.com/ccaudek/r_functions/main/neoffi60_neuro.R"
)

neoffi60_neuro_subscale <- scoring_neoffi60_neuro(neoffi60_neuro_items)

# rio::export(
#   neoffi60_neuro_subscales, 
#   here::here("data", "prep", "quest_scales", "neoffi60_neuro_scores.csv")
# )
rio::export(
  neoffi60_neuro_subscale, 
  snakemake@output[["neoffi60neuro_scores"]]
)

# eof ----

