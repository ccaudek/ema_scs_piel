# Relational Self-Compassion Scale

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

d <- rio::import(
  # here::here("data", "prep", "quest_scales", "quest_ema_1_prep.csv")
  snakemake@input[["quest_data1"]]
  )

NITEMS <- 16

rscs_items <- d[, c(3, 70:72, 74:86)]  
rscs_items_names <- paste0("rscs_", 1:NITEMS)
rscs_items_names_plus_id <- c("user_id", rscs_items_names)
colnames(rscs_items) <- rscs_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d[, 3], d[, 73])

rio::export(
  rscs_items,
  # here::here("data", "prep", "quest_scales", "rscs_items.csv")
  snakemake@output[["rscs_cols"]]
)

# eof ----

