# Neuroticism subscale of NEO-FFI-60

suppressPackageStartupMessages({
  library(tidyverse)
  library(rio)
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest_scales", "quest_ema_1_prep.csv"))

NITEMS <- 12

# Select 21 items
neuroticism_items <- d[, c(3, 87:92, 94:99)]
neuroticism_items_names <- paste0("neuroticism_", 1:NITEMS)
neuroticism_items_names_plus_id <- c("user_id", neuroticism_items_names)

colnames(neuroticism_items) <- neuroticism_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d[, 3], d[, 93])

rio::export(
  neuroticism_items,
  here::here("data", "prep", "quest_scales", "neoffi60_neuro_items.csv")
)

# eof ----

