# No-attachment to Ego Scale

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest_scales", "quest_ema_1_prep.csv"))

NITEMS <- 7

ego_items <- d[, c(3, 137:138, 140:144)]  

ego_items_names <- paste0("nates_", 1:NITEMS)
ego_items_names_plus_id <- c("user_id", ego_items_names)
colnames(ego_items) <- ego_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d$user_id, d[, 139])

rio::export(
  ego_items,
  here::here("data", "prep", "quest_scales", "nates_items.csv")
)

# eof ----
