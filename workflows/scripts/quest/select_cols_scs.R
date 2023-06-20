# Self-Compassion Scale

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
  library("here")
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest_scales", "quest_ema_1_prep.csv"))

NITEMS <- 26

scs_items <- d[, c(3, 43:61, 63:69)]  
scs_items_names <- paste0("scs_", 1:NITEMS)
scs_items_names_plus_id <- c("user_id", scs_items_names)
colnames(scs_items) <- scs_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d[, 3], d[, 62])

rio::export(
  scs_items,
  here::here("data", "prep", "quest_scales", "scs_items.csv")
)
