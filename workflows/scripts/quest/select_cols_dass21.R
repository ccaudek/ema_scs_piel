# DASS-21

suppressPackageStartupMessages({
  library(tidyverse)
  library(rio)
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest_scales", "quest_ema_1_prep.csv"))

NITEMS <- 21

# Select 21 items
dass21_items <- d[, c(3, 21:35, 37:42)]
dass21_items_names <- paste0("dass21_", 1:NITEMS)
dass21_items_names_plus_id <- c("user_id", dass21_items_names)

colnames(dass21_items) <- dass21_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d[, 3], d[, 36])

# Define the levels you want to convert to numeric values
levels_to_numeric <- c(
  "Non mi è mai accaduto" = 0,
  "Mi è capitato qualche volta" = 1,
  "Mi è capitato con una certa frequenza" = 2,
  "Mi è capitato quasi sempre" = 3
)

temp <- dass21_items[, 2:22]

dass21_items_num <- apply(temp, 2, function(x) levels_to_numeric[x]) |> 
  as.data.frame()

dass21_items_num$user_id <- d[, 3]

dass21_items_num <- dass21_items_num %>% 
  relocate(user_id)

rio::export(
  dass21_items_num,
  here::here("data", "prep", "quest_scales", "dass21_items.csv")
)

# eof ----

