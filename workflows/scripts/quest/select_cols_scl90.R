# SCL-90

suppressPackageStartupMessages({
  library(tidyverse)
  library(rio)
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest_scales", "quest_ema_1_prep.csv"))

NITEMS <- 90

# Select 21 items
scl90_items <- d[, c(3, 145:231, 233:235)]
scl90_items_names <- paste0("scl90_", 1:NITEMS)
scl90_items_names_plus_id <- c("user_id", scl90_items_names)
colnames(scl90_items) <- scl90_items_names_plus_id

# Define the levels you want to convert to numeric values
levels_to_numeric <- c(
  "Per niente" = 0,
  "Un poco" = 1,
  "Moderatamente" = 2,
  "Molto" = 3,
  "Moltissimo" = 4
)

# Convert catch item to numeric values.
catch_item_scl90 <- 
  apply(data.frame(d[, 232]), 2, function(x) levels_to_numeric[x]) |> 
  as.data.frame()

# Add catch item to catch_items.csv file.
add_catch_item(d[, 3], catch_item_scl90)

temp <- scl90_items[, 2:91]

scl90_items_num <- apply(temp, 2, function(x) levels_to_numeric[x]) |> 
  as.data.frame()

scl90_items_num$user_id <- d[, 3]

scl90_items_num <- scl90_items_num %>% 
  relocate(user_id)

rio::export(
  scl90_items_num,
  here::here("data", "prep", "quest_scales", "scl90_items.csv")
)

# eof ----

