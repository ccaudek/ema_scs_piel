# Difficulty in Emotion Regulation Strategies (DERS)

suppressPackageStartupMessages({
  library(tidyverse)
  library(rio)
})

source(here::here("workflows", "scripts", "quest", "funs", "funs_quest.R"))

d <- rio::import(here::here("data", "prep", "quest_scales", "quest_ema_1_prep.csv"))

NITEMS <- 36

ders_items <- d[, c(3, 100:131, 133:136)]
ders_items_names <- paste0("ders_", 1:NITEMS)
ders_items_names_plus_id <- c("user_id", ders_items_names)

colnames(ders_items) <- ders_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d[, 3], d[, 132])

# Define the levels you want to convert to numeric values
levels_to_numeric <- c(
  "Quasi mai" = 1,
  "A volte" = 2,
  "Circa la metà delle volte" = 3,
  "Molte volte"  = 4,
  "Quasi sempre" = 5
)

temp <- ders_items[, 2:37]

ders_items_num <- apply(temp, 2, function(x) levels_to_numeric[x]) |> 
  as.data.frame()

ders_items_num$user_id <- d[, 3]

ders_items_num <- ders_items_num %>% 
  relocate(user_id)

rio::export(
  ders_items_num,
  here::here("data", "prep", "quest_scales", "ders_items.csv")
)

# eof ----
