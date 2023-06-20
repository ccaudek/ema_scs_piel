# Rosenberg

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("rio")
})

d <- rio::import(here::here("data", "prep", "quest_scales", "quest_ema_1_prep.csv"))
# d <- rio::import(snakemake@input[["quest"]])

NITEMS <- 10

rosenberg_items <- d[, c(3, 10:16, 18:20)]  

rosenberg_items_names <- paste0("ros_", 1:NITEMS)
rosenberg_items_names_plus_id <- c("user_id", rosenberg_items_names)
colnames(rosenberg_items) <- rosenberg_items_names_plus_id

# Add catch item to catch_items.csv file.
add_catch_item(d[, 3], d[, 17])

# Levels to convert to numeric values
levels_to_numeric <- c(
  "Fortemente in disaccordo" = 1,
  "In disaccordo" = 2,
  "D'accordo" = 3,
  "Fortemente d'accordo" = 4
)

temp <- rosenberg_items[, 2:11]

rosenberg_items_num <- apply(temp, 2, function(x) levels_to_numeric[x]) |> 
  as.data.frame()

rosenberg_items_num$user_id <- d[, 3]

rosenberg_items_num <- rosenberg_items_num %>% 
  relocate(user_id)

rio::export(
  rosenberg_items_num, 
  here::here("data", "prep", "quest_scales", "rosenberg_items.csv")
  # snakemake@output[["csv"]]
)

