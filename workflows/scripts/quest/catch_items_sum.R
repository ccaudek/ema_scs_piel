# Analysis of careless responding using the catch items

suppressPackageStartupMessages({
  library("tidyverse")
  library("rio")
})

# Catch items
d <- rio::import(here::here("data", "prep", "quest_scales", "catch_items.csv"))
# d <- rio::import(snakemake@input[["quest_data1"]])

col_names <- paste0("catch_", 1:8)
colnames(d) <- c("user_id", col_names)

# Define the conditions for each column
conditions <- c(
  col1_condition = 1,
  col2_condition = 4,
  col3_condition = 5,
  col4_condition = 1,
  col5_condition = 3,
  col6_condition = 3,
  col7_condition = 2,
  col8_condition = 4
)

# Apply the conditions to each column and calculate the sum
d$condition_met <- apply(d[, 2:9], 1, function(row) {
  sum(ifelse(row > conditions, 1, 0))
})

# user_id for at least 2 outlying responses on the catch items
out <- d |> 
  dplyr::filter(condition_met > 1)
bad_ids_catch_items <- unique(out$user_id)

# Inconsistent responses on the 4 PANAS items
ema_dat <- rio::import(
  here::here("data", "prep", "ema", "ema_data_2.RDS")
)

user_names <- unique(ema_dat$user_id)

avg_cor <- rep(NA, length(user_names))
for (i in 1:length(user_names)) {
  
  foo <- ema_dat |> 
    dplyr::filter(
      user_id == user_names[i]
    )
  r1 <- cor(foo$happy, foo$satisfied)
  r2 <- cor(foo$upset, foo$nervous)
  r3 <- cor(foo$upset, foo$happy)
  r4 <- cor(foo$nervous, foo$satisfied)
  
  avg_cor[i] <- mean(c(r1, r2, abs(r3), abs(r4)))
}

avg_cor_df <- data.frame(
  user_names, avg_cor
)

# Subjects with avg_cor == NA:
# lu_na_1998_08_08_762_f
# da_ma_1999_03_20_644_m
# fi_ca_1992_11_24_582_m ?
# gi_bo_1998_07_08_603_f
# gi_pi_2001_06_27_869_f
# li_pa_1971_06_11_419_f
# gi_to_2002_01_15_962_m
# ir_ro_1999_06_26_855_f

THRESHOLD <- 0.3

temp <- avg_cor_df |> 
  dplyr::filter(avg_cor < THRESHOLD)
bad_ids_panas <- temp$user_names

bad_ids <- c(bad_ids_catch_items, as.character(bad_ids_panas))

df <- data.frame(
  bad_ids
)

rio::export(
  df,
  here("data", "prep", "ema", "bad_ids.csv")
  # snakemake@output[["nates_cols"]]
)

# eof ----
