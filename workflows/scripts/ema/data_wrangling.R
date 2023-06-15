# Script name: data_wrangling.R
# Project: EMA self-compassion piel
# Script purpose: clean data; create `day` and `time_window`.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Jun 15 08:27:27 CEST 2023
# Last Modified Date: Thu Jun 15 08:34:19 CEST 2023
#
# 👉 input:  here("data", "prep", "ema", "ema_data_1.RDS")
#    output: here("data", "prep", "ema", "ema_data_2.RDS")
#            here("data", "prep", "ema", "user_id_reference.csv")

log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("rio")
  library("psych")
  library("sjPlot")
  library("purrr")
  library("lubridate")
  library("naniar")
  library("mice")
})

options(max.print = .Machine$integer.max)

mydat <-
  readRDS(here::here("data", "prep", "ema", "ema_data_1.RDS"))

# Correct subj_code
mydat$subj_code <- tolower(mydat$subj_code)

mydat$subj_code <- forcats::fct_recode(
  mydat$subj_code,
  # codice corretto            codice sbagliato
  "gr_ma_2003_01_10_460_f" = "-gr_ma_2003_01_10_460_f",
  "se_so_2000_01_20_910_f" = "-se_so_2000_01_20_910_f",
  "re_la_1997_08_07_327_f" = "re_la_1997_08_07_327_f-",
  "en_mi_1969_10_31_341_f" = "enri-→-en_mi_1969_10_31_341_f",
  "re_la_1997_08_07_327_f" = "re_la_1997_08_07_327_f-",
  "or_ca_1958_01_16_074_f" = "or_ca_1958_01_16_74_f",
  "bi_tr_2002_07_01_356_f" = "bi_tr_2002_07_01_f",
  "cl_pa_2002_08_18_019_f" = "cl_pa_2002_08_019_f",
  "ga_vi_2002_03_10_308_m" = "ga_vi_2002_03_308_m",
  "fr_vi_1998_02_11_212_f" = "fr_vi_1998_02_212_f",
  "vi_co_2000_12_01_146_f" = "vi_co_2000_01_146_f",
  "fa_sa_1998_01_08_860_f" = "fa_sa_1998_01_08_860_f ",
  "vi_fl_1998_10_17_074_f" = "vi_flo_1998_10_17_074_f",
  "ga_bu_2000_08_06_000_f" = "ga_bu_2000_08_06_f",
  "yu_tr_1994_05_04_676_f" = ""
)

# Save `user_id` names to be used as reference for matching 
# with the questionnaires data.
user_id <- unique(mydat$subj_code)
user_id_reference_df <- data.frame(user_id = user_id)

rio::export(
  user_id_reference_df,
  here::here("data", "prep", "ema", "user_id_reference.csv")
)

# Check for missing data.
if (0) {
  # Are there missing values in the dataset?
  any_na(mydat)
  # How many?
  n_miss(mydat)
  prop_miss(mydat)
  # Which variables are affected?
  mydat %>%
    is.na() %>%
    colSums()
}

# Only 30 rows are deleted.
df <- na.omit(mydat)

df$date <- date(df$t1)

# Dates
# 2022-04-02
# 2022-04-09
# 2022-04-16
# 2022-04-23
# 2022-04-30
# 2022-05-07
# 2022-05-14
# 2022-05-21
# 2022-05-28
# 2022-06-04

# table(df$date)

# Wrong days.
excluded_dates <- c(
  "2022-03-31",
  "2022-04-01",
  "2022-04-04",
  "2022-04-06",
  "2022-04-07",
  "2022-04-11",
  "2022-04-13",
  "2022-04-14",
  "2022-04-18",
  "2022-04-21",
  "2022-04-28",
  "2022-05-05",
  "2022-05-12",
  "2022-05-16",
  "2022-05-19",
  "2022-05-26",
  "2022-06-02",
  "2022-06-11"
)

# Exclude wrong days.
df1 <- df %>%
  filter(!(date %in% excluded_dates))

# table(df1$date)

df1$date <- factor(df1$date)
df1$day <- forcats::fct_recode(
  df1$date,
  "1" = "2022-04-02",
  "2" = "2022-04-09",
  "3" = "2022-04-16",
  "4" = "2022-04-23",
  "5" = "2022-04-30",
  "6" = "2022-05-07",
  "7" = "2022-05-14",
  "8" = "2022-05-21",
  "9" = "2022-05-28",
  "10" = "2022-06-04"
)

# time windows:
# 10:00-10:30, 8:00-8:30   8:00-8:30 AM 10:00-10:30 AM
# 15:00-15:30, 13:00-13:30 1:00-1:30 PM 3:00-3:30 PM
# 17:00-17:30, 15:00-15:30 3:00-3:30 PM 5:00-5:30 PM
# 19:00-19:30, 17:00-17:30 5:00-5:30 PM 7:00-7:30 PM
# 21:00-21:30  19:00-19:30 7:00-7:30 PM 9:00-9:30 PM

time_window <- case_when(
  hour(df1$t1) < 12 ~ 1,
  (hour(df1$t1) > 14) & (hour(df1$t1) <= 16) ~ 2,
  (hour(df1$t1) > 16) & (hour(df1$t1) <= 18) ~ 3,
  (hour(df1$t1) > 18) & (hour(df1$t1) <= 20) ~ 4,
  (hour(df1$t1) > 20) & (hour(df1$t1) <= 22) ~ 5,
  TRUE ~ 999
)
df1$time_window <- time_window

good_cols <- c(
  "subj_code",
  "day",
  "date",
  "time_window",
  "context",
  "nervous",
  "upset",
  "satisfied",
  "happy",
  "scs_pos_1",
  "scs_neg_2",
  "scs_pos_3",
  "scs_neg_4",
  "scs_neg_5",
  "scs_pos_6",
  "scs_pos_7",
  "scs_neg_8"
)

df2 <- df1 %>%
  dplyr::select(good_cols)

# Save cleaned data.
# saveRDS(df2, here::here("data", "prep", "ema", "ema_data_2.RDS"))
saveRDS(df2, snakemake@output[["rds"]])

# eof ----
