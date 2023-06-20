# Script name: import_ema_data.R
# Project: EMA self-compassion piel
# Script purpose: Read individual EMA data files and save an RDS file.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Jun 15 04:48:21 CEST 2023
# Last Modified Date: Thu Jun 15 04:54:41 CEST 2023
#
# 👉 input:  here("data", "raw", "piel2022")
#    output: here("data", "prep", "ema", "ema_data_1.RDS")

log <- file(snakemake@log[[1]], open="wt")
sink(log)
sink(log, type="message")

suppressPackageStartupMessages({
  library(psych)
  library(ggplot2)
  library(sjPlot)
  library(dplyr)
  library(rio)
  library(here)
  library(purrr)
  library(lubridate)
})

# All individual EMA files are stored in this directory.
dir <- here::here("data", "raw", "piel2022")

file_names <- list.files(path = dir, full.names = TRUE)
file_names <- as.character(list.files(path = dir))
n_files <- length(file_names)

d_list <- list()

for (index_file in 1:n_files) {
  d1 <-
    read.csv(
      here("data", "raw", "piel2022", file_names[index_file]),
      header = FALSE
    )

  # get subject code
  subj_code <- d1[3, 2]

  # remove first two columns, which contain the times when the app
  # was open and the time when the app was closed.
  d2 <- d1[-(1:3), -c(1, 2)]

  # The responses are stored in the rows 2, 5, 8, ...
  # index_resp is an index that select the rows with the responses.
  index_resp <- seq(2, nrow(d2), by = 3)

  # Each row of d2 (which contains the responses to the 5 items at
  # time t) is saved in the j-th element of the list1 list.
  list1 <- NULL
  j <- 1
  for (i in index_resp) {
    list1[[j]] <- d2[i, ]
    j <- j + 1
  }

  # Convert to data.frame
  df <- do.call(rbind.data.frame, list1)

  # Change the names of the columns
  colnames(df) <-
    c(
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

  # Add subj_code.
  df$subj_code <- subj_code

  # The same for the times of the responses (rows 3, 6, 9, ...)
  index_time <- seq(3, nrow(d2), by = 3)
  list_time <- NULL
  j <- 1
  for (i in index_time) {
    list_time[[j]] <- d2[i, ]
    j <- j + 1
  }
  df_time <- do.call(rbind.data.frame, list_time)

  colnames(df_time) <-
    c(
      "t1",
      "t2",
      "t3",
      "t4",
      "t5",
      "t6",
      "t7",
      "t8",
      "t9",
      "t10",
      "t11",
      "t12",
      "t13"
    )

  dat_time <- df_time %>%
    purrr::map_dfr(ymd_hms, tz = "Europe/Rome")
  # force_tz(dat_time, tzone = "GMT")

  d <- cbind(df, dat_time)

  d_list[[index_file]] <- d
}

mydat <- do.call(rbind.data.frame, d_list)

mydat[, 1:13] <- mydat[, 1:13] %>%
  purrr::map_dfr(as.numeric)

# Save complete raw data
# here::here("data", "prep", "ema", "ema_data_1.RDS")
saveRDS(mydat, snakemake@output[["rds"]])

# eof ----