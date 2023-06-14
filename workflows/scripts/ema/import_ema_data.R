# Read individual data and save an RDS file.

suppressPackageStartupMessages({
  library(psych) # for describing the data
  library(plyr) # for data manipulation
  library(ggplot2) # for data visualization
  library(sjPlot) # for model visualization
  library(dplyr)
  library(rio)
  library(here)
  library(purrr)
  library(lubridate)
})

dir <- here::here("data", "raw", "piel2022")

file_names <- as.character(list.files(path = dir))
n_files <- length(file_names)

d_list <- list()

for (index_file in 1:n_files) {
  d1 <- read.csv(here("data", "raw", "piel2022", file_names[index_file]), header = FALSE)

  # get subject code
  subj_code <- d1[3, 2]

  # remove first two columns, which contain the times when the app was open and
  # the time when the app was closed.
  d2 <- d1[-(1:3), -c(1, 2)]

  # The responses are stored in the rows 2, 5, 8, ...
  # index_resp is an index that select the rows with the responses.
  index_resp <- seq(2, nrow(d2), by = 3)

  # Each row of d2 (which contains the responses to the 5 items at time t) is
  # saved in the j-th element of the list1 list.
  list1 <- NULL
  j <- 1
  for (i in index_resp) {
    list1[[j]] <- d2[i, ]
    j <- j + 1
  }

  # Convert to data.frame
  df <- do.call(rbind.data.frame, list1)
  # df <- temp %>%
  #   dplyr::select(!V8)

  # Change the names of the columns
  colnames(df) <-
    c(
      "context", "nervous", "upset", "satisfied", "happy",
      "scs_pos_1", "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5", 
      "scs_pos_6", "scs_pos_7", "scs_neg_8"
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
  # df_time <- temp %>%
  #   dplyr::select(!V8)

  colnames(df_time) <-
    c(
      "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11",
      "t12", "t13"
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

saveRDS(
  mydat,
  here::here(
    "data", "prep", "ema", "ema_data_1.RDS"
  )
)

# eof ----

