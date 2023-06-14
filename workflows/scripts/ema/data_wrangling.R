suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(rio)
  library(psych) # for describing the data
  library(plyr) #for data manipulation
  library(sjPlot) # for model visualization
  library(purrr)
  library(lubridate)
})

options(max.print = .Machine$integer.max)

d <- readRDS(
  here::here(
    "data", "prep", "ema", "ema_data_1.RDS"
  )
)

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

user_id <- unique(mydat$subj_code)
user_id_reference_df <- data.frame(
  user_id = user_id
)

rio::export(
  user_id_reference_df,
  here::here("data", "prep", "ema", "user_id_reference.csv")
)

mydat_clean <- mydat[-which(is.na(mydat$t1)), ]
mydat_clean1 <- mydat_clean[-which(is.na(mydat_clean$t2)), ]
mydat_clean2 <- mydat_clean1[-which(is.na(mydat_clean1$t3)), ]
mydat_clean3 <- mydat_clean2[-which(is.na(mydat_clean2$t5)), ]
mydat_clean4 <- mydat_clean3[-which(is.na(mydat_clean3$t7)), ]

df <- mydat_clean4[-which(is.na(mydat_clean4$t13)), ]

df$subj_code <- factor(df$subj_code)
unique(df$subj_code)

map(mydat, ~sum(is.na(.)))

table(mydat$subj_code)

out <- mydat %>%
  group_by(subj_code) |> 
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(!is.na(.)))) |> 
  as.data.frame()

rowSums(out[, 2:27])





d$date <- date(d$t1)

# Delete wrong days 
d1 <- filter(d, !(date == "2022-03-31") & !(date == "2022-04-21")) %>%
  filter(!(date == "2022-04-28"))

d1$date <- factor(d1$date)
d1$date <- forcats::fct_recode(
  d1$date,
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

# Determine the time window of each administration. The function hour() 
# returns the number of the hour minus 2 (so, I summed + 2)
time_window <- rep(NA, length(d1$t1))
for(i in 1:length(d1$t1)){
  if (hour(d1$t1)[i] + 2 < 12) {
    time_window[i] <- 1
  } else if ((hour(d1$t1)[i] + 2 > 14) & (hour(d1$t1)[i] + 2 < 16)) {
    time_window[i] <- 2
  } else if ((hour(d1$t1)[i] + 2 > 16) & (hour(d1$t1)[i] + 2 < 18)) {
    time_window[i] <- 3
  } else if ((hour(d1$t1)[i] + 2 > 18) & (hour(d1$t1)[i] + 2 < 20)) {
    time_window[i] <- 4
  } else if ((hour(d1$t1)[i] + 2 > 20) & (hour(d1$t1)[i] + 2 < 22)) {
    time_window[i] <- 5
  }  else {
    time_window[i] <- 999
  }
  # print(i)
}

d1$time_window <- time_window

# convert subj_code from string to number 
grpid = function(x) match(x, unique(x))

d1 <- d1 %>% 
  mutate(id = group_indices(., subj_code) %>% grpid)

good_cols <- c(
  "id", "subj_code", "time_window", "date", "context", "nervous", "upset", "satisfied", 
  "happy", "scs_pos_1", "scs_neg_2", "scs_pos_3", "scs_neg_4", "scs_neg_5", 
  "scs_pos_6", "scs_pos_7", "scs_neg_8"
)

d2 <- d1 %>% 
  dplyr::select(all_of(good_cols))

saveRDS(
  d2,
  here::here(
    "data", "prep", "ema", "ema_data_prep.RDS"
  )
)

