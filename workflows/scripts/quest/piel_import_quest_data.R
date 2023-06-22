# EMA self-compassion PIEL 2022
# This script import the questionnaires data, removes the duplicate rows,
# and cleans subj_code. Save the cleaned questionnaires data in 
# "quest_ema_1_prep.csv" and "quest_ema_2_prep.csv".

suppressPackageStartupMessages({
  library("rio")
  library("dplyr")
  # Levenshtein distance
  library("stringdist")
})

# q1 <- rio::import(
#   here::here("data", "raw", "quest2022", "quest_ema.xlsx")
# )
q1 <- rio::import(snakemake@input[["quest_data1"]])

# q2 <- rio::import(
#   here::here("data", "raw", "quest2022", "quest_ema2.xlsx")
# )
q2 <- rio::import(snakemake@input[["quest_data2"]])


# Remove first row ("prova")
q1 <- q1[-1, ]

q1[, 3] <- tolower(q1[, 3])
q2[, 3] <- tolower(q2[, 3])

q1[, 3] <- gsub("__", "_", q1[, 3])
q2[, 3] <- gsub("__", "_", q2[, 3])

# Keep rows with distinct IDs
q1 <- q1[!duplicated(q1$`Inserisca il suo codice anonimo (esempio: ma_ro_1997_05_04_174_m)`), ]
q2 <- q2[!duplicated(q2$`Inserisca il suo codice anonimo (esempio: ma_ro_1997_05_04_174_m)`), ]

user_id_q1 <- q1[, 3]
user_id_q2 <- q2[, 3]

# In order to correct the strings `subj_code`, compute the Levenshtein distance
# with respect to the levels of `subj_code` in the user_id_reference.csv
# file, which is used as reference (the reference ids are those used in the EMA).
reference <- rio::import(
  here::here("data", "prep", "ema", "user_id_reference.csv")
)
ref <- unique(reference$user_id)

# Q1 -------------------
# Compute the Levenshtein distance between ref and user_id_q1.
distances <- stringdistmatrix(ref, user_id_q1, method = "lv")

# Find the best match for each string in user_id_q1.
best_matches <- apply(distances, 2, function(x) ref[which.min(x)])

# Find the minimum value in each column
min_value <- apply(distances, 2, min)

# Comparison between the original levels of user_id_q1_u with the best
# matches. The column min_values indicates the distance between the two
# stings expressed as number of letters.
foo <- data.frame(
  user_id_q1, best_matches, min_value
)

foo |> 
  dplyr::filter(min_value > 0)

# Correct subj_code.
q1[, 3] <- forcats::fct_recode(
  q1[, 3],
  # codice corretto            codice sbagliato
  "ca_to_2002_06_18_995_f" = "ca_to_2002_06_18_696_f",
  "lu_te_2002_05_01_691_m" = "lu_te_2002_05_01_691",
  "to_to_2002_06_09_367_m" = "toto2002_06_09_367_m",
  "ma_ro_1997_05_04_174_m" = "ma_ro_1997_05_04_174_m)",
  "al_ro_2001_10_28_111_f" = "ar_ro_2001_10_28_111_f",
  "kr_al_1994_04_06_276-f" = "kr_al_1994_04_06_276_f",
  "lu_lo_1995_07_30_092_m" = "lu_lo_1995_07_30_092_f",
  "en_mi_1969_10_31_341_f" = "en_mi_1969_10_341_f",
  "es_to_1996_04_16_738_f" = "et_to_1996_04_16_738_f",
  "al_fe_1978_06_14_547_f" = "al_fe_1978_06_14_f",
  "fr_la_2000_09_28_698_f" = "fr_la_2000_09_28_698",
  "je_be_1999_12_16_900_f" = "je_be_1999_12_16_767_f",
  "ga_bu_2000_08_06_000_f" = "ga_bu_2000_08_06_f"
)

# Q2 -------------------
# Compute the Levenshtein distance between ref and user_id_q2.
distances <- stringdistmatrix(ref, user_id_q2, method = "lv")

# Find the best match for each string in user_id_q2.
best_matches <- apply(distances, 2, function(x) ref[which.min(x)])

# Find the minimum value in each column
min_value <- apply(distances, 2, min)

# Comparison between the original levels of user_id_q1_u with the best
# matches. The column min_values indicates the distance between the two
# stings expressed as number of letters.
foo <- data.frame(
  user_id_q2, best_matches, min_value
)

foo |> 
  dplyr::filter(min_value > 0)

# Correct subj_code.
q2[, 3] <- forcats::fct_recode(
  q2[, 3],
  # codice corretto            codice sbagliato
  "ca_to_2002_06_18_995_f" = "ca_to_2002_06_18_696_f",
  "de_mi_2002_12_05_800_f" = "de_mi_2002_12_05_008_f",
  "el_lu_2002_04_30_278_f" = "el_lu_30_04_278_f",
  "da_pa_1999_11_10_406_m" = "da_pa_1999_11_10_m",
  "ma_ma_2002_10_19_612_f" = "ma_ma_2002_02_19_815_f"
)

# Change the name of a column
colnames(q1)[3] <- "user_id"
colnames(q2)[3] <- "user_id"

# Keep rows with distinct IDs, because by correcting user_id, some errors were
# introduced.
q1 <- q1[!duplicated(q1$user_id), ]
q2 <- q2[!duplicated(q2$user_id), ] # eating disorders items.

# Save Q1 csv file.
# rio::export(
#   q1,
#   here::here("data", "prep", "quest_scales", "quest_ema_1_prep.csv")
# )
rio::export(
  q1,
  snakemake@output[["quest_data1"]]
)

# Save Q2 csv file.
# rio::export(
#   q2,
#   here::here("data", "prep", "quest_scales", "quest_ema_2_prep.csv")
# )
rio::export(
  q2,
  snakemake@output[["quest_data2"]]
)

# eof ----



