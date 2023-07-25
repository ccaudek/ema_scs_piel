# Script name: mpath_reliability.R
# Project: EMA mpath 2023
# Script purpose: compute multilevel reliability
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Jun 27 06:51:35 2023
# Last Modified Date: Tue Jun 27 06:51:35 2023
#
# 👉 
#
# https://www.rdocumentation.org/packages/psych/versions/2.3.6/topics/multilevel.reliability
# https://github.com/simsem/semTools/issues/106
# https://github.com/marklhc/mcfa_reliability_supp/blob/master/multilevel_alpha.R
#
# https://github.com/marklhc/mcfa_reliability_supp/blob/master/compare_semTools.md

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(psych)
  library(rio)
  library(semTools)
})

# Piel --------------------------------------------------------------

d_piel <- readRDS(
  "/Users/corrado/_repositories/ema_scs_piel/data/prep/ema/ema_data_2.RDS"
) 
length(unique(d_piel$user_id))

na_items_piel <- d_piel |> 
  dplyr::select(user_id, happy, satisfied, nervous, upset)

# Assuming 'your_df' is your dataframe
na_piel <- na_items_piel %>%
  mutate(across(where(is.numeric), ~ ((. - min(.)) / (max(.) - min(.))) * 100))

na_piel <- na.omit(na_piel)

# # Recode response levels so that they vary between -3 and +3.
# na_items_piel[, sapply(na_items_piel, is.numeric)] <- 
#   na_items_piel[, sapply(na_items_piel, is.numeric)] - 4

# negative affect
na_piel$happy <- na_items_piel$happy * -1
na_piel$satisfied <- na_items_piel$satisfied * -1


# M-path ------------------------------------------------------------

d_mpath <- readRDS(
  here::here(
    "/Users/corrado/_repositories/ema_scs_mpath/data/prep/ema/ema_data_2.RDS"
  )
)   
length(unique(d_mpath$user_id))


na_items_mpath <- d_mpath |> 
  dplyr::select(user_id, happy, satisfied, nervous, upset)

na_mpath <- na.omit(na_items_mpath)


# Combine both data sets ----------------------------------------------------

both_df <- bind_rows(na_piel, na_mpath)

# Reverse positive items

df <- both_df %>%
  mutate(across(c(happy, satisfied), ~ (100 - .)))

cor(df[, 2:5])


# Negative affect -----------------------------------------------------

item_names <- c(
  "person", "i1", "i2", "i3", "i4"
)

colnames(df) <- item_names

# Convert Person to integers.
# scs_items$person_int <- NA
# scs_items$person_int <- match(scs_items$person, unique(scs_items$person))
# scs_items$person <- match(scs_items$person, unique(scs_items$person))


# ------------------------------------------------------------

mcfa11 <- 
'level: 1
     f1 =~ NA * i1 + l1 * i1 + l2 * i2 + l3 * i3 + l4 * i4
     i1 ~~ ev1w * i1
     i2 ~~ ev2w * i2
     i3 ~~ ev3w * i3
     i4 ~~ ev4w * i4
     f1 ~~ 1 * f1

   level: 2
     f1 =~ NA * i1 + l1 * i1 + l2 * i2 + l3 * i3 + l4 * i4
     # fixed residual variances
     i1 ~~ ev1b * i1
     i2 ~~    0 * i2
     i3 ~~ ev3b * i3
     i4 ~~ ev4b * i4
     f1 ~~ vf1b * f1

   # tilde omega values:
   tilomgb := (l1 + l2 + l3 + l4)^2 * vf1b /
              ((l1 + l2 + l3 + l4)^2 * vf1b + ev1b + 0 + ev3b + ev4b)
   tilomgw := (l1 + l2 + l3 + l4)^2 * 1 /
              ((l1 + l2 + l3 + l4)^2 * 1 + ev1w + ev2w + ev3w + ev4w)
   # score reliability:
   omg2l := (l1 + l2 + l3 + l4)^2 * (1 + vf1b) /
            ((l1 + l2 + l3 + l4)^2 * (1 + vf1b) + 
             ev1b + 0 + ev3b + ev4b + ev1w + ev2w + ev3w + ev4w)
   omgb := (l1 + l2 + l3 + l4)^2 * vf1b /
           ((l1 + l2 + l3 + l4)^2 * vf1b + ev1b + 0 + ev3b + ev4b + 
            (ev1w + ev2w + ev3w + ev4w + (l1 + l2 + l3 + l4)^2) / 25.1)
'

mcfa11_fit <- cfa(mcfa11, data = df, cluster = "person")
summary(mcfa11_fit)

comp_rel <- compRelSEM(
  mcfa11_fit,
  obs.var = FALSE,
  config = c("f1"), 
  shared = "f1"
)
comp_rel

# $config
# $config$f1
# omega_W  omega_2L 
# 0.8386613 0.8413634 
# 
# 
# $shared
# $shared$f1
# omega_B       IRR 
# 0.8101592 0.9584794 

# An interrater reliability (IRR) coefficient is also returned, quantifying 
# generalizability across rater/sampling-error only


# eof ----

