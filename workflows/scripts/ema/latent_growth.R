#' Script name: latent_growth.R
#' Project: ema self-compassion piel
#' Script purpose: use LGM approach to test the bipolar hypothesis
#' @author: Corrado Caudek <corrado.caudek@unifi.it>
#' Date Created: Sat Jul 29 08:39:43 2023
#' Last Modified Date: Sat Jul 29 08:39:43 2023
#'
#' 👉 
#' The problem with an ANCOVA model for testing the difference in slopes of
#' the effect of negative affect on positive and negative state self-compassion
#' is that, by assuming that nonlinear transformations are admissible for 
#' self-compassion, the interactions are meaningless. In alternative, I try to 
#' build a SEM model with ordinal self-compassion items and test the bipolar
#' continuum hypothesis within this framework.


suppressPackageStartupMessages({
  library("lavaan")
  library("semPlot")
  library("knitr")
  library("markdown")
  library("patchwork")
  library("psych")
  library("DT")
  library("kableExtra")
  library("lme4")
  library("lcsm")
  library("tidyr")
  library("stringr")
})
set.seed(12345)

# The data are in piel_data.

# First problem: select only participants with a low number of missing values.

# Select the first 8 data points (days)
temp <- piel_data |> 
  dplyr::filter(bysubj_day < 9)

# First, group the data by user_id
grouped_df <- temp %>%
  group_by(user_id) %>%
  # Next, calculate the number of unique days per subject
  summarise(num_days = n_distinct(bysubj_day)) %>%
  # Finally, filter subjects where the maximum number of days is at least 5
  filter(num_days >= 4)

# Now, you have a new data frame called "grouped_df" containing subjects with at least 5 days.
# If you need to use this filtered list of user_ids to subset the original dataframe:
filtered_subjects <- grouped_df$user_id

# To get the rows in the original dataframe corresponding to the filtered subjects:
selected_df <- temp %>%
  filter(user_id %in% filtered_subjects)

selected_df |> 
  group_by(user_id) |> 
  summarize(
    n = max(bysubj_day)
  ) |> 
  as.data.frame()

# remove rows for each user_id where there are less than 4 unique levels of 
# time_window for any single level of date.
d1 <- selected_df %>%
  group_by(user_id, date) %>%
  filter(n_distinct(time_window) >= 2) %>%
  ungroup()

unique(d1$neg_aff) |> sort()
# [1] -8 -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8

d1$neg_aff_fact <- factor(d1$neg_aff)

d1 <- d1 |> 
  group_by(user_id) |> 
  mutate(
    sc_pos = scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7,
    sc_neg = scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8 
  ) |> 
  ungroup()

d2 <- d1 |> 
  group_by(user_id, neg_aff_fact) |> 
  summarise(
    sc_pos = mean(sc_pos),
    sc_neg = mean(sc_neg)
  ) |> 
  ungroup()


d3 <- d2 %>%
  pivot_wider(names_from = neg_aff_fact, values_from = c(sc_pos, sc_neg))

desired_order <- c(
  "user_id",   "sc_pos_-8", "sc_pos_-7", "sc_pos_-6", "sc_pos_-5", "sc_pos_-4", 
  "sc_pos_-3", "sc_pos_-2", "sc_pos_-1", "sc_pos_0",  "sc_pos_1", "sc_pos_2",
  "sc_pos_3",  "sc_pos_4", "sc_pos_5",  "sc_pos_6",  "sc_pos_7", "sc_pos_8",
  "sc_neg_-8", "sc_neg_-7", "sc_neg_-6", "sc_neg_-5", "sc_neg_-4", "sc_neg_-3", 
  "sc_neg_-2", "sc_neg_-1", "sc_neg_0",  "sc_neg_1",  "sc_neg_2", "sc_neg_3", 
  "sc_neg_4",  "sc_neg_5",  "sc_neg_6",  "sc_neg_7", "sc_neg_8"
)

# Rearrange the columns based on the order in the "desired_order" vector
d3 <- d3 %>%
  select(all_of(desired_order))

new_names <- 
  c("user_id", paste0("pos_", seq(1, 17)), paste0("neg_", seq(1, 17)))

colnames(d3) <- new_names


x_var_list <- paste0("pos_", seq(1, 17))
y_var_list <- paste0("neg_", seq(1, 17))

# psych::describe(d3)

plot_trajectories(
  data = d3,
  id_var = "user_id",
  var_list = y_var_list,
  xlab = "Time", ylab = "Value",
  connect_missing = FALSE,
  # random_sample_frac = 0.018,
  title_n = TRUE
)


# Imputation
# library(mlim)
# MLIM <- mlim(d3, m=1, seed = 2022, tuning_time = 180) 




# new_df <- d3 %>%
#   mutate(
#     na1 = rowMeans(select(., 2:3)),
#     na2 = rowMeans(select(., 4:5)),
#     na3 = rowMeans(select(., 6:7)),
#     na4 = rowMeans(select(., 8:9)),
#     na5 = rowMeans(select(., 10:11)),
#     na6 = rowMeans(select(., 12:13)),
#     na7 = rowMeans(select(., 14:15)),
#     na8 = rowMeans(select(., 16:19))
#   ) %>%
#   select(user_id, na1, na2, na3, na4, na5, na6, na7, na8)
# 
# 
# numeric_vars <- new_df[, sapply(new_df, is.numeric)]
# imputed_data <- mice(numeric_vars, method = "pmm", m = 1)
# # Step 4: Extract the imputed data
# imputed_df <- complete(imputed_data)
# imputed_df$user_id <- new_df$user_id
# 
# numeric_vars <- d3[, sapply(d3, is.numeric)]
# imputed_data <- mice(numeric_vars, method = "pmm", m = 1)
# # Step 4: Extract the imputed data
# imputed_df <- complete(imputed_data)
# imputed_df$user_id <- new_df$user_id
# 
# 
# plot_trajectories(
#   data = imputed_df,
#   id_var = "user_id",
#   var_list = paste0("pos_", seq(1, 17)),
#   xlab = "Time", ylab = "Value",
#   connect_missing = FALSE,
#   # random_sample_frac = 0.018,
#   title_n = TRUE
# )


lg_model <- '
  # latent variable definitions
      # intercept 
      eta_1 =~ 1*pos_1
      eta_1 =~ 1*pos_2
      eta_1 =~ 1*pos_3
      eta_1 =~ 1*pos_4
      eta_1 =~ 1*pos_5
      eta_1 =~ 1*pos_6
      eta_1 =~ 1*pos_7
      eta_1 =~ 1*pos_8
      eta_1 =~ 1*pos_9
      eta_1 =~ 1*pos_10
      eta_1 =~ 1*pos_11
      eta_1 =~ 1*pos_12
      eta_1 =~ 1*pos_13
      eta_1 =~ 1*pos_14
      eta_1 =~ 1*pos_15
      eta_1 =~ 1*pos_16
      eta_1 =~ 1*pos_17

      # linear slope 
      eta_2 =~ 0*pos_1
      eta_2 =~ 1*pos_2
      eta_2 =~ 2*pos_3
      eta_2 =~ 3*pos_4
      eta_2 =~ 4*pos_5
      eta_2 =~ 5*pos_6
      eta_2 =~ 6*pos_7
      eta_2 =~ 7*pos_8
      eta_2 =~ 8*pos_9
      eta_2 =~ 9*pos_10
      eta_2 =~ 10*pos_11
      eta_2 =~ 11*pos_12
      eta_2 =~ 12*pos_13
      eta_2 =~ 13*pos_14
      eta_2 =~ 14*pos_15
      eta_2 =~ 15*pos_16
      eta_2 =~ 16*pos_17

  # factor variances
      eta_1 ~~ eta_1
      eta_2 ~~ eta_2

  # covariances among factors 
      eta_1 ~~ eta_2

  # factor means 
      eta_1 ~ 1
      eta_2 ~ 1

  # manifest variances (made equivalent by naming theta)
 

  # manifest means (fixed at zero)
      pos_1 ~ 0*1
      pos_2 ~ 0*1
      pos_3 ~ 0*1
      pos_4 ~ 0*1
      pos_5 ~ 0*1
      pos_6 ~ 0*1
      pos_7 ~ 0*1
      pos_8 ~ 0*1
      pos_9 ~ 0*1
      pos_10 ~ 0*1
      pos_11 ~ 0*1
      pos_12 ~ 0*1
      pos_13 ~ 0*1
      pos_14 ~ 0*1
      pos_15 ~ 0*1
      pos_16 ~ 0*1
      pos_17 ~ 0*1
' #end of model definition

# ------------------------------------------------------------------------------
# Remove subjects with a shallow slope. The results are the same.

# Assuming your data frame is named 'd3', first, reshape it to long format
d3_long <- d3 %>%
  pivot_longer(cols = starts_with("pos_"), names_to = "position", values_to = "value") %>%
  mutate(position = as.integer(str_extract(position, "\\d+")))  # Extract position number as integer

# Group by 'user_id'
grouped_data <- d3_long %>%
  group_by(user_id)

# Fit a linear regression model for each group and extract the slope
slope_data <- grouped_data %>%
  summarize(slope = lm(value ~ position)$coefficients[2])

# Filter the levels of 'user_id' where the slope is 0 or negative
user_id_with_negative_slope <- slope_data %>%
  filter(slope <= 0.2) %>%
  pull(user_id)

new_dat <- d3[d3$user_id %in% user_id_with_negative_slope, ]

my_data_complete <- lavaan::lavDataFull(d3)


# ------------------------------------------------------------------------------

lg_fit <- sem(lg_model,
  data = d3,
  meanstructure = TRUE,
  estimator = "MLR",
  missing = "ML"
)

fitMeasures(lg_fit, c("chisq", "df", "pvalue", "cfi", "tli", "srmr", "rmsea"))

summary(lg_fit, fit.measures = TRUE, standardized = TRUE) |>
  print()

semPaths(lg_fit,what = "path", whatLabels = "par")

# ------------------------------------------------------------------------------

model <- ' i =~ 1*pos_1 + 1*pos_2 + 1*pos_3 + 1*pos_4 + 1*pos_5 + 1*pos_6 + 
                1*pos_7 + 1*pos_8 + 1*pos_9 + 1*pos_10 + 1*pos_11 + 1*pos_12 +
                1*pos_13 + 1*pos_14 + 1*pos_15 + 1*pos_16 + 1*pos_17
           s =~ 0*pos_1 + 1*pos_2 + 2*pos_3 + 3*pos_4 + 4*pos_5 + 5*pos_6 + 
                6*pos_7 + 7*pos_8 + 8*pos_9 + 9*pos_10 + 10*pos_11 + 11*pos_12 +
                12*pos_13 + 13*pos_14 + 14*pos_15 + 15*pos_16 + 16*pos_17
'
fit <- growth(model, data=d3, missing = "ML")
fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "srmr", "rmsea"))


# ------------------------------------------------------------------------------
# It makes things worse!!

# Step 1: Handle missing data (impute using mean)
d3_imputed <- as.data.frame(imputeTS::na.mean(d3))

# Step 2: Calculate Mahalanobis distance
mahalanobis_distance <- mahalanobis(d3_imputed[, 2:ncol(d3_imputed)], colMeans(d3_imputed[, 2:ncol(d3_imputed)]), cov(d3_imputed[, 2:ncol(d3_imputed)]))

# Step 3: Set the threshold (you can adjust the value as needed)
threshold <- qchisq(0.999, df = ncol(d3_imputed) - 1)

# Step 4: Identify outliers
outliers <- which(mahalanobis_distance > threshold)

# Step 5: Remove outliers
d3_cleaned <- d3[-outliers, ]

# ------------------------------------------------------------------------------












fit <- fit_uni_lcsm(
  data = d3,
  var = c("na1", "na2", "na3", "na4"),
  model = list(
    alpha_constant = TRUE,
    alpha_linear = FALSE,
    beta = TRUE,
    phi = TRUE
  ),
  missing = "fiml"
)

extract_fit(fit)


extract_param(fit) |>
  print()

summary(fit) |>
  print()





# both
multi4_lavaan_results <- fit_bi_lcsm(
  data = d3,
  var_x = x_var_list,
  var_y = y_var_list,
  model_x = list(alpha_constant = TRUE, beta = TRUE, phi = TRUE),
  model_y = list(alpha_constant = TRUE, beta = TRUE, phi = TRUE),
  coupling = list(xi_lag_xy = TRUE, xi_lag_yx = TRUE),
  missing = "fiml"
)

extract_fit(multi4_lavaan_results)



sscs <- piel_data |> 
  dplyr::select(user_id, starts_with("scs_")) |> 
  distinct(user_id, .keep_all = TRUE) |> 
  dplyr::select(-user_id)

mod <- '
  SC =~ scs_pos_1 + scs_neg_2 + scs_pos_3 + scs_neg_4 + scs_neg_5 + 
        scs_pos_6 + scs_pos_7 + scs_neg_8
'

fit <- sem(mod, data=sscs, std.lv = TRUE, estimator = "MLM")
fitMeasures(fit)

mod <- '
  P =~ scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7 
  N =~ scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8

  P ~~ N
'

fit <- sem(mod, data=sscs, std.lv = TRUE, estimator = "MLM")
fitMeasures(fit)

