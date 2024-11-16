#' Script name: piel_effects_on_sc.R
#' Project: EMA SC piel
#' Script purpose: lmer analyses
#' @author: Corrado Caudek <corrado.caudek@unifi.it>
#' Date Created: Wed Jun 28 06:00:22 2023
#' Last Modified Date: Wed Jun 28 06:00:22 2023
#'
#' 👉 
#' PURPOSE: Contextual factors were assessed with three methods:
#' 1. Current mood.
#' 2. Pleasantness/unpleasantness of the most salient previously occurred event.
#' 3. Level of attachment/detachment to the current situation.
#'
#' Statistical analyses
#' 
#' - For exam-independent days:
#' 1. Within-person (within single day, across weeks) and between-person 
#' effects of the three contextual factors on the PSC and NSC components.
#' 2. Interactions effects with SCS.

# EMA notification dates.

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

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("rio")
  # library("multilevelTools")
  library("lmerTest")
  # library("JWileymisc") # testDistribution()
  # library("extraoperators") # %!in%
  library("sjPlot") # plot_model()
  library("performance")
  # library(jtools)
  library("glmmTMB")
  library("mlim")
  library("MuMIn")
  library("patchwork")
  library("brms")
  library(loo)
  library(report)
  # library("sjPlot")
  # library(effects)
  # library(readxl)
  # library(mousetrap)
  # library(misty)
  # library(report)
  # library(jtools)
  # library(interactions)
})

source(
  here::here(
    "workflows", "scripts", "ema", "functions", "funs_ema_piel.R"
  )
)

# Read raw data.
d <- readRDS(here::here("data", "prep", "ema", "ema_data_2.RDS"))

nrow(d)
# 1] 12722

# Recode `state-scs`

levels2_to_numeric <- c(
  "1" = -3,
  "2" = -2,
  "3" = -1,
  "4" =  1,
  "5" =  2,
  "6" =  3
)

temp <- d |>
  dplyr::select(dplyr::starts_with("scs_")) 

# Convert catch item to numeric values.
temp1 <-
  apply(temp[2:9], 2, function(x) levels2_to_numeric[x]) |>
  as.data.frame()

# Remove `scs_` items.
df <- d |>
  dplyr::select(-starts_with("scs_"))
# Add numeric columns for `scs_`
d1 <- cbind(df, temp1)

# Create new variables
d <- d1 |>
  mutate(
    neg_aff = upset + nervous - satisfied - happy,
    psc = scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7,
    nsc = scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8
  ) 

# Remove NAs on SC.
temp2 <- d[!(is.na(d$psc) | is.na(d$nsc) | is.na(d$neg_aff)), ]


# Compliance: on how many times on average the participants responded with
# respect to all the possible notifications?
nrow(temp2) / 
  (length(unique(temp2$user_id)) * 5 * length(unique(temp2$bysubj_day)))
# [1] 0.8322718


unique(temp2$date)
# [1] 2022-04-09 2022-04-16 2022-04-23 2022-04-30 2022-05-07 2022-05-14 2022-05-21
# [8] 2022-05-28 2022-06-04 2022-04-02

temp2 |>
  group_by(day) |>
  summarize(
    n = n_distinct(user_id)
  )
  
foo <- temp2 |>
  dplyr::select(
    psc, nsc, context, neg_aff
  )

check_outliers(foo[2:5])

bad_obs <- c(
  418, 694, 1308, 1567, 1886, 1889, 1890, 1891, 1892,
  1893, 1895, 1896, 1897, 1898, 1900, 1901, 1902, 1903, 1904, 1905, 1906, 1907,
  1908, 1909, 1910, 1914, 1915, 1916, 1917, 1918, 1919, 2565, 2567, 2571, 2572,
  2573, 2574, 2575, 2576, 2577, 2634, 2635, 2637, 2640, 2646, 2684, 2836, 2999,
  3244, 3245, 3247, 3626, 3895, 3905, 4256, 4380, 4541, 4720, 4748, 4826, 5271,
  5276, 5391, 5667, 6073, 6323, 6539, 6540, 6576, 6577, 6895, 7072, 7270, 7438,
  7538, 7574, 7821, 8058, 8074, 8152, 8154, 8713, 8776, 9425, 9429, 9861, 10406,
  10481, 10589, 10657, 10659, 10669, 10730, 10883, 10918, 11643, 11648, 11649,
  12035, 12043, 12052
)

temp3 <- temp2[-bad_obs, ]
nrow(temp3)
# [1] 12621

# Select only participants with a low number of missing values.

# Select the first 9 days
temp <- temp3 |> 
  dplyr::filter(bysubj_day != "10")

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

result <- d1 %>%
  group_by(user_id, bysubj_day, time_window) %>%
  filter(row_number() == 1)

# Within-person centering
piel_data_1 <- center3L(result, neg_aff, user_id, bysubj_day)
piel_data <- center3L(piel_data_1, context, user_id, bysubj_day)

# recode negative affect
piel_data$na_moment <- 
  (piel_data$neg_aff_Moment - mean(piel_data$neg_aff_Moment, na.rm= T)) /
  sd(piel_data$neg_aff_Moment, na.rm= T)

piel_data$na_day <- 
  (piel_data$neg_aff_Day - mean(piel_data$neg_aff_Day, na.rm= T)) /
  sd(piel_data$neg_aff_Day, na.rm= T)

piel_data$na_person <- 
  (piel_data$neg_aff_Person - mean(piel_data$neg_aff_Person, na.rm= T)) /
  sd(piel_data$neg_aff_Person, na.rm= T)

# recode event pleasantness
piel_data$cntx_moment <- 
  (piel_data$context_Moment - mean(piel_data$context_Moment, na.rm= T)) /
  sd(piel_data$context_Moment, na.rm= T)

piel_data$cntx_day <- 
  (piel_data$context_Day - mean(piel_data$context_Day, na.rm= T)) /
  sd(piel_data$context_Day, na.rm= T)

piel_data$cntx_person <- 
  (piel_data$context_Person - mean(piel_data$context_Person, na.rm= T)) /
  sd(piel_data$context_Person, na.rm= T)

# state self-compassion
piel_data$spsc <- piel_data$psc + 12.1
bc <- MASS::boxcox(spsc ~ na_moment * na_day + na_person, data=piel_data)
(lambda <- bc$x[which.max(bc$y)])
lambda <- 1.232323
piel_data$yp <- (piel_data$spsc^lambda-1)/lambda

plot(density(piel_data$yp))
cor(piel_data$yp, piel_data$psc)

piel_data$zpsc <- 
  (piel_data$yp - mean(piel_data$yp, na.rm= T)) /
  sd(piel_data$yp, na.rm= T)

cor(piel_data$zpsc, piel_data$psc)
# [1] 0.9984285
plot(density(piel_data$zpsc))

piel_data$znsc <- 
  (piel_data$nsc - mean(piel_data$nsc, na.rm= T)) /
  sd(piel_data$nsc, na.rm= T)

piel_data$zcntx <- 
  (piel_data$context - mean(piel_data$context, na.rm= T)) /
  sd(piel_data$context, na.rm= T)


# Save final EMA data set
saveRDS(
  piel_data,
  here::here("data", "prep", "ema", "cleaned_piel_data.RDS")
)

# Negative State Self-Compassion and Negative Affect ---------------------------

# lag on na_moment
# piel_data <- piel_data %>%
#   arrange(user_id, day, time_window) %>%
#   group_by(user_id, day) %>%
#   mutate(na_moment_lag = lag(na_moment, default = first(na_moment))) |> 
#   ungroup()
# This does not produce anything interesting.



# Variance components -----------------------------------------------------

foo <- piel_data

foo$nsc <- scale(foo$nsc) |> as.numeric()
foo$psc <- scale(foo$psc) |> as.numeric()
foo$bysubj_day <- factor(foo$bysubj_day)


# Step 1: Filter days with less than 3 notifications and remove subjects with insufficient days
foo_filtered <- foo %>%
  group_by(user_id, bysubj_day) %>%
  dplyr::filter(n_distinct(time_window) >= 3) %>%
  ungroup() %>%
  group_by(user_id) %>%
  dplyr::filter(n_distinct(bysubj_day) >= min_days_per_subject) %>%
  ungroup()

# Step 2: Calculate raw slopes within each subject and day
slopes_within <- foo_filtered %>%
  group_by(user_id, bysubj_day) %>%
  nest() %>%
  mutate(model = map(
    data, ~ lm(nsc ~ psc + neg_aff_Day + neg_aff_Person + neg_aff_Moment + 
                 context_Moment + context_Day + context_Person, data = .x)
    ),
         slope = map_dbl(model, ~ coef(.x)["psc"])) %>%
  ungroup() %>%
  dplyr::select(user_id, bysubj_day, slope)

# Step 3: Compute average slope for each subject
avg_slope_per_subject <- slopes_within %>%
  group_by(user_id) %>%
  summarise(avg_slope = mean(slope, na.rm = TRUE))

# Step 4: Calculate variance of average slopes across subjects (between-subject variability)
between_subject_variance <- var(avg_slope_per_subject$avg_slope, na.rm = TRUE)
cat("Between-subject variance:", between_subject_variance, "\n")


# Step 5: Compute the variance of slopes within each subject across days
within_subject_variance <- slopes_within %>%
  group_by(user_id) %>%
  summarise(var_slope_within = var(slope, na.rm = TRUE))

# Step 6: Calculate the mean within-subject variance across all subjects
mean_within_subject_variance <- mean(within_subject_variance$var_slope_within, na.rm = TRUE)

# Print the result
cat("Mean within-subject variance:", mean_within_subject_variance, "\n")


# Step 1: Fit a multilevel model with random slopes for subjects
model_lme_day <- lmer(
  nsc ~ psc + neg_aff + context + (1 + psc | user_id) + (1 | user_id:bysubj_day), 
  data = foo_filtered
)

model_lme_day <- lmer(
  nsc ~ psc + 
    neg_aff_Day + neg_aff_Person + neg_aff_Moment + 
    context_Moment + context_Day + context_Person + 
    (1 + psc | user_id) + 
    (1 | user_id:bysubj_day), 
  data = foo_filtered
)

# Step 2: Summary of the model to inspect variance components
summary(model_lme_day)

# Step 1: Extract the between-subject variance (random slopes for subjects)
between_subject_variance <- as.data.frame(VarCorr(model_lme_day)) %>%
  dplyr::filter(grp == "user_id" & var1 == "psc") %>%
  select(vcov) %>%
  pull()

cat("Between-subject variance (from random slopes):", between_subject_variance, "\n")

# Step 2: Extract within-subject variance (random intercepts for days within subjects)
within_subject_variance <- as.data.frame(VarCorr(model_lme_day)) %>%
  dplyr::filter(grp == "user_id:bysubj_day") %>%
  select(vcov) %>%
  pull()

cat("Within-subject variance (from day-level intercepts):", within_subject_variance, "\n")

# Step 3: Extract the residual variance (within-subject/day-level variability)
residual_variance <- as.data.frame(VarCorr(model_lme_day)) %>%
  dplyr::filter(grp == "Residual") %>%
  select(vcov) %>%
  pull()

cat("Residual variance (within-day variance):", residual_variance, "\n")


# Step 1: Fit the model 
fit <- brm(
  nsc ~ psc + 
    neg_aff_Day + neg_aff_Person + neg_aff_Moment + 
    context_Moment + context_Day + context_Person + 
    (1 + psc | user_id) + 
    (1 | user_id:bysubj_day),
  data = foo_filtered,
  family = student(),
  cores = 8,
  algorithm = "meanfield",
  backend = "cmdstanr"
)
pp_check(fit)

# Step 2: Extract variance components from the fitted model
variance_components <- VarCorr(fit)
print(variance_components)

# Step 3: Extract between-subject variance (random slopes for psc)
between_subject_variance <- variance_components$user_id$sd["psc", "Estimate"]^2
cat("Between-subject variance (random slopes for psc):", between_subject_variance, "\n")
# Between-subject variance (random slopes for psc): 0.02788971 

# Step 4: Correct extraction of within-subject variance (random intercepts for days within subjects)
within_subject_variance <- variance_components$`user_id:bysubj_day`$sd["Intercept", "Estimate"]^2
cat("Within-subject variance (day-level intercepts):", within_subject_variance, "\n")
# Within-subject variance (day-level intercepts): 0.02816534 

# Step 5: Extract the posterior mean for the residual scale (sigma) and degrees of freedom (nu)
posterior_summary <- summary(fit)$spec_pars

# Extract sigma (scale parameter for the Student's t distribution)
sigma <- posterior_summary["sigma", "Estimate"]

# Extract nu (degrees of freedom for the Student's t distribution)
nu <- posterior_summary["nu", "Estimate"]

# Step 6: Calculate residual variance for Student's t-distribution
if(nu > 2) {
  residual_variance <- (nu / (nu - 2)) * sigma^2
  cat("Residual variance (within-day variance):", residual_variance, "\n")
} else {
  cat("Degrees of freedom too low to compute residual variance.\n")
}
# Residual variance (within-day variance): 0.2074695 

#' To compare within-subject variation in the relationship between the 
#' positive (CS) and negative (UCS) components of State Self-Compassion, we 
#' operationalized this relationship as the linear slope of UCS scores predicted 
#' by CS scores, controlling for Negative Affect and context evaluation, both 
#' coded at within-day, between-day, and between-subject levels.
#' We conducted a Bayesian multilevel analysis after filtering the data to 
#' ensure at least three notifications per day. This was followed by fitting a 
#' mixed-effects model with random intercepts at the day level and random slopes 
#' for CS at the subject level to account for the hierarchical structure of the 
#' data.
#' 
#' The results show that the between-subject variance for the effect of CS on 
#' UCS was relatively small (variance = 0.0279), suggesting that the 
#' relationship between these components is generally stable across individuals. 
#' In contrast, the within-subject variance at the day level was substantial 
#' (variance = 0.0282), indicating notable daily fluctuations within individuals. 
#' Furthermore, the residual variance, representing within-day variability, was 
#' large (variance = 0.2075), suggesting that unmeasured momentary factors 
#' contribute significantly to the variation in UCS.
#' 
#' These findings imply that while an inverse relationship between CS and UCS is 
#' consistent at the between-subject level, significant within-subject 
#' variability suggests that this relationship may be influenced by daily 
#' changes in context and negative affect, underscoring the importance of 
#' considering both momentary and day-level factors when testing the bipolar 
#' continuum hypothesis.




#######












# Now, remove subjects who do not have enough days left after filtering
# Define a threshold for minimum number of valid days (adjust based on your needs, e.g., 2 valid days)
min_days_per_subject <- 2  # Adjust this value based on your requirement

foo_filtered <- foo_filtered %>%
  group_by(user_id) %>%
  filter(n_distinct(bysubj_day) >= min_days_per_subject) %>%
  ungroup()

# Calculate raw slopes within each subject and day
slopes_within <- foo_filtered %>%
  group_by(user_id, bysubj_day) %>%
  do(model = lm(nsc ~ psc + neg_aff, data = .)) %>%
  mutate(slope = coef(model)[["psc"]]) %>%
  ungroup()

# Inspect the calculated slopes
head(slopes_within)

# Calculate variance of slopes within subjects (across days)
within_var_raw <- slopes_within %>%
  group_by(user_id) %>%
  summarise(var_slope_within = var(slope, na.rm = TRUE)) %>%
  summarise(mean_within_var = mean(var_slope_within, na.rm = TRUE))

# Calculate variance of slopes between subjects
between_var_raw <- var(slopes_within$slope, na.rm = TRUE)

# Print raw variances
cat("Mean variance of slopes within subjects (raw data):", within_var_raw$mean_within_var, "\n")
# Mean variance of slopes within subjects (raw data): 2.117806 
cat("Variance of slopes between subjects (raw data):", between_var_raw, "\n")
# Variance of slopes between subjects (raw data): 1.937069 


# Define weakly informative priors
priors <- c(
  prior(normal(0, 1), class = "sd", group = "user_id"),
  prior(normal(0, 1), class = "sigma")  # For residuals
)

# Fit the model with adjusted priors
model_adjusted <- brm(
  slope ~ 1 + (1 + bysubj_day | user_id),
  data = slopes_within,
  family = student(),
  prior = priors,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  backend = "cmdstanr"
)

pp_check(model_adjusted) + xlim(-10, 10)

# Extract the variance components
variance_components <- VarCorr(model_adjusted)

# Print the variance components to inspect
print(variance_components)

# Extract specific variances
between_subject_variance <- variance_components$user_id$sd[1]^2  # Between-subject variance
within_subject_variance <- attr(variance_components, "residual")$sd^2  # Within-subject variance

cat("Between-subject variance:", between_subject_variance, "\n")
cat("Within-subject variance:", within_subject_variance, "\n")



# Get posterior samples for random effects
posterior_samples <- as_draws_df(model_adjusted)

# Between-subject variance (from user_id)
between_subject_posterior <- posterior_samples[["sd_user_id__Intercept"]]^2

# Residual variance (within-subject)
within_subject_posterior <- posterior_samples[["sigma"]]^2

# Compute 95% credible intervals
between_subject_ci <- quantile(between_subject_posterior, probs = c(0.025, 0.975))
within_subject_ci <- quantile(within_subject_posterior, probs = c(0.025, 0.975))

cat("95% CI for between-subject variance:", between_subject_ci, "\n")
cat("95% CI for within-subject variance:", within_subject_ci, "\n")

between_subject_est <- quantile(between_subject_posterior, probs = c(0.5))
within_subject_est <- quantile(within_subject_posterior, probs = c(0.5))
c(between_subject_est, within_subject_est)


# -------------------------------------------------------------------------



m <- lmer(
  znsc ~ na_moment + na_day + na_person + 
    cntx_moment + cntx_day + cntx_person + 
    (1 + na_moment + cntx_moment + na_day + cntx_day | user_id),
  data = piel_data
)

mod_nsc <- brm(
  znsc ~ na_moment + na_day + na_person + 
    cntx_moment + cntx_day + cntx_person + 
    # na_moment:cntx_moment + na_day:cntx_day + na_person:cntx_person +
    (1 + na_moment + cntx_moment + na_day + cntx_day | user_id),
  data = piel_data,
  family = asym_laplace(),
  backend = "cmdstanr",
  algorithm = "meanfield"
)
pp_check(mod_nsc)
summary(mod_nsc)
marginal_effects(mod_nsc, "na_moment")
bayes_R2(mod_nsc)
loo <- loo(mod_nsc)
plot(loo)





delta_t <-
  posterior_samples(mod_nsc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_na_moment / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%       50%     97.5% 
# 0.4807264 0.4994895 0.5180982 

delta_t <-
  posterior_samples(mod_nsc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_na_day / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%       50%     97.5% 
# 0.5935431 0.6098466 0.6258260 

delta_t <-
  posterior_samples(mod_nsc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_na_person / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%      50%    97.5% 
# 1.343279 1.362746 1.383117 

delta_t <-
  posterior_samples(mod_nsc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_cntx_moment / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%          50%        97.5% 
# -0.037251242 -0.021409754 -0.004058851 

delta_t <-
  posterior_samples(mod_nsc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_cntx_day / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%        50%      97.5% 
# 0.02501838 0.03771344 0.05103810 

delta_t <-
  posterior_samples(mod_nsc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_cntx_person / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%       50%     97.5% 
# 0.1613766 0.1773132 0.1928712 


# Positive SC


mod_psc <- brm(
  zpsc ~ na_moment + na_day + na_person + 
    cntx_moment + cntx_day + cntx_person + 
    # na_moment:cntx_moment + na_day:cntx_day + na_person:cntx_person +
    (1 + na_moment + cntx_moment + na_day + cntx_day | user_id),
  data = piel_data,
  family = asym_laplace(),
  # backend = "cmdstanr",
  algorithm = "meanfield"
)
pp_check(mod_psc)
summary(mod_psc)
marginal_effects(mod_psc, "na_moment")
bayes_R2(mod_psc)
loo <- loo(mod_psc)
plot(loo)


tab_model(mod_psc, mod_nsc, show.ci = 0.89)


# Effect size

delta_t <-
  posterior_samples(mod_psc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_na_moment / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%        50%      97.5% 
# -0.3752343 -0.3604481 -0.3468724 

delta_t <-
  posterior_samples(mod_psc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_na_day / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%        50%      97.5% 
# -0.5528627 -0.5347352 -0.5157917 

delta_t <-
  posterior_samples(mod_psc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_na_person / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%        50%      97.5% 
# -1.0045272 -0.9790804 -0.9534275  

delta_t <-
  posterior_samples(mod_psc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_cntx_moment / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%         50%       97.5% 
# -0.03503294 -0.02311753 -0.01090089 

delta_t <-
  posterior_samples(mod_psc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_cntx_day / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%          50%        97.5% 
# -0.003415534  0.029785854  0.063546637 

delta_t <-
  posterior_samples(mod_psc, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 8:13, .funs = funs(.^2) ) %>%
  mutate(delta = b_cntx_person / sqrt(rowSums(.[8:13]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%          50%        97.5% 
# -0.031602952 -0.013903094  0.001732434 













mod_nsc <- lmer(
  znsc ~ na_moment + na_day + na_person +
    cntx_moment + cntx_day + cntx_person +
    (1 + na_moment + na_day | user_id),
  data = piel_data,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_nsc)

MuMIn::r.squaredGLMM(mod_nsc)
#     R2m      R2c
# 0.4705414 0.753502

summary(mod_nsc)

# Check assumptions.

# Check for normality
res <- residuals(mod_nsc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_nsc), piel_data$znsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_nsc, type='diag')

sjPlot::tab_model(mod_nsc, title = "Negative State Self-Compassion")

# Plot effect
p_neg_mom <- plot_model(
  mod_nsc, 
  type= "pred", 
  terms="na_moment", 
  axis.lim = c(-2, 2), 
  title = "",
  axis.title = "Negative State Self-Compassion",
  wrap.title = "asjdhfk"
) + 
  labs(x = "Negatve Affect (moment)")


p_neg_day <- plot_model(
  mod_nsc, 
  type= "pred", 
  terms="na_day", 
  axis.lim = c(-2, 2), 
  title = "",
  axis.title = "Negative State Self-Compassion",
) + 
  labs(x = "Negatve Affect (day)")


p_neg_per <- plot_model(
  mod_nsc, 
  type= "pred", 
  terms="na_person", 
  axis.lim = c(-2, 2), 
  title = "",
  axis.title = "Negative State Self-Compassion",
) + 
  labs(x = "Negative Affect (person)")


# Positive State Self-Compassion and Negative Affect ------------------------------------

mod_psc <- lmer(
  zpsc ~ na_moment + na_day + na_person +
    (1 + na_moment + na_day | user_id),
  data = piel_data,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_psc)

MuMIn::r.squaredGLMM(mod_psc)
#    R2m      R2c
# 0.4104511 0.7416541

summary(mod_psc)

# Check assumptions.

# Check for VIF
car::vif(mod_psc)

# Check for normality
res <- residuals(mod_psc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_psc), piel_data$zpsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_psc, type='diag')

sjPlot::tab_model(mod_psc, title = "Positive State Self-Compassion")


# Plot effect
p_pos_mom <- plot_model(
  mod_psc, 
  type= "pred", 
  terms="na_moment", 
  axis.lim = c(-2, 2), 
  title = "",
  axis.title = "Positive State Self-Compassion"
) + 
  labs(x = "Negatve Affect (moment)")

p_pos_mom + p_neg_mom


p_pos_day <- plot_model(
  mod_psc, 
  type= "pred", 
  terms="na_day", 
  axis.lim = c(-2, 2), 
  title = "",
  axis.title = "Positive State Self-Compassion",
) + 
  labs(x = "Negatve Affect (day)")

p_pos_day + p_neg_day


p_pos_per <- plot_model(
  mod_psc, 
  type= "pred", 
  terms="na_person", 
  axis.lim = c(-2, 2), 
  title = "",
  axis.title = "Positive State Self-Compassion",
) + 
  labs(x = "Negative Affect (person)")

p_pos_per + p_neg_per

# Negative State Self-Compassion and Event Pleasantness ------------------------------------

mod_cntx_nsc <- lmer(
  znsc ~ cntx_moment + cntx_day + cntx_person +
    (1 + cntx_moment + cntx_day | user_id),
  data = piel_data,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_cntx_nsc)

MuMIn::r.squaredGLMM(mod_cntx_nsc)
#     R2m       R2c
# 0.09076359 0.6488234

summary(mod_cntx_nsc)

# Check assumptions.

# Check for VIF
car::vif(mod_cntx_nsc)

# Check for normality
res <- residuals(mod_cntx_nsc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_cntx_nsc), piel_data$zpsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_cntx_nsc, type='diag')

sjPlot::tab_model(mod_cntx_nsc, title = "Negative State Self-Compassion")


plot_model(mod_cntx_nsc)


# Positive State Self-Compassion and Event Pleasantness ------------------------------------

mod_cntx_psc <- lmer(
  zpsc ~ cntx_moment + cntx_day + cntx_person +
    (1 + cntx_moment + cntx_day | user_id),
  data = piel_data,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod_cntx_psc)

MuMIn::r.squaredGLMM(mod_cntx_psc)
#    R2m       R2c
# 0.1512064 0.6514

summary(mod_cntx_psc)

# Check assumptions.

# Check for VIF
car::vif(mod_cntx_psc)

# Check for normality
res <- residuals(mod_cntx_psc)
lattice::qqmath(res)

# Check for linearity
plot(residuals(mod_cntx_psc), piel_data$zpsc) 

# Check for homoscedasticity
# Check normality RE
sjPlot::plot_model(mod_cntx_psc, type='diag')

sjPlot::tab_model(mod_cntx_psc, title = "Positive State Self-Compassion")

# Add SCS data ------------------------------------------------------

# Import SCS data
scs_scores_df <- rio::import(
  here::here(
    "data", "prep", "quest_scales", "scs_scores.csv"
  )
)

# intersect(
#   scs_scores_df$user_id, piel_data$user_id
# )

dat <- left_join(piel_data, scs_scores_df, by = "user_id")

# Imputation
# temp = mice(dat, seed = 500) 
# data_imp = complete(temp, 1)

temp <- dat |> 
  select_if(is.numeric)

MLIM <- mlim(temp, m=1, seed = 2022, tuning_time = 180) 

dat$scs_total_score <- MLIM$scs_total_score
dat$scs <- scale(dat$scs_total_score) |> as.numeric()

mod3_psc <- lmer(
  zpsc ~ scs * (na_moment + na_day + na_person) +
    (1 + na_moment + na_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod3_psc)
MuMIn::r.squaredGLMM(mod3_psc)

car::vif(mod3_psc)
summary(mod3_psc)
sjPlot::tab_model(mod3_psc, title = "")

mod3_nsc <- lmer(
  znsc ~ scs * (na_moment + na_day + na_person) +
    (1 + na_moment + na_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

mod3_nsc <- lmer(
  znsc ~ scs * na_person +
    (1 | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod3_nsc)
MuMIn::r.squaredGLMM(mod3_nsc)

car::vif(mod3_nsc)
summary(mod3_nsc)
sjPlot::tab_model(mod3_nsc, title = "")

# positive scs and event pleasantness

mod5_cntx_psc <- lmer(
  zpsc ~ scs_total_score * (cntx_moment + cntx_day + cntx_person) +
    (1 + cntx_moment + cntx_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod5_cntx_psc)
MuMIn::r.squaredGLMM(mod5_cntx_psc)

car::vif(mod5_cntx_psc)
summary(mod5_cntx_psc)
sjPlot::tab_model(mod5_cntx_psc, title = "")

# negative scs and event pleasantness

mod5_cntx_nsc <- lmer(
  znsc ~ scs_total_score * (cntx_moment + cntx_day + cntx_person) +
    (1 + cntx_moment + cntx_day | user_id),
  data = dat,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

reportMLM(mod5_cntx_nsc)
MuMIn::r.squaredGLMM(mod5_cntx_nsc)

car::vif(mod5_cntx_nsc)
summary(mod5_cntx_nsc)
sjPlot::tab_model(mod5_cntx_nsc, title = "")

# eof -----------------

# bysubj_psc <- dat |> 
#   group_by(user_id) |> 
#   summarize(
#     zpsc = mean(zpsc),
#     scs_total_score = mean(scs_total_score)
#   )
# 
# fm <- lm(
#   zpsc ~ scale(scs_total_score),
#   data = bysubj_psc
# )
# summary(fm)
# 

# eof ----

# 
# with(piel_data,
#   cbind(zdec, zcntx, nervous, upset, happy, satisfied)
# ) |> cor()
# 
# 
# 
# 
# m1 <- lmer(
#   zpsc ~ znsc * (zdec + zcntx + na_moment + na_day + na_person) +
#     (1 + znsc + (zdec + zcntx + na_moment + na_day + na_person) | user_id),
#   data = piel_data,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# m2 <- lmer(
#   znsc ~ zpsc * (zdec + zcntx + na_moment + na_day + na_person) +
#     (1 + zpsc + (zdec + zcntx + na_moment + na_day + na_person) | user_id),
#   data = piel_data,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# piel_data <-  center3L(piel_data, zcntx, user_id, bysubj_day)
# 
# 
# m1 <- lmer(
#   zpsc ~ zcntx_Moment + zcntx_Day + zcntx_Person + 
#     (1 + zcntx_Moment + zcntx_Day + zcntx_Person | user_id),
#   data = piel_data,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# m2 <- lmer(
#   znsc ~ zcntx_Moment + zcntx_Day + zcntx_Person + 
#     (1 + zcntx_Moment + zcntx_Day + zcntx_Person | user_id),
#   data = piel_data,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# 
# piel_data <-  center3L(piel_data, zdec, user_id, bysubj_day)
# 
# 
# mod1 <- lmer(
#   zpsc ~ zdec_Moment + zdec_Day + zdec_Person + 
#     (1 + zdec_Moment + zdec_Day + zdec_Person | user_id),
#   data = piel_data,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# mod2 <- lmer(
#   znsc ~ zdec_Moment + zdec_Day + zdec_Person + 
#     (1 + zdec_Moment + zdec_Day + zdec_Person | user_id),
#   data = piel_data,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# 
# 
# 
# 
# ################
# 
# 
# 
# piel_data$sc_comb <- piel_data$psc + piel_data$nsc
# hist(piel_data$sc_comb)
# 
# 
# m1 <- lmer(
#   sc_comb ~ na_moment + na_day + na_person + 
#     (1 + na_moment + na_day + na_person | user_id),
#   data = piel_data,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# summary(m1)
# 
# reportMLM(m1)
# 


bform <- bf(znsc | mi() ~ mi(na_moment) + mi(na_day) + na_person +
              (1 + mi(na_moment) + mi(na_day) | user_id)) +
  bf(na_moment | mi() ~ mi(na_day) + na_person) +
  bf(na_day | mi() ~ mi(na_moment) + na_person) +
  set_rescor(FALSE)


fit <- brm(
  bform, 
  data = df,
  algorithm = "meanfield"
)
summary(fit)
