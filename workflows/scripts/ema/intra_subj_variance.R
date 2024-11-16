#' Longitudinal model of the intra-subject variance of CS and UCS
#' as a function of DERS.
#' 
#' The idea comes from this paper:
#' Feng, Y., & Hancock, G. R. (2022, April 11). A Structural Equation Modeling 
#' Approach for Modeling Variability as a Latent Variable. Psychological 
#' Methods. Advance online publication. http://dx.doi.org/10.1037/met0000477
#' 
#' 

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("rio")
  library("multilevelTools")
  library("lmerTest")
  library("JWileymisc") # testDistribution()
  library("extraoperators") # %!in%
  library("sjPlot") # plot_model()
  library("brms")
  library("cmdstanr")
  library("MplusAutomation")
  library("gt")
  library("glue")
  library("kableExtra")
  library("misty")
})

d <- readRDS(here::here("data", "prep", "ema", "ema_data_2.RDS"))

d <- d |>
  mutate(
    neg_aff = upset + nervous - satisfied - happy,
    psc = scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7,
    nsc = scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8
  ) 

cor(d$psc, d$nsc)

d$bysubj_day_f <- factor(d$bysubj_day)
d$time_window_f <- factor(d$time_window)


# Compute intra-subject standard deviation for each variable across all measurement occasions
intra_subject_sd <- d %>%
  group_by(user_id) %>%
  summarize(
    sd_psc = sd(psc, na.rm = TRUE),
    sd_nsc = sd(nsc, na.rm = TRUE),
    sd_neg_aff = sd(neg_aff, na.rm = TRUE),
    sd_context = sd(context, na.rm = TRUE)
  )

# View the result
print(intra_subject_sd)

plot(intra_subject_sd$sd_psc, intra_subject_sd$nsc)

plot(intra_subject_sd$sd_neg_aff, intra_subject_sd$sd_psc)
plot(intra_subject_sd$sd_neg_aff, intra_subject_sd$sd_nsc)

plot(intra_subject_sd$sd_context, intra_subject_sd$sd_psc)
plot(intra_subject_sd$sd_context, intra_subject_sd$sd_nsc)

ders <- rio::import(
  here::here("data", "prep", "quest_scales", "ders_scores.csv")
) |> 
  dplyr::select(user_id, ders_ts)


tot_df <- left_join(intra_subject_sd, ders, by = "user_id")

d1 <- left_join(d, ders, by = "user_id")


fm1 <- lm(scale(sd_psc) ~ scale(ders_ts), data = tot_df)
summary(fm1)

fm2 <- lm(scale(sd_nsc) ~ scale(ders_ts), data = tot_df)
summary(fm2)

# Increased variability in SC is associated with high DERS scores (difficulti in 
# emotion regulation)
# There is no relation between USC variability and DERS scores.


# Location Scale Model ----------------------------------------------------

options(mc.cores = parallel::detectCores())

d1$psc <- scale(d1$psc) |> as.numeric()
d1$nsc <- scale(d1$nsc) |> as.numeric()
d1$ders <- scale(d1$ders_ts) |> as.numeric()


mod1_ls <- brm(
  bf(
    psc ~ 1 + ders + (1 + ders | user_id / bysubj_day),  
    sigma ~ 1 + ders + (1 | user_id)
  ),
  family = gaussian(),  # Use function for clarity
  data = d1,
  seed = 1234,

  backend = "cmdstanr",
  silent = 2, refresh = 0
  # algorithm = "meanfield"
)
pp_check(mod1_ls)
summary(mod1_ls)
bayes_R2(mod1_ls)


# Save data for Mplus -----------------------------------------------------

# Glimpse the original data
glimpse(d1)


d1_imp <- missRanger(d1, pmm.k = 5, num.trees = 500)

# Prepare the data for Mplus
ema_data <- d1_imp %>%
  # Select relevant variables
  dplyr::select(user_id, bysubj_day, time_window, psc, nsc, ders) %>%
  
  # Rename columns for Mplus compatibility
  rename(
    CurAve_d = psc,       # Use psc (positive SC) as the main variable
    UCS = nsc,            # Use nsc (negative SC) if needed later
    FSMean = ders         # DERS as Level-2 variable (renamed for compatibility)
  ) %>%
  # Ensure no duplicated rows (if applicable)
  distinct()


ema_data %>%
  group_by(user_id) %>%
  summarise(FSMean_var = var(FSMean)) %>%
  filter(FSMean_var > 0)

# Aggregate FSMean to be constant for each user_id
ema_data <- ema_data %>%
  group_by(user_id) %>%
  mutate(FSMean = mean(FSMean, na.rm = TRUE)) %>%
  ungroup()


ema_data %>%
  group_by(user_id) %>%
  summarise(FSMean_var = var(FSMean, na.rm = TRUE)) %>%
  dplyr::filter(FSMean_var > 0) %>%
  print(n = Inf)

ema_data <- ema_data %>%
  group_by(user_id) %>%
  mutate(FSMean = mean(FSMean, na.rm = TRUE)) %>%
  ungroup()

ema_data %>%
  group_by(user_id) %>%
  summarise(FSMean_var = var(FSMean, na.rm = TRUE)) %>%
  dplyr::filter(FSMean_var > 0)


ema_data %>%
  group_by(user_id) %>%
  summarise(FSMean_var = var(FSMean)) %>%
  dplyr::filter(FSMean_var > 0)

ema_data <- ema_data %>%
  group_by(user_id) %>%
  summarise(
    bysubj_day = first(bysubj_day), # Retain one value for compatibility (not used in BETWEEN)
    time_window = first(time_window), # Same for time_window
    CurAve_d = list(CurAve_d),       # Retain within-level variable
    FSMean = mean(FSMean, na.rm = TRUE) # Force FSMean to be constant
  ) %>%
  unnest(cols = c(CurAve_d)) # Restore within-level structure

ema_data %>%
  group_by(user_id) %>%
  summarise(FSMean_var = var(FSMean, na.rm = TRUE)) %>%
  dplyr::filter(FSMean_var > 0) # Should produce no rows


ema_data <- ema_data %>%
  mutate(user_id_numeric = as.numeric(factor(user_id))) %>%  # Create numeric user_id
  dplyr::select(user_id_numeric, bysubj_day, time_window, CurAve_d, FSMean)  # Keep relevant columns


write.table(
  ema_data_imp, 
  here::here(
    "workflows", "scripts", "ema", "mplus_models", "ema_data.dat"
  ),
  row.names = FALSE, 
  col.names = FALSE, sep = " "
)


# Load the .dat file back into R
ema_data_check <- read.table(
  here::here("workflows", "scripts", "ema", "mplus_models", "ema_data.dat"), 
  header = FALSE, 
  sep = " "
)

# Assign column names for clarity
colnames(ema_data_check) <- c("user_id_numeric", "bysubj_day", "time_window", "CurAve_d", "FSMean")

# Check for variation in FSMean within clusters
ema_data_check %>%
  group_by(user_id_numeric) %>%
  summarise(FSMean_var = var(FSMean)) %>%
  filter(FSMean_var > 0)

# Filter problematic clusters
problematic_ids <- c(17, 19, 20, 24, 26, 37, 44, 50, 53, 61) # Add all from the output
ema_data_check %>%
  dplyr::filter(user_id_numeric %in% problematic_ids) %>%
  arrange(user_id_numeric)

# Reprocess to make FSMean constant within each user_id_numeric
ema_data <- ema_data %>%
  group_by(user_id_numeric) %>%
  mutate(FSMean = mean(FSMean, na.rm = TRUE)) %>%  # Force constant value
  ungroup()

# Save the corrected data to a new .dat file
write.table(
  ema_data,
  here::here("workflows", "scripts", "ema", "mplus_models", "ema_data.dat"),
  row.names = FALSE,
  col.names = FALSE,
  sep = " "
)





# Set the_dir for further use when specifying the location of Mplus models.
the_dir <- here::here("workflows", "scripts", "ema", "mplus_models")


# Model 1: Unconditional ------------------------------------------------

runModels(paste0(the_dir, "/m1_unconditional.inp"), showOutput = TRUE)
m1_unc <- MplusAutomation::readModels(paste0(the_dir, "/m1_unconditional.out"))
summary(m1_unc)


# Model 1: Conditional ------------------------------------------------

runModels(paste0(the_dir, "/m1_conditional.inp"), showOutput = TRUE)
m1_unc <- MplusAutomation::readModels(paste0(the_dir, "/m1_unconditional.out"))
summary(m1_unc)


runModels(paste0(the_dir, "/m3_conditional.inp"), showOutput = TRUE)
m3_unc <- MplusAutomation::readModels(paste0(the_dir, "/m3_unconditional.out"))
summary(m1_unc)

#### UCS -----------------------

# Prepare the data for Mplus
ema_data <- d1_imp %>%
  # Select relevant variables
  dplyr::select(user_id, bysubj_day, time_window, psc, nsc, ders) %>%
  
  # Rename columns for Mplus compatibility
  rename(
    CurAve_d = nsc,       # Use NSC (negative SC) as the main variable
    PSC = psc,            # Rename PSC for potential future use
    FSMean = ders         # DERS as the Level-2 variable (renamed for clarity)
  ) %>%
  
  # Ensure no duplicated rows (if applicable)
  distinct()

# Check if FSMean varies within user_id clusters
ema_data %>%
  group_by(user_id) %>%
  summarise(FSMean_var = var(FSMean)) %>%
  filter(FSMean_var > 0)

# Aggregate FSMean to be constant for each user_id
ema_data <- ema_data %>%
  group_by(user_id) %>%
  mutate(FSMean = mean(FSMean, na.rm = TRUE)) %>%
  ungroup()

# Verify that FSMean is constant within clusters
ema_data %>%
  group_by(user_id) %>%
  summarise(FSMean_var = var(FSMean, na.rm = TRUE)) %>%
  filter(FSMean_var > 0)  # Should return no rows

# Summarize and structure data for Mplus
ema_data <- ema_data %>%
  group_by(user_id) %>%
  summarise(
    bysubj_day = first(bysubj_day),       # Retain representative value
    time_window = first(time_window),    # Retain representative value
    CurAve_d = list(CurAve_d),           # Retain within-level variable
    FSMean = mean(FSMean, na.rm = TRUE)  # Ensure FSMean is constant
  ) %>%
  unnest(cols = c(CurAve_d))  # Restore within-level structure

# Convert user_id to numeric for compatibility with Mplus
ema_data <- ema_data %>%
  mutate(user_id_numeric = as.numeric(factor(user_id))) %>%
  dplyr::select(user_id_numeric, bysubj_day, time_window, CurAve_d, FSMean)

# Save data to a .dat file for Mplus
write.table(
  ema_data,
  here::here("workflows", "scripts", "ema", "mplus_models", "ema_data.dat"),
  row.names = FALSE,
  col.names = FALSE,
  sep = " "
)


runModels(paste0(the_dir, "/m3_conditional.inp"), showOutput = TRUE)
m3_unc <- MplusAutomation::readModels(paste0(the_dir, "/m3_unconditional.out"))
summary(m1_unc)
