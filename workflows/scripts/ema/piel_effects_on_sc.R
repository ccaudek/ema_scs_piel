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

# Recode `state-scs` -----------------------------------------------------------------

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

d |>
  dplyr::select(psc, nsc, neg_aff) |>
  ungroup() |>
  na.omit() |>
  cor() 

# Remove NAs on SC.
temp2 <- d[!(is.na(d$psc) | is.na(d$nsc) | is.na(d$neg_aff)), ]

unique(temp2$date)
# [1] 2022-04-09 2022-04-16 2022-04-23 2022-04-30 2022-05-07 2022-05-14 2022-05-21
# [8] 2022-05-28 2022-06-04 2022-04-02

temp2 |>
  group_by(day) |>
  summarize(
    n = n_distinct(user_id)
  )
  
# temp3 <- temp2 %>%
#   group_by(user_id) |> 
#   mutate(bysubj_day = dense_rank(day)) |> 
#   ungroup()

# boo <- temp3 |> 
#   group_by(foo) |> 
#   summarize(
#     n = n_distinct(user_id)
#   )

# temp4 <- temp3 |> 
#   dplyr::filter(bysubj_day < 11)

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

piel_data_1 <-  center3L(temp3, neg_aff, user_id, bysubj_day)
piel_data <-  center3L(piel_data_1, context, user_id, bysubj_day)

rm(temp, temp1, temp2, temp3) # ???

# temp <- with(
#   piel_data,
#   data.frame(
#     user_id, upset, nervous, satisfied, happy
#   ) |> 
#     mutate(upset = upset * -1, nervous = nervous * -1)
# )
# 
# panas_items <- temp %>%
#   group_by(user_id) %>%
#   summarize_at(vars(upset, nervous, satisfied, happy), mean, na.rm = TRUE) |>
#   dplyr::select(-user_id)
# 
# length(unique(temp$user_id))
# 
# ee <- eigen(cov(panas_items, use="complete.obs"))
# ee$values[1] / sum(ee$values)
# # [1] 0.7476952
# 
# mod <- 'na =~ upset + nervous + satisfied + happy'
# fit <- cfa(mod, data = panas_items, std.lv = TRUE)
# compRelSEM(fit)
# # 0.8
# 
# fa_scores <- lavPredict(fit)
# na <- fa_scores[, 1]
# 
# cor(na, rowSums(panas_items))
# [1] 0.9113241


# Check compliance

temp <- piel_data |> 
  group_by(bysubj_day) |> 
  summarize(
    nid = n_distinct(user_id), 
    n = n()
  ) 

# Compliance: on how many days on average the participants responded?
mean(temp$nid) / length(unique(piel_data$user_id))
# [1] 0.846319

# For the days in which participants responded, on which proportion of time-
# window they responded?
mean((temp$n / (temp$nid*5)))
# [1] 0.843486

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


# Negative State Self-Compassion and Negative Affect ------------------------------------

# mod_nsc <- lmer(
#   znsc ~ zdec + zcntx + na_moment + na_day + na_person +
#     (1 + zdec + zcntx + na_moment + na_day | user_id),
#   data = piel_data,
#   REML = T,
#   control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# ) 

mod_nsc <- lmer(
  znsc ~ na_moment + na_day + na_person +
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
