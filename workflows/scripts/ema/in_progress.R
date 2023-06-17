# Preliminary analysis of the effect of context on negative affect.
#
# https://cran.r-project.org/web/packages/multilevelTools/vignettes/lmer-vignette.html

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("rio")
  library("multilevelTools")
  library("lmerTest")
  library("JWileymisc") # testDistribution()
  library("extraoperators") # %!in%
  library("sjPlot") # plot_model()
})

d <- readRDS(here::here("data", "prep", "ema", "ema_data_2.RDS"))

d <- d |>
  mutate(
    neg_aff = upset + nervous - satisfied - happy,
    psc = scs_pos_1 + scs_pos_3 + scs_pos_6 + scs_pos_7,
    nsc = scs_neg_2 + scs_neg_4 + scs_neg_5 + scs_neg_8
  ) |>
  dplyr::rename(user_id = subj_code)

cor(d$psc, d$nsc)

#' The ICC is a measure of the proportion of variance that is between people
#' versus the total variance (i.e., variance between people and variance
#' within persons). multilevelTools provides a function, iccMixed() to
#' estimate ICCs based on mixed effects / multilevel models. The following
#' code does this for negative affect. The output is the ICC for the row
#' named user_id. An ICC of 1 indicates that 100% of all variance exists
#' between people, which would mean that 0% of variance exists within person,
#' indicating that people have identical scores every time they are assessed.
#' Conversely an ICC of 0 would indicate that everyone’s average was identical
#' and 100% of the variance exists within person. For negative affect, we can
#' see the ICCs fall between 0 and 1, indicating that some variance is between
#' people (i.e., individuals have different average levels of negative affect
#' and stress) but also that some variance is within person, meaning that
#' people’s negative affect fluctuates or vary within a person across the
#' day and the 10-days of the study.

iccMixed(
  dv = "neg_aff",
  id = "user_id",
  data = d
)

#' For multilevel data, it is helpful to examine between and within person
#' aspects of a variable separately. multilevelTools makes this easy using
#' the meanDecompose() function. This is important as, for example, if on
#' 11 of 12 days, someone has a negative affect score of 5, and then one
#' day a score of 1, the score of 1 may be an extreme value, for that person
#' even though it is common for the rest of the participants in the study.
#' meanDecompose() returns a list with X values at different levels, here
#' by ID and the residuals, which in this case are within person effects.

tmp <- meanDecompose(neg_aff ~ user_id, data = d)
str(tmp, nchar.max = 30)

#' We make plots of the distributions using testDistribution(), which
#' defaults to testing against a normal distribution, which is a common
#' default and in our case appropriate for linear mixed effects / multilevel
#' models.

plot(
  testDistribution(tmp[["neg_aff by user_id"]]$X,
    extremevalues = "theoretical", ev.perc = .001
  ),
  varlab = "Between Person Negative Affect"
)

plot(
  testDistribution(tmp[["neg_aff by residual"]]$X,
    extremevalues = "theoretical", ev.perc = .001
  ),
  varlab = "Within Person Negative Affect"
)

strict_control <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12
))

d$context_c <- d$context - mean(d$context)
d$neg_aff_c <- d$neg_aff - mean(d$neg_aff)
d$psc_c <- d$psc - mean(d$psc)
d$nsc_c <- d$nsc - mean(d$nsc)

m1 <- lmer(
  psc_c ~ context_c + neg_aff_c +
    (1 + context_c + neg_aff_c | user_id) +
    (1 | user_id:day) +
    (1 | user_id:day:time_window),
  data = d,
  control = strict_control
)

#' (1 + context + neg_aff | user_id): This specifies the random effects
#' part of the model formula. It includes random intercepts (1) and random
#' slopes for context and neg_aff variables, nested within the user_id
#' variable. This allows the intercept and slopes to vary across different
#' subjects.
#' (1 | user_id:day): This includes a random intercept for the interaction
#' of user_id and day. It captures the correlation of repeated measurements
#' within the same subject across different days.
#' (1 | user_id:day:time_window): This includes a random intercept for the
#' interaction of user_id, day, and time_window. It captures the correlation
#' of repeated measurements within the same subject across different days
#' and time windows.

m1x <- lmer(
  psc_c ~ context_c + neg_aff_c +
    (1 + context_c + neg_aff_c | user_id),
  data = d
)

md <- modelDiagnostics(m1x, ev.perc = .001)
plot(md, ask = FALSE, nrow = 4, ncol = 3)

mvextreme <- subset(
  md$extremeValues,
  EffectType == "Multivariate Random Effect user_id"
)
head(mvextreme)

unique(mvextreme$user_id)

strict_control <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12
))

m1a <- update(
  m1,
  data = subset(d, user_id %!in% unique(mvextreme$user_id)),
  control = strict_control
)

md <- modelDiagnostics(m1a, ev.perc = .001)
plot(md, ask = FALSE, ncol = 4, nrow = 3)

modelPerformance(m1a)

summary(m1a)

m3 <- lmer(
  nsc ~ context * neg_aff +
    (1 + context + neg_aff | user_id) +
    (1 | user_id:day) +
    (1 | user_id:day:time_window),
  data = d,
  control = strict_control
)

md <- modelDiagnostics(m3, ev.perc = .001)
plot(md, ask = FALSE, nrow = 4, ncol = 3)

mvextreme <- subset(
  md$extremeValues,
  EffectType == "Multivariate Random Effect user_id"
)
head(mvextreme)

unique(mvextreme$user_id)

m3a <- update(
  m3,
  data = subset(d, user_id %!in% unique(mvextreme$user_id)),
  control = strict_control
)

md <- modelDiagnostics(m3a, ev.perc = .001)
plot(md, ask = FALSE, ncol = 4, nrow = 3)

modelPerformance(m3a)

summary(m3a)

plot_model(m1a, type = "eff", terms = c("neg_aff_c", "context_c")) # + ylim(9, 20)
plot_model(m3, type = "eff", terms = c("neg_aff", "context")) # + ylim(9, 20)
