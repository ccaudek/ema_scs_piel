# Script name: an_covariance.R
# Project: EMA piel
# Script purpose: test bipolar continuum hypothesis with analysis of covariance
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Tue Aug  1 15:25:36 2023
# Last Modified Date: Tue Aug  1 15:25:36 2023
#
# 👉 

suppressPackageStartupMessages({
  library("performance")
  library("loo")
  library("coda")
})

# Read cleaned EMA data set
piel_data <- readRDS(
  here::here("data", "prep", "ema", "cleaned_piel_data.RDS")
)

sc <- (c(-piel_data$znsc, piel_data$zpsc))
valence <- 
  c(
    rep("n", length(piel_data$znsc)), rep("p", length(piel_data$zpsc))
    ) |> 
  as.factor()
na_moment <- c(piel_data$na_moment, piel_data$na_moment)
na_day <- c(piel_data$na_day, piel_data$na_day)
na_person <- c(piel_data$na_person, piel_data$na_person)
cntx_moment <- c(piel_data$cntx_moment, piel_data$cntx_moment)
cntx_day <- c(piel_data$cntx_day, piel_data$cntx_day)
cntx_person <- c(piel_data$cntx_person, piel_data$cntx_person)
user_id <- piel_data$user_id

mydat <- data.frame(
  sc, valence, na_moment, na_day, na_person, 
  cntx_moment, cntx_day, cntx_person, user_id
)

contrasts(mydat$valence) = contr.sum(2) * -1 # so that "n" is coded as -1
contrasts(mydat$valence)

mod_ancov <- brm(
  sc ~ valence *
    (na_moment + na_day + na_person + cntx_moment + cntx_day + cntx_person) +
    (1 + valence + na_moment + cntx_moment + na_day + cntx_day | user_id),
  data = mydat,
  family = asym_laplace(),
  algorithm = "meanfield"
)

pp_check(mod_ancov)
summary(mod_ancov)
marginal_effects(mod_ancov, "na_moment")
bayes_R2(mod_ancov)
# Estimate   Est.Error      Q2.5     Q97.5
# R2 0.7316375 0.008384582 0.7146675 0.7467794

r2_bayes(mod_ancov)

loo_1 <- loo(mod_ancov)
plot(loo_1)


tab_model(mod_ancov)



# Effect size

delta_t <-
  posterior_samples(mod_ancov, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 15:21, .funs = funs(.^2) ) %>%
  mutate(delta = `b_valence1:na_moment` / sqrt(rowSums(.[15:21]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%         50%       97.5% 
# -0.03140562  0.00596326  0.04892668  

delta_t <-
  posterior_samples(mod_ancov, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 15:21, .funs = funs(.^2) ) %>%
  mutate(delta = `b_valence1:na_day` / sqrt(rowSums(.[15:21]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%         50%       97.5% 
# -0.01474696  0.02723182  0.06685576 

delta_t <-
  posterior_samples(mod_ancov, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 15:21, .funs = funs(.^2) ) %>%
  mutate(delta = `b_valence1:na_person` / sqrt(rowSums(.[15:21]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%        50%      97.5% 
# 0.03777834 0.08605392 0.13619903  

delta_t <-
  posterior_samples(mod_ancov, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 15:21, .funs = funs(.^2) ) %>%
  mutate(delta = `valence1:cntx_moment` / sqrt(rowSums(.[15:21]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%        50%      97.5% 
# 0.03777834 0.08605392 0.13619903 

delta_t <-
  posterior_samples(mod_ancov, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 15:21, .funs = funs(.^2) ) %>%
  mutate(delta = `valence1:cntx_day` / sqrt(rowSums(.[15:21]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%        50%      97.5% 
# 0.03777834 0.08605392 0.13619903 

delta_t <-
  posterior_samples(mod_ancov, pars = c("^b_", "sd_", "sigma") ) %>% # taking the square of each variance component
  mutate_at(.vars = 15:21, .funs = funs(.^2) ) %>%
  mutate(delta = `valence1:cntx_person` / sqrt(rowSums(.[15:21]) ) )
quantile(delta_t$delta, c(.025, 0.5, 0.975))
# 2.5%        50%      97.5% 
# 0.03777834 0.08605392 0.13619903 


# Model without interaction

mod2_ancov <- brm(
  sc ~ valence + 
    (na_moment + na_day + na_person + cntx_moment + cntx_day + cntx_person) +
    (1 + valence + na_moment + cntx_moment + na_day + cntx_day | user_id),
  data = mydat,
  family = asym_laplace(),
  algorithm = "meanfield"
)

loo_2 <- loo(mod2_ancov)
m1 <- add_criterion(m1, "loo")


comp <- loo_compare(loo_1, loo_2)
print(comp, digits = 2)
#             elpd_diff se_diff 
# mod2_ancov     0.00      0.00
# mod_ancov  -1536.57     42.61


mod3_ancov <- brm(
  sc ~ valence + na_moment + na_day + na_person +
    (1 + valence + na_moment + na_day | user_id),
  data = mydat,
  family = asym_laplace(),
  algorithm = "meanfield"
)

loo_3 <- loo(mod3_ancov)

comp <- loo_compare(loo_2, loo_3)
print(comp, digits = 2)
#            elpd_diff se_diff
# mod3_ancov    0.00      0.00
# mod2_ancov -145.93     42.51


mod4_ancov <- brm(
  sc ~ valence + na_day + na_person +
    (1 + valence + na_day | user_id),
  data = mydat,
  family = asym_laplace(),
  algorithm = "meanfield"
)

loo_4 <- loo(mod4_ancov)
comp <- loo_compare(loo_3, loo_4)
print(comp, digits = 2)


mod0_ancov <- brm(
  sc ~ 1 + (1 | user_id),
  data = mydat,
  family = asym_laplace(),
  algorithm = "meanfield"
)

loo_0 <- loo(mod0_ancov)
comp <- loo_compare(loo_3, loo_0)
print(comp, digits = 2)
#             elpd_diff se_diff 
# mod3_ancov     0.00      0.00
# mod0_ancov -8202.54    135.14




### DIAGNOSTICS
## Check trace plots ##
pdf(file="trace.pdf", width = 12, height = 9)
mcmc_plot(mod3_ancov, type="trace")
dev.off()


## Check histograms plots ##
pdf(file="hist.pdf", width = 12, height = 9)
mcmc_plot(mod3_ancov, type="hist")
dev.off()

## Check autocorrelation ##
pdf(file="ac.pdf", width = 40, height = 9)
mcmc_plot(mod3_ancov, type="acf")
dev.off()

## Check Gelman-Rubin diagnostic ##
gelman.diag(as.mcmc(mod3_ancov)[, 1:21])
