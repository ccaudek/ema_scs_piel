library(bestNormalize)
library(sjPlot)
library(miceRanger)
library(nlme)

# Compute the coefficient of variation
compute_cv <- function(x) {
  sd(x) / mean(x)
}

# Applying the computation on the data frame
cv_results <- d %>%
  group_by(user_id, bysubj_day) %>%
  filter(n() > 1) %>%  # Ensure at least two values per group
  summarise(cv_neg_aff = compute_cv(neg_aff), .groups = 'drop')

# View the results
hist(cv_results$cv_neg_aff)

# Remove outliers -- si può fare meglio.
cv_results$cv_neg_aff <- ifelse(
  cv_results$cv_neg_aff < -20, NA, cv_results$cv_neg_aff
)

cv_results$cv_neg_aff <- ifelse(
  cv_results$cv_neg_aff > 20, NA, cv_results$cv_neg_aff
)

# Impute missing values
miceObj <- miceRanger(
  cv_results
  , m=1
  , returnModels = TRUE
  , verbose=FALSE
)

imputed_data <- completeData(miceObj) |> 
  as.data.frame()
colnames(imputed_data) <- colnames(cv_results)
summary(imputed_data)

d$sc <- as.vector(scale(d$psc - d$nsc))

bysubj_day_sc_df <- d |> 
  group_by(user_id, bysubj_day) |> 
  summarize(
    sc = mean(sc, na.rm = T),
    context = mean(context)
  ) |> 
  ungroup()

df <- inner_join(imputed_data, bysubj_day_sc_df, by = c("user_id", "bysubj_day"))
summary(df)

result = bestNormalize(df$cv_neg_aff)
df$cv <- result$x.t
hist(df$cv)

fm <- lmer(
  cv_neg_aff ~ sc + bysubj_day + context + (1 | user_id),
  data = df,
  REML = TRUE,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
  
)
summary(fm)
plot(fm)
plot_model(fm, type="pred", terms="sc")
performance::r2_nakagawa(fm)

vc_model <- lme(
  cv_neg_aff ~ sc + bysubj_day + context, 
  random = ~ 1 + bysubj_day | user_id, 
  method = "REML", 
  data = df
)
summary(vc_model)
plot(vc_model)
plot_model(vc_model, type="pred", terms="sc")
performance::r2_nakagawa(vc_model)


# Marginalize with respect to bysubj_day
mydat <- df |> 
  group_by(user_id) |> 
  summarize(
    cv = mean(cv_neg_aff, na.rm = T, trim = 0.1),
    sc = mean(sc, na.rm = T, trim = 0.1),
    context = mean(context)
  )

mydat <- mydat |> 
  mutate(
    cntx = as.vector(scale(context))
  )

result = bestNormalize(mydat$cv)
mydat$cvn <- result$x.t
hist(mydat$cvn)

hist(mydat$cv)

fm <- lm(cvn ~ sc + cntx, data = mydat)
summary(fm)
plot(fm)

plot_model(
  fm, 
  type="pred", 
  terms="sc",
  title="Predicted values of CV(Dialy Negative Affect)"
  ) +
  xlab("State Self Compassion") +
  ylab("Coefficient of Variation\n(Daily Negative Affect)") 


# Bayesian approach

# The distribution of cv_neg_aff is very bad. So it is better to collapse over
# day.
mod1 <- brm(
  cv_neg_aff ~ sc + bysubj_day + context + (bysubj_day | user_id),
  data = df,
  family = asym_laplace(),
  backend = "cmdstanr",
  cores = 8,
  threads = threading(2),
  silent = 2
)
pp_check(mod1) + xlim(-2, 2)
bayes_R2(mod)
summary(mod)
conditional_effects(mod, "sc")



mydat$zsc <- as.vector(scale(mydat$sc))

mod <- brm(
  cv ~ zsc + control,
  data = mydat,
  family = student(),
  backend = "cmdstanr",
  cores = 4,
  threads = threading(2),
  silent = 2
)
pp_check(mod) + xlim(-2, 2)
bayes_R2(mod)
summary(mod)
conditional_effects(mod, "zsc")

loo(mod)

# All'aumentare della SC di stato, la variazione dell'umore all'interno del
# giorno diminuisce.