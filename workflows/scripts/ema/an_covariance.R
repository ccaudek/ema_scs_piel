

y <- (c(piel_data$zpsc, -piel_data$znsc))
valence <- c(rep("p", length(piel_data$zpsc)), rep("n", length(piel_data$znsc)))
na_moment <- c(piel_data$na_moment, piel_data$na_moment)
na_day <- c(piel_data$na_day, piel_data$na_day)
na_person <- c(piel_data$na_person, piel_data$na_person)
user_id <- piel_data$user_id

mydat1 <- data.frame(
  y, valence, na_moment, na_day, na_person, user_id
)

mod_na <- lmer(
  y ~ valence * (na_moment + na_day + na_person) +
    (1 + valence + (na_moment + na_day) | user_id),
  data = mydat1,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

summary(mod_na)





y <- (c(piel_data$zpsc, -piel_data$znsc))
valence <- c(rep("p", length(piel_data$zpsc)), rep("n", length(piel_data$znsc)))
cntx_moment <- c(piel_data$cntx_moment, piel_data$cntx_moment)
cntx_day <- c(piel_data$cntx_day, piel_data$cntx_day)
cntx_person <- c(piel_data$cntx_person, piel_data$cntx_person)
user_id <- piel_data$user_id

mydat2 <- data.frame(
  y, valence, cntx_moment, cntx_day, cntx_person, user_id
)

mod_cntx <- lmer(
  y ~ valence * (cntx_moment + cntx_day + cntx_person) +
    (1 + valence + (cntx_moment + cntx_day) | user_id),
  data = mydat2,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

summary(mod_cntx)

MuMIn::r.squaredGLMM(mod_cntx)

mod2_cntx <- lmer(
  y ~ valence + (cntx_moment + cntx_day + cntx_person) +
    (1 + valence + (cntx_moment + cntx_day) | user_id),
  data = mydat2,
  REML = T,
  control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

MuMIn::r.squaredGLMM(mod2_cntx)
