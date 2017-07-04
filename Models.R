# Hierarchical modeling of lupus data
source('lib/reload.R'); reload()
load('data/rda/data.rda')

# Crude mortality rates -----------------------------------------------------

lupus_data %>% 
  group_by(hospid) %>% 
  summarise(mortality = mean(dead, na.rm = T)) -> crude_mortality


# Non-adjusted models -----------------------------------------------------


# Non-Bayesian ------------------------------------------------------------

fit_pool <- glm(dead ~ 1, data = lupus_data, 
                family = binomial(link = 'logit'))

fit_nopool <- glm(dead ~ 0 + hospid, data = lupus_data,
                  family = binomial())

fit_partial <- glmer(dead ~ (1 | hospid), data = lupus_data,
                   family = binomial(link = 'logit'))


# Bayesian -----------------------------------------------------------------
options(mc.cores = 3)

SEED <- 201
summary_stats <- function(posterior){
  x <- plogis(posterior)
  t(apply(x, 2, quantile, probs = c(0, .1, .25, .5, .75,.9, 1)))
}

wi_prior <- normal(-1,1)
fit_pool_b <- stan_glm(dead ~ 1, data=lupus_data, 
                         family=binomial(link = 'logit'),
                         prior_intercept = wi_prior, seed = SEED)
pool <- summary_stats(as.matrix(fit_pool_b))

fit_nopool_b <- stan_glm(dead ~ 0 + factor(hospid), data=lupus_data,
                     family = binomial(link = 'logit'),
                     prior_intercept = wi_prior, 
                     seed = SEED)
nopool <- summary_stats(as.matrix(fit_nopool_b))

fit_partial_b <- stan_glmer(dead ~ (1 | hospid), data=lupus_data,
                          family = binomial(link = 'logit'),
                          prior_intercept = wi_prior, seed = SEED)
partials <- summary_stats(as.matrix(fit_partial_b))

shift_draws <- function(draws) {
  sweep(draws[,-1], MARGIN=1, STATS = draws[,1], FUN='+')
}
bl <- as.data.frame(summary_stats(shift_draws(as.matrix(fit_partial_b))))

ggplot(bl, aes(x = 1:nrow(bl), y =`50%`, ymin = `10%`, ymax = `90%`))+
  geom_pointrange()+geom_point(color='red')

# Case-mix adjusted models ------------------------------------------------

## Non-Bayesian

fit_adj_pool <- glm(dead ~  agecat + payer + 
                      + slecomb_cat + 
                      ventilator + year,
                    data = lupus_data,
                    family = binomial())

fit_adj_nopool <- glm(dead ~ 0 + hospid +
                        agecat + payer + 
                        slecomb_cat + ventilator + year, 
                      data = lupus_data, 
                      family  = binomial())

fit_adj_partial <- glmer(dead ~ (1 | hospid) + 
                           agecat + payer + 
                           slecomb_cat + ventilator + year, 
                         data = lupus_data,
                         family = binomial())

## Bayesian using rstanarm

SEED <- 201
wi_prior <- normal(0,10)
fit_adj_pool_b <- stan_glm(dead ~  agecat + payer + 
                             + slecomb_cat + 
                             ventilator + year,
                           data = lupus_data,
                           family = binomial(),
                           prior_intercept = wi_prior,
                           seed = SEED)
fit_adj_partial_b <- stan_glmer(dead ~ (1 | hospid) + 
                                  agecat + payer + 
                                  slecomb_cat + ventilator + year, 
                                data = lupus_data,
                                family = binomial(),
                                prior_intercept = wi_prior,
                                seed = SEED)

shift_draws <- function(draws) {
  sweep(draws[,-(1:21)], MARGIN=1, STATS = draws[,1], FUN='+')
}

alphas <- shift_draws(as.matrix(fit_adj_partial_b))
summary_stats(alphas)

logit <- function(x) log(x/(1 - x))

save(fit_pool, fit_nopool, fit_partial, fit_pool_b, fit_partial_b, fit_adj_pool, fit_adj_nopool, fit_adj_partial, fit_adj_pool_b, fit_adj_partial_b, file='data/rda/modelResults.rda', compress=T)
