# Hierarchical modeling of lupus data
source('lib/reload.R'); reload()
load(file.path(datadir,'data/rda/data.rda'))

options(mc.cores = parallel::detectCores() - 1)
# Crude mortality rates -----------------------------------------------------

lupus_data %>% 
  group_by(hospid) %>% 
  summarise(mortality = mean(dead, na.rm = T)) -> crude_mortality


# Non-adjusted models -----------------------------------------------------


# Non-Bayesian ------------------------------------------------------------

fit_pool <- glm(dead ~ 1, data = lupus_data, 
                family = binomial(link = 'logit'))
fit_pool_ann <- glm(dead ~ year, data=lupus_data,
                    family=binomial())

fit_nopool <- glm(dead ~ 0 + hospid, data = lupus_data,
                  family = binomial())

fit_partial <- glmer(dead ~ (1 | hospid), data = lupus_data,
                   family = binomial(link = 'logit'))
fit_partial_ann <- glmer(dead ~ (1 | hospid:year), data = lupus_data,
                     family = binomial(link = 'logit'))


# Bayesian -----------------------------------------------------------------

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

fit_pool_ann_b <- stan_glm(dead~ year, data=lupus_data,
                           family=binomial(),
                           prior_intercept = wi_prior, seed=SEED)

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

fit_adj_partial_yr <- glmer(dead ~ (1 | hospid:year)+
                              agecat + payer + 
                              slecomb_cat + ventilator,
                            data=lupus_data, 
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

save(fit_pool, fit_nopool, fit_partial, fit_pool_b, fit_partial_b, 
     fit_adj_pool, fit_adj_nopool, fit_adj_partial, fit_adj_pool_b, 
     fit_adj_partial_b, 
     file=file.path(datadir,'data/rda/modelResults.rda'), compress=T)

# Level I regression ------------------------------------------------------

## Check to make sure features are unique at group-level 
bl <- lupus_data %>% group_by(hospid) %>% 
  summarise_at(vars(hosp_region, bedsize, teach, highvolume), 
               funs(length(unique(.))))
bl %>% select(-hospid) %>% 
  purrrlyr::by_row(sum, .collate='cols', .to='totals') -> bl2
bl2 = cbind(bl$hospid, bl2)
bl2 %>% count(totals)

### There are 293 hospitals where all the features are truly group-level. 
### For the rest, there is at least one feature which changes within hospital, but
### not within year. 


