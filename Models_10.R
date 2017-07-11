# Models for data with hospitals restricted to at least 10 admissions

source('lib/reload.R'); reload()
load(file.path(datadir, 'data/rda/data.rda'))
options(mc.cores = parallel::detectCores())

SEED <- 300
wi_prior <- normal(0,10)
qs <- c(0,.1,.25,.5,.75,.9,1)

data_used <- lupus_data_10
# Unadjusted mortality rates ----------------------------------------------

fit_nopool <- glm(dead ~ 0 + hospid, data=data_used, 
                  family = binomial())
fit_ppool_b <- stan_glmer(dead ~ (1 | hospid), data=data_used, 
                          family = binomial(),
                          prior_intercept = wi_prior,
                          seed = SEED)

# quantile(plogis(mean(predict(fit_ppool))+ranef(fit_ppool_b)$hospid[,1])*100,qs) %>% 
#   round(2) %>% as.data.frame() %>% rownames_to_column('Percentile') %>% 
#   setNames(c('Percentile','Mortality Rate')) -> bl

# Adjusting for case-mix -------------------------------------------------

fit_nopool_adj <- glm(dead ~ 0 + hospid + 
                        agecat + payer + slecomb_cat + 
                        ventilator + year,
                      data = data_used,
                      family = binomial())
fit_ppool_adj_b <- stan_glmer(dead ~ (1|hospid) +
                                agecat + payer + slecomb_cat + 
                                ventilator + year,
                              data = data_used,
                              family = binomial(),
                              prior_intercept = wi_prior,
                              seed = SEED)


# Adjust for case-mix, group across hospitals and years -------------------

fit_nopool_adj_yr <- fit_nopool_adj
fit_ppool_adj_yr_b <- stan_glmer(dead ~ (1|hospid) + (1|year) +
                                   agecat + payer + slecomb_cat + ventilator, 
                                 data = data_used,
                                 family = binomial(),
                                 prior_intercept = wi_prior,
                                 seed = SEED)

save(fit_nopool,  fit_ppool_b, fit_nopool_adj,  fit_ppool_adj_b, fit_nopool_adj_yr, 
     fit_ppool_adj_yr_b, 
     file = file.path(datadir,'data','rda','Models_10.rda'),
     compress = T)

# Reliability-adjusted mortality, following Dimick ------------------------

## Compute the average mortality across hospitals, on logit scale

d <- data_used %>% select(dead, hospid, year, agecat, payer, slecomb_cat, 
                           ventilator) %>% 
  filter(complete.cases(.))


out_ram1 <- ram(fit_ppool_adj_b, d)

p_yr <- colMeans(posterior_linpred(fit_ppool_adj_yr_b))
avg_p_yr <- d %>% mutate(p_yr = p_yr) %>% group_by(year) %>% summarise(avg = mean(p_yr))


ram_adj_yr_b <- plogis(outer(ranef(fit_ppool_adj_yr_b)$hospid[,1],as.numeric(avg_p_yr$avg),'+')) %>% 
  as.data.frame() %>% setNames(as.character(2002:2011))

apply(ram_adj_yr_b*100,2,quantile,qs) %>% as.data.frame() %>% 
  rownames_to_column("Percentile") %>%  as_tibble() -> out_ram2

library(openxlsx)
out <- list("Within hospital" = out_ram1, "Within hospital+year" = out_ram2)
openxlsx::write.xlsx(out, file = "docs/At_least_10.xlsx", asTable = T,
                     withFilter = F)
