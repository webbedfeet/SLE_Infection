# Final tables and figures for paper

source('lib/reload.R'); reload()
load(file.path(datadir, 'data/rda/data.rda'))
options(mc.cores = parallel::detectCores())

SEED <- 300
wi_prior <- normal(0,10)
qs <- c(0,.1,.25,.5,.75,1)
# Unadjusted mortality rates ----------------------------------------------

fit_nopool <- glm(dead ~ 0 + hospid, data=lupus_data, 
                  family = binomial())
fit_ppool <- glmer(dead ~ (1 | hospid), data = lupus_data, 
                   family = binomial())
fit_ppool_b <- stan_glmer(dead ~ (1 | hospid), data=lupus_data, 
                          family = binomial(),
                          prior_intercept = wi_prior,
                          seed = SEED)


# Adjusting for case-mix -------------------------------------------------

fit_nopool_adj <- glm(dead ~ 0 + hospid + 
                        agecat + payer + slecomb_cat + 
                        ventilator + year,
                      data = lupus_data,
                      family = binomial())
fit_ppool_adj <- glmer(dead ~ (1|hospid) +
                         agecat + payer + slecomb_cat + 
                         ventilator + year,
                       data = lupus_data,
                       family = binomial())
fit_ppool_adj_b <- stan_glmer(dead ~ (1|hospid) +
                                agecat + payer + slecomb_cat + 
                                ventilator + year,
                              data = lupus_data,
                              family = binomial(),
                              prior_intercept = wi_prior,
                              seed = SEED)



# Adjust for case-mix, group across hospitals and years -------------------

fit_nopool_adj_yr <- fit_nopool_adj
fit_ppool_adj_yr <- glmer(dead ~ (1|hospid) + (1|year) +
                            agecat + payer + slecomb_cat + ventilator, 
                          data =lupus_data,
                          family=binomial())
fit_ppool_adj_yr_b <- stan_glmer(dead ~ (1|hospid) + (1|year) +
                                   agecat + payer + slecomb_cat + ventilator, 
                                 data =lupus_data,
                                 family=binomial(),
                                 prior_intercept = wi_prior,
                                 seed= SEED)

save(fit_nopool, fit_ppool, fit_ppool_b, fit_nopool_adj, fit_ppool_adj, fit_ppool_adj_b, fit_nopool_adj_yr, fit_ppool_adj_yr, fit_ppool_adj_yr_b, 
     file = file.path(datadir,'data','rda','ForPaper.rda'),
     compress = T)

# Output generating functions ---------------------------------------------

ests_nopool <- function(m){
  hospid <- tidy(m) %>% filter(str_detect(term,'hospid')) %>% select(term,estimate)
  year <- tidy(m) %>% filter(str_detect(term,'year')) %>% 
    select(term,estimate) %>% 
    bind_rows(tibble(term='year2002',estimate=0),.)
  blah <- data.frame(plogis(outer(hospid$estimate, year$estimate,'+')))
  names(blah) <- as.character(seq(2002,2011,by=1))
  out <- apply(blah*100, 2, quantile, qs) %>% 
    round(2) %>% as.data.frame(check.names=F) %>% 
    rownames_to_column('Percentile') %>% 
    bind_cols('Estimate' = rep('No pooling',6),.)
  return(out)
}

ests_ppool <- function(m, nm='Partial pooling'){
  intercept <- fixef(m)[1]
  hospid <- ranef(m)$hospid[,1]
  year <- ranef(m)$year[,1]
  blah <- plogis(outer(hospid, year, '+')+intercept)
  names(blah) <- as.character(seq(2002,2011,by=1))
  out <- apply(blah*100, 2, quantile, qs) %>% 
    round(2) %>% as.data.frame(check.names=F) %>% 
    setNames(as.character(seq(2002,2011,by=1)))%>% 
    rownames_to_column('Percentile') %>% 
    bind_cols('Estimate' = rep(nm,6),.) 
  return(out)
}

# Output tables -----------------------------------------------------------

out1 <- data.frame(rbind(
  'No pooling' = coef(fit_nopool) %>% plogis() %>% quantile(qs) %>% round(2),
  'Partial pooling' = coef(fit_ppool)$hospid %>% pull(`(Intercept)`) %>% plogis() %>% 
    quantile(qs) %>% round(2),
  'Bayesian' = coef(fit_ppool_b)$hospid %>%pull(`(Intercept)`) %>% plogis() %>% 
    quantile(qs) %>% round(2) 
), check.names=F) %>% 
  rownames_to_column("Estimate")

out2 <- data.frame(rbind(
  'No pooling' = tidy(fit_nopool_adj) %>% 
    filter(str_detect(term, 'hospid')) %>% 
    mutate(p = plogis(estimate)) %>% 
    pull(p) %>% quantile(qs) %>% round(2),
  'Partial pooling' = (ranef(fit_ppool_adj)$hospid+fixef(fit_ppool_adj)[1]) %>% 
    mutate(p = plogis(`(Intercept)`)) %>% 
    pull(p) %>% quantile(qs) %>% round(2),
  "Bayesian" = coef(fit_ppool_adj_b)$hospid %>% mutate(p = plogis(`(Intercept)`)) %>% 
    pull(p) %>% quantile(qs) %>% round(2)
), check.names=F) %>% rownames_to_column("Estimate")

out3 <- data.frame(rbind(
  ests_nopool(fit_nopool_adj_yr),
  ests_ppool(fit_ppool_adj_yr),
  ests_ppool(fit_ppool_adj_yr_b, 'Bayesian')
))

library(openxlsx)

out <- list('Crude rates' = out1, "Case-mix adjusted" = out2, "Case-mix grouped by hospital and year" = out3)
write.xlsx(out, file='docs/SLE Infection Tables.xlsx')
