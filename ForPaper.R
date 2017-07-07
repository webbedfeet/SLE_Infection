# Final tables and figures for paper

source('lib/reload.R'); reload()
load(file.path(datadir, 'data/rda/data.rda'))
options(mc.cores = parallel::detectCores())

SEED <- 300
wi_prior <- normal(0,10)
qs <- c(0,.1,.25,.5,.75,.9,1)
# Unadjusted mortality rates ----------------------------------------------

fit_nopool <- glm(dead ~ 0 + hospid, data=lupus_data, 
                  family = binomial())
fit_ppool <- glmer(dead ~ (1 | hospid), data = lupus_data, 
                   family = binomial())
fit_ppool_b <- stan_glmer(dead ~ (1 | hospid), data=lupus_data, 
                          family = binomial(),
                          prior_intercept = wi_prior,
                          seed = SEED)

quantile(plogis(mean(predict(fit_ppool))+ranef(fit_ppool_b)$hospid[,1])*100,qs) %>% 
  round(2) %>% as.data.frame() %>% rownames_to_column('Percentile') %>% 
  setNames(c('Percentile','Mortality Rate')) -> bl

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

fit_ppool_adj_1 <- glmer(dead ~ (1|hospid) +
                           agecat + payer + slecomb_cat + 
                           ventilator + year,
                         data = lupus_data %>% filter(payer != '3'),
                         family = binomial())

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
    bind_cols('Estimate' = rep('No pooling',n),.)
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
    bind_cols('Estimate' = rep(nm,n),.) 
  return(out)
}

# Output tables -----------------------------------------------------------

qs <- c(0,.1,.25,.5,.75,.9,1)
n <- length(qs)
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
), check.names=F)

library(openxlsx)

out <- list('Crude rates' = out1, "Case-mix adjusted" = out2, "Case-mix grouped by hospital and year" = out3)
write.xlsx(out, file='docs/SLE Infection Tables.xlsx')

d <- lupus_data %>% select(dead, hospid, year, agecat, payer, slecomb_cat, 
                           ventilator) %>% 
  filter(complete.cases(.))

pred_data <- d %>% mutate(
  p_adj = predict(fit_ppool_adj, ., type='response'),
  p_adj_b = colMeans(posterior_predict(fit_ppool_adj_b, .)),
  # p_adj_yr = predict(fit_ppool_adj_yr,., type='response'),
  # p_adj_yr_b = colMeans(posterior_predict(fit_ppool_adj_yr_b, .)),
  p_nopool_adj = predict(fit_nopool_adj,., type='response'),
  p_nopool_adj_yr = predict(fit_nopool_adj_yr, ., type='response')
) %>% 
  group_by(hospid) %>% 
  summarise_at(vars(starts_with('p_')), mean) %>% 
  select(-hospid) %>% 
  apply(2, quantile, qs) %>% 
  round(4) %>% apply(2,'*',100)

qs2 = c(0.05, 0.5, 0.95)
d %>% mutate(
  p_adj_yr = predict(fit_ppool_adj_yr,., type='response'),
  p_adj_yr_b = colMeans(posterior_predict(fit_ppool_adj_yr_b, .))
) %>% 
  group_by(hospid, year) %>% 
  summarise_at(vars(starts_with('p_')), mean) %>% ungroup() -> bl
bl %>% nest(-year) %>% 
  arrange(year) %>% mutate(d1 = map(data, ~apply(.[,-1], 2, quantile, qs2))) %>% 
  mutate(d1 = map(d1, ~as.data.frame(.))) %>% 
  mutate(d1 = map(d1, ~rownames_to_column(., 'Percentiles'))) %>% 
  select(year, d1) %>% unnest() %>% 
  mutate(percs = as.numeric(str_replace(Percentiles,'%',''))/100)-> bl2
bl2 %>% gather(variable, value, starts_with('p_')) %>% 
  spread(year, value) %>% 
  arrange(variable, percs) %>% 
  # select(-percs) %>% 
  select(variable, Percentiles,percs, `2002`:`2011`)-> bl3

bl3 %>% gather(year, value, -(variable:percs)) %>% 
  select(-percs) %>% 
  spread(Percentiles, value) %>% 
  ggplot(aes(x=as.numeric(year), y = `50%`, ymin=`5%` , ymax = `95%`))+
    geom_point()+geom_pointrange()+facet_wrap(~variable, nrow=1)
pred_data %>% select(starts_with('p_')) %>% apply( 2, quantile, qs) %>% 
  round(4)


# Reliability-adjusted mortality, following Dimick ------------------------

## Compute the average mortality across hospitals, on logit scale

d <- lupus_data %>% select(dead, hospid, year, agecat, payer, slecomb_cat, 
                           ventilator) %>% 
  filter(complete.cases(.))

p = predict(fit_ppool_adj, d)
avg_p <- mean(p)

ram_adj <- plogis(avg_p + ranef(fit_ppool_adj)$hospid[,1])
ram_adj_b <- plogis(avg_p + ranef(fit_ppool_adj_b)$hospid[,1])
as.data.frame(round(quantile(ram_adj_b*100,qs),2)) %>% 
  rownames_to_column('Percentile') %>% 
  setNames(c('Percentile', 'Mortality Rate')) -> out_ram1

round(quantile(100*ram_adj_b, qs),2)

p_yr <- predict(fit_ppool_adj_yr, d)
avg_p_yr <- d %>% mutate(p_yr = p_yr) %>% group_by(year) %>% summarise(avg = mean(p_yr))


ram_adj_yr_b <- plogis(outer(ranef(fit_ppool_adj_yr_b)$hospid[,1],as.numeric(avg_p_yr$avg),'+')) %>% 
  as.data.frame() %>% setNames(as.character(2002:2011))

round(apply(ram_adj_yr_b*100,2,quantile,qs),2) %>% as.data.frame() %>% 
  rownames_to_column("Percentile") %>%  as_tibble() -> out_ram2
