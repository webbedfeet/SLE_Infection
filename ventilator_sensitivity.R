# Checking sensitivity to including the ventilator variable in the case-mix
# adjustment


# Preamble ----------------------------------------------------------------

source('lib/reload.R'); reload()
load(file.path(datadir, 'data','rda','data.rda'))
options(mc.cores = parallel::detectCores())
wi_prior <- normal(0,10)
SEED = 301 

# Summary of ventilator status --------------------------------------------

lupus_data %>% 
  group_by(hospid) %>% 
  summarise(n = n(),vent = mean(ventilator=='1', na.rm=T)) %>%   # ventilator is a factor
  ggplot(aes(n, vent))+geom_point()+geom_smooth()

# Modeling full data without ventilator adjustment ------------------------

load(file.path(datadir,'data','rda','modelResults.rda')) # This is using n=5 cutoff

fit_partial <- update(fit_adj_partial_b, .~. - ventilator)

out_5 <- left_join(
  ram(fit_adj_partial_b, lupus_data %>% filter(complete.cases(.))),
  ram(fit_partial, lupus_data %>% filter(complete.cases(.))),
  by = 'Percentile') %>% 
  setNames(c("Percentile", "With ventilator", "Without ventilator"))

# Modeling reduced data without ventilator adjustment ---------------------

load(file.path(datadir,'data','rda','forPaper.rda')) # This is using n=10 cutoff

fit_partial_10 <- update(fit_ppool_adj_b, . ~ . - ventilator)

out_10 <- left_join(
  ram(fit_ppool_adj_b, lupus_data_10 %>% filter(complete.cases(.))),
  ram(fit_partial_10, lupus_data_10 %>% filter(complete.cases(.))),
  by = 'Percentile') %>% 
  setNames(c("Percentile", "With ventilator", "Without ventilator"))

out <- list("n=5" = out_5, "n=10" = out_10)
openxlsx::write.xlsx(out, file='docs/ventilator.xlsx')
