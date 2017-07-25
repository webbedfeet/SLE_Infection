# Models for looking at hospital performance in non-lupus admissions
source('lib/reload.R'); reload()

load(file.path(datadir,'data','rda','data.rda'))

options(mc.cores = parallel::detectCores())

SEED <- 300
wi_prior <- normal(0,10)
qs <- c(0,.1,.25,.5,.75,.9,1)
data_used <- nonlupus_data


## Descriptives for elix_score
bl <- nonlupus_data %>% 
  mutate(elix_cat = cut(nonlupus_data$elix_score, 
                unique(quantile(nonlupus_data$elix_score, seq(0,1,by=0.05),
                                             na.rm=T)),
                include.lowest = T, right=F)) %>% 
  group_by(elix_cat) %>% 
  summarise(p = mean(dead), x = mean(elix_score))

                  

fit_ppool_adj_nonlupus <- stan_glmer(dead ~ (1|hospid) +
                                agecat + payer + elix_score + 
                                ventilator + year,
                              data = nonlupus_data,
                              family = binomial(),
                              prior_intercept = wi_prior,
                              seed = SEED)

 fit_nopool_adj_nonlupus <- glm(dead~-1+hospid+agecat + payer + 
                                 slecomb_cat + ventilator + year, 
                               data = lupus_data,
                               family=binomial())


d <- nonlupus_data %>% select(dead, hospid, agecat, payer, elix_score, ventilator,
                              year) %>% 
  filter(complete.cases(.))
 
ram(fit_ppool_adj_nonlupus,d)