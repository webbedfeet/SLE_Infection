# Code for modeling the effect of being a lupus patient on survival when admitted for sepsis

source('lib/reload.R'); reload()
load(file.path(datadir, 'data','rda','data.rda'))

fn_odds_ratio <- function(d){
  counts = d %>% count(dead, lupus)
  n = counts$n
  if(length(n) < 4) return(NA)
  return(n[1]*n[4]/n[2]/n[3])
}
# No pooling model --------------------------------------------------------


## Empirical odds ratios

all_data %>% nest(-hospid) %>% 
  mutate(N = map_int(data, ~nrow(count(dead, lupus)))) %>% 
  select(hospid, N) -> counts
## No adjustment

mod_nopool <- all_data %>% select(dead, hospid, lupus, agecat) %>% 
  mutate(lupus = as.factor(lupus),
         dead = as.factor(dead)) %>% 
  nest(-hospid) %>% 
  mutate(mod = map(data, ~glm(dead~lupus + agecat, data=., family = binomial())),
         lupus_logodds = map_dbl(mod, ~tidy(.)[2,2]))

plausible_OR <- all_data %>% nest(-hospid) %>% 
  mutate(n = map_int(data, ~nrow(count(., lupus, dead)))) %>% 
  select(-data) %>% filter(n == 4)

mod_nopool2 <- all_data %>% filter(hospid %in% plausible_OR$hospid) %>% 
  select(dead, hospid, lupus, agecat, ventilator) %>% 
  mutate(lupus = as.factor(lupus),
         dead = as.factor(dead)) %>% 
  nest(-hospid) %>% 
  mutate(mod = map(data, ~glm(dead~lupus + agecat, data=., 
                              family = binomial())),
         lupus_logOR = map_dbl(mod, ~tidy(.)[2,2]), 
         empirical_OR = map_dbl(data, fn_odds_ratio),
         empirical_logOR = log(empirical_OR)) %>% 
  select(-data, -mod)

mod_nopool2 %>% filter(lupus_logOR < 10) %>% ggplot(aes(lupus_logOR)) + 
  geom_density()


mod_ppool <- glmer(dead ~ lupus + (lupus|hospid) + agecat, 
                   data = all_data, family = binomial())

ggplot(coef(mod_ppool)$hospid, aes(lupus))+
  geom_histogram(binwidth=0.05) + 
  xlab('Log odds ratio for lupus')

mod_ppool2 <- glmer(dead ~ lupus + ventilator + 
                      (lupus * ventilator|hospid) + agecat, 
                    data=all_data, family=binomial())


all_data %>% select(dead, lupus, hospid) %>% 
  nest(-hospid) %>% 
  mutate(odds_ratio = map_dbl(data, fn_odds_ratio)) -> OR




# Partial pooling ---------------------------------------------------------

mod_partial <- glmer(dead ~ (lupus|hospid), data=all_data,
                          family = binomial)
next_mod <- sampling(mod_partial, iter = 2000)



# Using ML to get to predicted probability of death -----------------------

# library(randomForest)
# 
# train_set <- all_data %>% filter(lupus == 0) %>% 
#   select(dead, ventilator, agecat, slecomb, male, zipinc_qrtl) %>% 
#   filter(complete.cases(.))
# 
# rfor_train <- randomForest(dead ~ ventilator + agecat + 
#                              slecomb + male + zipinc_qrtl, 
#                            data =train_set, 
#                            importance=TRUE)
# 
# test_set <- filter(all_data, lupus == 1)
# predicted_death = predict(rfor_train, 
#                           newdata = filter(all_data, lupus==1))
# 
# bl <- cbind(filter(all_data, lupus == 1), 'pred' = predicted_death) 
# bl <- bl %>% select(agecat, dead, lupus, male, pred, hospid) %>% 
#   filter(!is.na(pred))
# bl <- bl %>% mutate(pred_cut = cut(pred, seq(0,0.5,0.1), include.lowest = TRUE),
#                     pred_cutpt = ((as.numeric(pred_cut)-0.5)/10))
# bl %>% group_by(pred_cutpt) %>% summarise(m = mean(dead), 
#                                           n = n(),
#                                           se = sqrt(m*(1-m)/n),
#                                           ymin = m - 2*se,
#                                           ymax = m + 2*se) %>% 
#   ggplot(aes(pred_cutpt, m, ymin = ymin, ymax = ymax))+
#   geom_pointrange()+geom_abline(linetype=2)+
#   xlab('Predicted probability of death')+
#   ylab('Observed proportion of death')+
#   scale_x_continuous(breaks = seq(0.05, 0.45, by=0.1), 
#                      labels = levels(bl$pred_cut))
# 
# bl2 <- bl %>% group_by(hospid) %>% summarize(m = mean(dead), p = mean(pred), chi = sum(dead)/sum(pred))
# 
# 
# ## Calibration
# 
# out <- tibble(p = predict(rfor_train), dead = train_set$dead)
# p = predict(rfor_train)
# dead = train_set$dead
# 
# cal_data <- tibble(p = p, dead = dead)
# cal_data <- cal_data %>% mutate(p_cut = cut(p, seq(0,.5,.1), include.lowest = T),
#                                 p_cutnum = ((as.numeric(p_cut)-0.5)/10))
# 
# cal_data_summ <- cal_data %>% group_by(p_cutnum) %>% 
#   summarize(m = mean(dead),
#             n = n(),
#             se = sqrt(m*(1-m)/n),
#             ymin = m - 2*se,
#             ymax = m + 2*se)
# ggplot(cal_data_summ, aes(x=p_cutnum, y = m, ymin=ymin, ymax=ymax))+
#   geom_pointrange()+
#   geom_abline(linetype=2)

library(xgboost)

train_set <- train_set %>% select(dead, ventilator, agecat, slecomb, male, zipinc_qrtl)
dtrain <- xgb.DMatrix(model.matrix(~.-1, train_set)[,-1], label = train_set$dead)
test_set1 = model.matrix(~.-1,test_set %>% select(dead, ventilator, agecat, slecomb, male, zipinc_qrtl))


dtest = xgb.DMatrix(test_set1[,-1], label = test_set1[,1])
params <- list(eta = 0.3, objective = 'reg:logistic')
xgmod = xgb.train(params, dtrain, nrounds=10)
p = predict(xgmod, dtest)

set.seed(102)
p2 = xgb.cv(params, dtrain, nrounds=10, nfold=10, prediction=T)$pred
cal = tibble(dead = train_set$dead, p = p2)
cal <- cal %>% mutate(pcut = cut(p, seq(0,.9,.1), include.lowest = T),
                      pcutnum = ((as.numeric(pcut)-0.5)/10))
cal_summ = cal %>% 
  filter(!is.na(p)) %>% 
  group_by(pcutnum) %>% 
  summarize(m = mean(dead),
            n = n(),
            se = sqrt(m*(1-m)/n),
            ymin = m-2*se,
            ymax = m+2*se)

ggplot(cal_summ, aes(pcutnum, m,  ymin=ymin, ymax=ymax))+
  geom_pointrange()+
  geom_abline(linetype=2)+
  labs(x = 'Predicted probability',
       y = 'Observed proportion')+
  scale_x_continuous(breaks = seq(0.05,0.85,.1),
                     labels = levels(cal$pcut))

predcal <- tibble(dead = test_set1[,'dead'], p = predict(xgmod, dtest))
predcal <- predcal %>% mutate(pcut = cut(p, seq(0,.8,.1), include.lowest = T),
                              pcutnum = ((as.numeric(pcut)-0.5)/10)) 
predcal_summ <- predcal %>% group_by(pcutnum) %>% 
  summarise(m = mean(dead),
            n = n(),
            se = sqrt(m*(1-m)/n),
            ymin = m-2*se,
            ymax = m+2*se)

ggplot(predcal_summ, aes(pcutnum, m, ymin=ymin, ymax=ymax))+
  geom_pointrange()+
  geom_abline(linetype=2)+
  labs(x = 'Predicted probability',
       y = 'Observed proportion')+
  scale_x_continuous(breaks = seq(0.05, .75, .1),
                     labels = levels(predcal$pcut)) 
                     


# by hospital -------------------------------------------------------------

## overall model
all_data <- readRDS('data/all_data.rds')
train_set <- all_data %>% filter(lupus == 0) %>% 
  select(dead, agecat, lupus, ventilator, elix_score, male, 
         medicare, medicaid, private, otherins,
          hospid) %>% 
  model.matrix(~.-1, data=.)
dtrain = xgb.DMatrix(train_set[,-1], label = train_set[,1])
test_set <- all_data %>% filter(lupus == 1) %>% 
  select(dead, agecat, lupus, ventilator, elix_score, male, 
         medicare, medicaid, private, otherins,
         hospid) %>% 
  model.matrix(~.-1, data=.)
dtest = xgb.DMatrix(test_set[,-1], label = test_set[,1])
params <- list('eta' = 0.3,
               'objective'= 'reg:logistic',
               'early_stopping_rounds' = 5
)
xgbmodel1 = xgb.train(params, dtrain, nrounds = 20) # Includes hospitals
saveRDS(xgbmodel1, file = 'data/xgb_trained.rds')
pred1 <- predict(xgbmodel1, dtest)

bl <- all_data %>% filter(lupus == 1)

oe_overall <- bl[as.numeric(row.names(test_set)),] %>% 
  select(hospid, dead) %>% filter(complete.cases(.)) %>% 
  cbind(pred = pred1) %>% 
  group_by(hospid) %>% 
  summarise(obs = sum(dead), expect = sum(pred)) %>% 
  mutate(oe_ratio = obs/expect, log_oe = log(oe_ratio))

ggplot(oe_overall, aes(oe_ratio))+ 
  geom_histogram(binwidth=0.3)+
  geom_vline(xintercept = 1) +
  xlab('Observed/Expected ratio by hospital')

predcal <- tibble(pred = pred1, dead = test_set[,'dead']) %>% 
  mutate(predcut = cut(pred, c(seq(0,.6,.1),.8), include.lowest = T),
         predcutnum = ((as.numeric(predcut)-0.5)/10),
         predcutnum = ifelse(predcutnum==.65, .7, predcutnum))
predcal_summ <- predcal %>% group_by(predcutnum) %>% 
  summarize(m = mean(dead),
            n = n(),
            se = sqrt(m*(1-m)/n),
            ymin = m - 2*se,
            ymax = m + 2*se)
ggplot(predcal_summ, aes(predcutnum, m, ymin=ymin, ymax=ymax))+
  geom_pointrange()+
  geom_abline(linetype=2) +
  scale_x_continuous('Predicted probability',
                     breaks = sort(unique(predcal$predcutnum)),
                     labels = levels(predcal$predcut)) +
  ylab('Observed proportion in lupus patients')


# Characterizing bad hospitals --------------------------------------------

bad_hosp <- oe_overall %>% mutate(hospid = as.character(hospid)) %>% 
  filter(oe_ratio > 2) %>% 
  pull(hospid)

all_data <- all_data %>% mutate(bad_ind = hospid %in% bad_hosp,
                                hospid = as.character(hospid))
hosp_data <- all_data %>% select(hospid, bad_ind, highvolume, bedsize, teach, 
                                 northeast, midwest, south, west, rural, 
                                 smallurban, largeurban)

highvol <- all_data %>% group_by(hospid) %>% 
  summarize(new_highvolume = round(mean(highvolume[lupus==1], na.rm=T)))
bed <- all_data %>% group_by(hospid) %>% 
  summarize(new_bedsize = round(mean(bedsize[lupus == 1], na.rm = T)))

teach = all_data %>% group_by(hospid) %>% 
  summarize(new_teach = round(mean(teach[lupus == 1], na.rm = T)))

hosp_data <- hosp_data %>% left_join(highvol) %>% 
  left_join(bed) %>% 
  left_join(teach) %>% 
  select(hospid, bad_ind, northeast:west, new_highvolume:new_teach) %>% 
  distinct() %>% 
  mutate(new_bedsize = as.factor(new_bedsize),
         new_bedsize = as.factor(new_bedsize),
         new_teach = as.factor(new_teach))

hosp_data %>% crosstab(new_bedsize, bad_ind, percent='row')
hosp_data %>% crosstab(new_teach, bad_ind, percent='row')

hosp_data %>% mutate(region = northeast + 2 * midwest + 3 * south + 4 * west,
                     region = factor(region, labels = c('northeast','midwest',
                                                        'south','west'))) %>% 
  crosstab(region, bad_ind, percent = 'row')
###############################################################################
## Predict by hospital

all_data_split<- all_data %>% mutate(hospid = as.character(hospid)) %>% 
  split(.,.$hospid) # hospid was a factor with an extra level

get_obs_exp <- function(d){
  require(xgboost)
  params <- list('eta' = 0.3,
                 'objective'= 'reg:logistic',
                 'early_stopping_rounds' = 5
                 )
  train_data <- d %>% filter(lupus==0) %>% 
    select(dead, agecat,  ventilator,
           slecomb, male) %>% 
    model.matrix(~. - 1, .)
  test_data <- d %>% filter(lupus == 1) %>% 
    select(dead, agecat,  ventilator,
           slecomb, male) %>% 
    model.matrix(~. - 1, .)
  dtrain <-  xgb.DMatrix(train_data[,-1], label = train_data[,1])
  dtest <- xgb.DMatrix(test_data[,-1], label=test_data[,1])
  xgbmod <- xgb.train(params, dtrain, nrounds = 20)
  pred <- predict(xgbmod, dtest)
  pred_cal <- tibble(pred=pred, dead = test_data[,1]) 
  pred_cal_summ <- pred_cal %>% 
    summarize(obs = sum(dead), expect = sum(pred))
  return(pred_cal_summ)
}

obs_exp <- lapply(all_data_split, get_obs_exp)
obs_exp <- bind_rows(obs_exp, .id = 'hospid')
obs_exp <- obs_exp %>% mutate(oe_ratio = obs/expect, log_oe = log(oe_ratio))
ggplot(obs_exp, aes(oe_ratio))+geom_histogram()
ggplot(obs_exp, aes(oe_ratio)) + geom_density() + geom_rug()

saveRDS(all_data, file = 'data/all_data.rds')
save(obs_exp, oe_overall, mod_ppool, all_data, file = 'data/lupuseffect.rda')
