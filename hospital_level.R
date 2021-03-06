# Ecological analysis at hospital level

ProjTemplate::reload()
load(file.path(datadir, 'data','rda','data.rda'))

## relation between lupus and non-lupus mortality
ggplot(hosp_data, aes(x = lupus_mortality, y = nonlupus_mortality))+
  geom_point()+
  geom_abline()+
  geom_vline(xintercept = 0.2, linetype = 2)+ 
  geom_hline(yintercept = 0.2, linetype = 2)

hosp_data <- hosp_data %>% 
  mutate(excess_risk = (lupus_mortality - nonlupus_mortality)/nonlupus_mortality)

ggplot(hosp_data, aes(x = lupus_mortality, y = excess_risk))+
  geom_point() + geom_hline(yintercept = 0)

ggplot(hosp_data %>% filter(lupus_mortality <= 0.2, lupus_mortality > 0), 
       aes(x = excess_risk))+
  geom_histogram()
ggplot(hosp_data %>% filter(lupus_mortality > 0.2),
       aes(x = excess_risk))+
  geom_histogram()


## What predicts if lupus mortality is greater than 20%

d <- hosp_data %>% filter(complete.cases(.)) %>% 
  mutate(teach = factor(teach), highvolume = factor(highvolume))
rfor <- randomForest::randomForest((lupus_mortality > 0.2) ~ nonlupus_mortality + 
                             teach + highvolume+ lupus_vent + 
                             total_admissions + prop_lupus, 
                           data=d, importance = T)
randomForest::varImpPlot(rfor)
ggplot(hosp_data, aes(x = lupus_vent, y = lupus_mortality))+geom_point()

## stratify data by ventilator status

bl1 <- lupus_data_10 %>% group_by(hospid) %>% 
  summarise(lupus_mort_v = mean(dead[ventilator=='1']),
            lupus_mort_nv = mean(dead[ventilator=='0']))
bl2 <- nonlupus_data %>% group_by(hospid) %>% 
  summarise(nonlupus_mort_v = mean(dead[ventilator=='1']),
            nonlupus_mort_nv = mean(dead[ventilator=='0']))
bl <- full_join(bl1, bl2, by='hospid')

bl_dead <- filter(bl, lupus_mort_nv > 0 | lupus_mort_v > 0)
ggplot(bl_dead, aes(lupus_mort_v, nonlupus_mort_v))+geom_point()+
  geom_abline()
ggplot(bl_dead, aes(lupus_mort_nv, nonlupus_mort_nv))+geom_point()+
  geom_abline()


# P-value approach --------------------------------------------------------

## We can get p-values testing the hypothesis that the mortality rate among lupus patients is higher
## than among non-lupus patients

hosp_data %>% nest(-hospid) %>% 
  mutate(pval = map_dbl(data, ~binom.test(.$lupus_dead, .$lupus_admissions, .$nonlupus_mortality, 
                                      alternative='greater')$p.value)) %>% 
  select(-data) -> pvalues

## An issue here is that this approach doesn't deal with the different levels of ventilator use

g1 <- ggplot(hosp_data, aes(lupus_vent, lupus_mortality))+
  geom_point()+ geom_abline()+ geom_smooth()+ ylim(0,0.7)+
  labs(x = 'Proportion on ventilator', y = 'Mortality rate')
g2 <- ggplot(hosp_data, aes(nonlupus_vent, nonlupus_mortality))+
  geom_point() + geom_abline() + geom_smooth()+ ylim(0,0.7)+
  labs(x = 'Proportion on ventilator', y = 'Mortality rate')
plot_grid(g1, g2, nrow=1, align = 'v', labels = c('Lupus','Non-lupus'), hjust = -1.5)

ggplot(hosp_data, aes(nonlupus_vent, lupus_vent))+geom_point() + geom_smooth() + geom_abline()



# Modeling mortality rate incorporating all factors -----------------------

## A ML approach

hosp_data_lupus <- hosp_data %>% select(hospid, teach, highvolume, bedsize,
                                        hosp_region, starts_with('lupus')) %>% 
  set_names(str_replace(names(.), 'lupus_',''))
hosp_data_nonlupus <- hosp_data %>% select(hospid, teach, highvolume, bedsize,
                                           hosp_region, starts_with('nonlupus')) %>% 
  set_names(str_replace(names(.), 'nonlupus_',''))
  
library(caret)
set.seed(3028)

fitControl <- trainControl(method = 'repeatedcv',
                           number = 10, 
                           repeats = 10)
gbmfit <- train(mortality ~ teach + highvolume + bedsize + hosp_region + vent, 
                data=hosp_data_nonlupus, method='gbm', trControl = fitControl)
rffit <- randomForest::randomForest(mortality~ teach + highvolume + bedsize + hosp_region + vent, 
                      data = hosp_data_nonlupus)
bridgefit <- train(mortality ~ teach + highvolume + bedsize + hosp_region + vent, 
                     data=hosp_data_nonlupus, method='bridge', trControl = fitControl)
gamloessfit <- train(mortality ~ teach + highvolume + bedsize + hosp_region + vent, 
                   data=hosp_data_nonlupus, method='gamLoess', trControl = fitControl)

pred_gbm <- predict(gbmfit, newdata = hosp_data_lupus)
pred_rf <- predict(rffit, newdata = hosp_data_lupus)
pred_bridge = predict(bridgefit, newdata= hosp_data_lupus)
pred_gamloess <- predict(gamloessfit, newdata = hosp_data_lupus)

pvalues2 <- hosp_data_lupus %>% mutate(pred_gbm = pred_gbm, pred_rf=pred_rf,
                                       pred_gamloess = pred_gamloess) %>% 
  nest(-hospid) %>% 
  mutate(pval1 = map_dbl(data, ~binom.test(.$dead, .$admissions, p = .$pred_gbm, alternative='greater')$p.value),
         pval2 = map_dbl(data, ~binom.test(.$dead, .$admissions, p = .$pred_rf, alternative = 'greater')$p.value),
         pval3 = map_dbl(data, ~binom.test(.$dead, .$admissions, p = .$pred_gamloess, alternative='greater')$p.value)) %>% 
  dplyr::select(hospid, pval1:pval3)
## This method doesn't take into account the different hospital sizes in the prediction process, 
## i.e., it won't account for the relative precisions of each estimate. Debating whether to do this
## or not


# SMR-based approach ------------------------------------------------------

SMR <- hosp_data_lupus %>% mutate(pred_gbm = pred_gbm, pred_rf = pred_rf, pred_gamloess = pred_gamloess) %>% 
  mutate(expect1 = pred_gbm * admissions, expect2 = pred_rf * admissions, expect3 = pred_gamloess*admissions) %>% 
  mutate(smr1 = dead/expect1, smr2 = dead/expect2, smr3 = dead/expect3)



# More modeling taking frequencies into account ---------------------------

objControl <- trainControl(method='cv', number = 3, returnResamp='none')
trainDF <- hosp_data_nonlupus %>% 
  mutate(alive = admissions - dead) %>% 
  mutate_at(vars(teach, highvolume, bedsize, hosp_region), as.factor) %>% 
  dplyr::select(teach:hosp_region, vent, alive, dead)

objModel <- train(hosp_data_nonlupus[,-c('alive','dead')], trainDF[,c('alive','dead')], 
                  method='glmnet', metric = 'ROC', trControl=objControl)
