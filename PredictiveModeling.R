##%######################################################%##
#                                                          #
####        Individual-level predictive modeling        ####
####                 for risk of death                  ####
#                                                          #
##%######################################################%##

#+ setup, include=FALSE
ProjTemplate::reload()



# Read original data ---------------------------------------------------------------


dat <- haven::read_sas(file.path(datadir,'data','raw','exp_sepsis2.sas7bdat')) %>% 
  set_names(tolower(names(.))) %>% 
  mutate(key = as.character(key))

# See DataMunging for the data to do this.

#' We are developing a predictive model for the risk of death upon admission with sepsis,
#' regardless of lupus status. This is an "overall" predictive model and would give us
#' our best guess as to what the chance of dying is purely on patient characteristics.
#'
#' The individual-level predictors we'll use for this analysis are
#' age, race, year, zipinc_qrtl, elix_score, male, payer, ventialator and various failures.
#' We'll then add second-level hospital characteristics to refine estimates. In this process we
#' are intentionally ignoring the within-hospital / within-survey clustering of individuals. We may
#' return to this later using a GLMM approach with binary outcomes, but it's very complicated for
#' now.
#"
#' ### XGBoost

#' This is done in predictive_modeling.py. We should be able to use `reticulate` here

file.copy('indiv_dat1_risk.csv', file.path(datadir, 'data','indiv_dat1_risk.csv'), overwrite = T)
file.copy('indiv_dat2_risk.csv', file.path(datadir, 'data','indiv_dat2_risk.csv'))

indiv1_risk <- read_csv(file.path(datadir,'data','indiv_dat1_risk.csv'))

## Calibration of the model
indiv1_risk %>% 
  mutate(risk_cat = cut(risk, seq(0,1, by = 0.1), include.lowest = T, right = T)) %>%
  group_by(risk_cat) %>% 
  summarize(avgDeath = mean(dead), # Avg mortality rate for each group of predicted values
            avgPred = mean(risk)) %>% # Avg prediction for each group of predicted values
  ungroup() -> bl #
ggplot(bl, aes(avgPred, avgDeath))+geom_point() + geom_abline() + 
  labs(x = 'Predicted mortality', y = 'Observed mortality')
  
## AUC of the predictions

library(ROCR)
auc <- performance(prediction(indiv1_risk$risk, as.factor(indiv1_risk$dead)),'auc')
glue::glue('{auc@y.name}: {round(auc@y.values[[1]],3)}')

## Looking at RR and patterns with hospital properties
bl <- indiv1_risk %>% group_by(hospid, lupus) %>% 
  summarize(obs = sum(dead), expect = sum(risk)) %>% 
  ungroup() %>% 
  mutate(oe = obs/expect) %>% 
  select(hospid, oe, lupus) %>% 
  spread(lupus, oe) %>% 
  mutate(RR = `1`/`0`) %>% 
  select(hospid, RR) %>% 
  left_join(
    indiv1_risk %>% 
      filter(lupus == 0) %>% 
      group_by(hospid) %>% 
      summarize(mr = mean(dead)) %>% 
      ungroup()) %>% 
  left_join(
    dat %>% mutate(hospid = as.integer(hospid)) %>% 
      group_by(hospid, year) %>% summarize(N = n()) %>% ungroup() %>% 
      group_by(hospid) %>% summarize(avgN = mean(N)) %>% ungroup())

ggplot(bl, aes(x = mr, y = RR))+geom_point(aes(size = avgN)) +
  geom_smooth(data = bl %>% filter(RR>0), aes(mr, RR), se = T) +
  scale_y_continuous('Relative SMR of lupus patients', breaks = c(0,0.5,1:8))+
  scale_size('# patients (annual)', breaks = c(0,50,100, 500, 1000, 2000, 5000), range = c(1,10))+
  geom_hline(yintercept = c(0.5, 1,2), linetype = c(2,1,2), color = 'red') +
  geom_vline(xintercept = c(0.1, 0.166), linetype = 2) + # quartiles
  labs(x = 'Non-lupus mortality rate')

annual_lupus <- dat %>% mutate(hospid = as.integer(hospid)) %>% 
  group_by(hospid, year) %>% summarise(lup = sum(lupus)) %>% ungroup() %>% 
  group_by(hospid) %>% summarize(avglup = mean(lup)) %>% ungroup()

## Histogram of RR = 0 hospitals and size
bl %>% left_join(annual_lupus) %>% filter(RR == 0) %>% 
  gather(variable, value, mr:avglup) %>% 
  ggplot(aes(value))+geom_histogram(bins = 15) + 
  facet_wrap(~ variable, ncol = 1, scales='free')
### Statistical comparison of hospital outcomes by no mortality status
bl %>% left_join(annual_lupus) %>% 
  mutate(no_mort = factor(ifelse(RR == 0, 1, 0))) %>% 
  gather(variable, value, mr:avglup) %>% 
  nest(-variable) %>% 
  mutate(p.value = map_dbl(data, ~broom::tidy(wilcox.test(value ~ no_mort, data = .))$p.value)) %>% 
  select(-data)
bl %>% left_join(annual_lupus) %>% mutate(nomort = factor(ifelse(RR == 0 ,1 , 0))) %>% 
  ggplot(aes(x = nomort, y = mr))+geom_boxplot()

bl %>% left_join(annual_lupus) %>% 
  mutate(lupratio = avglup/avgN)%>% filter(RR==0) %>% 
  ggplot(aes(x = avgN, y = lupratio))+geom_point()

bl %>% left_join(annual_lupus) %>% 
  mutate(lupratio = avglup/avgN) %>%
  mutate(no_mort = factor(ifelse(RR == 0, 1, 0))) %>% 
  ggplot(aes(x = no_mort, y = lupratio))+geom_boxplot()
  
  
## Lupus sepsis experience as a factor in poor outcomes in hospitals
bl %>% left_join(annual_lupus) %>% 
  ggplot(aes(avglup, RR))+geom_point()+geom_hline(yintercept = 2)+
  labs(x = 'Avg number of lupus sepsis patients', 
       y = 'Relative SMR of lupus patients')

library(GGally)
ggpairs(left_join(bl, annual_lupus), columns = c(3:5,2))


bl <- bl %>% left_join(annual_lupus)
# Bootstrap variability of RRs ----------------------------------------------------------------

bootstraps <- read_csv(file.path(datadir,'data','bootstrapRR1.csv'))
summaries <- bootstraps %>% 
  summarize_all(funs(Q1 = quantile(., 0.25, na.rm=T),
                     Median = quantile(., 0.5, na.rm=T),
                     Q3 = quantile(., 0.75, na.rm=T),
                     LCB = quantile(., 0.025, na.rm=T),
                     UCB = quantile(., 0.975, na.rm = T),
                     Mean = mean(., na.rm=T),
                     SD = sd(., na.rm=T),
                     Max = max(., na.rm = T),
                     Min = min(., na.rm = T))) %>% 
  gather() %>% 
  separate(key, c('hospid','stat'), sep = '_') %>% 
  spread(stat, value) %>% 
  mutate(Range = Max - Min,
         IQR = Q3 - Q1)

SDs <- bootstraps %>% summarize_all(funs(sd(., na.rm=T))) %>% t()
ranges <- bootstraps %>% summarize_all(funs(mins = min(., na.rm=T), maxs = max(., na.rm=T))) %>% 
  gather() %>% 
  separate(key, c('hospid','stat'), sep='_') %>% 
  spread(stat, value) 
ggplot(ranges,aes(mins, maxs)) +geom_point()


# Tree-building -------------------------------------------------------------------------------

hosp_risk <- summarize_all(bootstraps, funs(mean(. > 2, na.rm = T))) %>% gather() %>% 
  set_names(c('hospid','risk'))
bl <- bl %>% mutate(hospid = as.character(hospid))
hosp_data <- readRDS(file.path(datadir, 'data','rda','exp_sepsis2','hosp_data.rds'))
hosp_data <- hosp_data %>% left_join(hosp_risk) %>% left_join(bl) %>% 
  mutate(high_RR = ifelse(RR > 2, 1, 0)) %>% 
  rename(Mortality_rate = mr,
         Sepsis_yr = avgN,
         Lupus_Sepsis_yr = avglup)
saveRDS(hosp_data, file = file.path(datadir, 'data','rda','exp_sepsis2','hosp_risk.rds'), compress = T)
library(rpart)
library(rpart.plot)       

tree_dat1 <- hosp_data %>% 
  select( teach, highvolume, bedsize, hosp_region,
         Mortality_rate, Sepsis_yr, Lupus_Sepsis_yr, risk)
tree1 <- rpart(risk ~ . , data = tree_dat1)
prp(tree1, type = 2)

tree1.1 <- rpart(risk~., data = tree_dat1 %>% select(-Lupus_Sepsis_yr)) #keep both 1 and 1.1
prp(tree1.1, type = 2)

tree_dat2 <- hosp_data %>% 
  select(teach, highvolume, bedsize, hosp_region, Mortality_rate, Sepsis_yr, Lupus_Sepsis_yr,
         high_RR)
tree2 = rpart(as.factor(high_RR) ~. , data = tree_dat2)
prp(tree2, type = 4)
