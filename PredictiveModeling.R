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

file.copy('indiv_dat1_risk.csv', file.path(datadir, 'data','indiv_dat1_risk.csv'))
file.copy('indiv_dat2_risk.csv', file.path(datadir, 'data','indiv_dat2_risk.csv'))

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


indiv1_risk <- read_csv(file.path(datadir,'data','indiv_dat1_risk.csv'))
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

## Histogram of RR = 0 hospitals and size

annual_lupus <- dat %>% mutate(hospid = as.integer(hospid)) %>% 
  group_by(hospid, year) %>% summarise(lup = sum(lupus)) %>% ungroup() %>% 
  group_by(hospid) %>% summarize(avglup = mean(lup)) %>% ungroup()
bl %>% left_join(annual_lupus) %>% 
  mutate(lupratio = avglup/avgN)%>% filter(RR==0) %>% 
  ggplot(aes(x = avgN, y = lupratio))+geom_point()

## Lupus sepsis experience as a factor in poor outcomes in hospitals
bl %>% left_join(annual_lupus) %>% 
  ggplot(aes(avglup, RR))+geom_point()+geom_hline(yintercept = 2)+
  labs(x = 'Avg number of lupus sepsis patients', 
       y = 'Relative SMR of lupus patients')

