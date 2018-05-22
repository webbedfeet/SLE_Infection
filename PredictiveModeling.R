##%######################################################%##
#                                                          #
####        Individual-level predictive modeling        ####
####                 for risk of death                  ####
#                                                          #
##%######################################################%##

#+ setup, include=FALSE
ProjTemplate::reload()
dat <- readRDS(file.path(datadir, 'data','rda','exp_sepsis2','dat.rds')) 
dat <- dat %>% 
  mutate(dead = as.numeric(as.character(dead)),
         race = as.factor(ifelse(race >=4, 5,race)))

indiv_dat <- dat %>% 
  select(dead, race, year, male, elix_score, highvolume, medicare:age_60plus, rural:largeurban,
         bed1:bed3) 
ind <- complete.cases(indiv_dat)
out <- as.data.frame(model.matrix(~. -1, data = indiv_dat))
out <- cbind(out, 'hospid'=dat$hospid[ind], 'lupus'=dat$lupus[ind])
# bl <- preProcess(indiv_dat1, method = c('center','scale','knnImpute'))
# indiv_dat1 <- predict(bl, newdata = indiv_dat1)
indiv_dat1 <- cbind(dead = dat$dead, indiv_dat1)
indiv_dat2 <- indiv_dat1[complete.cases(indiv_dat1),]
write_csv(out, 'indivdat.csv')

indiv2 <- dat %>% 
  select(dead, year, male, elix_score, highvolume, medicare:age_60plus, 
         rural:largeurban, bed1:bed3) 
ind = complete.cases(indiv2)
out2 = as.data.frame(model.matrix(~.-1, data=indiv2))
out2 = cbind(out2, 'hospid' = dat$hospid[ind], 'lupus' = dat$lupus[ind])
write_csv(out2, 'indivdat2.csv')


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

results <- read_csv('results.csv')
results <- results %>% mutate_at(vars(highvolume:bed3), funs(as.factor(ifelse(.==0,'No','Yes'))))
results2=results %>% mutate(RR = factor(ifelse(RR >= 2, 1,0))) %>% select(-hospid)
prp(rpart(RR ~ region+type+bedsize+highvolume, 
          data=results2, 
          control=rpart.control(cp=0.001, maxdepth = 2)), type=4,extra=1)

m3 <- rpart(RR~ region + type + bedsize + highvolume, 
            data = results2 %>% filter(RR_value > 0),
            control = rpart.control(cp=0.001, maxdepth=4))
prp(m3, type = 4)
#' ### Random Forest
#' 
#' 
#' 
#' 
#' ### Logistic Regression
#' 
#' 
#' 