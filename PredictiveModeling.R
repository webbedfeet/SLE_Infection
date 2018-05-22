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
         race = as.factor(ifelse(race >=4, 'other',race)))
indiv_dat <- dat %>% 
  select(dead, age, race, zipinc_qrtl, elix_score, male, ventilator, starts_with("failure"))
bl <- dummyVars(dead ~ ., data=indiv_dat, fullRank = T)
indiv_dat1 <- data.frame(predict(bl, newdata = indiv_dat))
bl <- preProcess(indiv_dat1, method = c('center','scale','knnImpute')) # kNN imputation of missing data
indiv_dat1 <- predict(bl, newdata = indiv_dat1)
indiv_dat1 <- cbind(dead = dat$dead, indiv_dat1)
saveRDS(indivd_dat1, file = file.path(datadir,'data','rda','exp_sepsis2','knnData.rds'), compress=T)

#' One thing to note here is that there is about 15% missing data in some of the individual variables,
#' which we are imputing with kNN to complete the data set

# TODO: Do sensitivity analysis between imputation and listwise deletion of data. 



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

set.seed(1028)
train_xgb <- train(dead ~ ., dat = indiv_dat1, method = 'xgbTree', 
                   trControl = trainControl(method = 'cv',
                                            number = 10))


#' ### Random Forest
#' 
#' 
#' 
#' 
#' ### Logistic Regression
#' 
#' 
#' 