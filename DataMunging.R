##%######################################################%##
#                                                          #
####     Updated analysis with exp_sepsis2.sas7bdat     ####
####            Data ingestion and munging              ####
#                                                          #
##%######################################################%##

ProjTemplate::reload()


# Read data ---------------------------------------------------------------

dat <- haven::read_sas(file.path(datadir,'data','raw','exp_sepsis2.sas7bdat')) %>% 
  set_names(tolower(names(.))) %>% 
  mutate(key = as.character(key))



# Data munging ------------------------------------------------------------

dat <- dat %>% 
  mutate_at(vars(male, dead, agecat, zipinc_qrtl, ventilator,
                 starts_with('failure'),
                 year, hospid), as.factor) %>% 
  mutate(year_scaled = scale(as.numeric(as.character(year)), center = T, scale = F)) %>% 
  mutate(payer = case_when(medicare == 1 ~ 1,
                           medicaid == 1 ~ 2,
                           private == 1 ~ 3,
                           otherins == 1 ~ 4))

lupus_data <- dat %>% filter(lupus == 1)
nonlupus_data <- dat %>% filter(lupus == 0)

# Sub-setting lupus data for hospitals with at least 10 lupus cases rather than the default 5
lupus_data %>% dplyr::count(hospid) %>% filter(n >= 10) %>% 
  select(hospid) %>% 
  left_join(lupus_data) -> lupus_data_10


# Creating hospital-level dataset -------------------------------------------------------------

hosp_data <- dat %>% 
  group_by(hospid) %>% 
  summarise_at(vars(teach, highvolume, bedsize, hosp_region),
               funs(max(.))) %>% 
  mutate_all( as.factor)

hosp_data <- dat %>% group_by(hospid) %>% 
  summarise(n_sepsis = n()) %>% # Add number of sepsis cases in hospital
  right_join(hosp_data)

hosp_data <- nonlupus_data %>% 
  group_by(hospid) %>% 
  summarise_at(vars(dead, ventilator, starts_with('failure')), # Adding percentage on ventilators and comorbs for nonlupus patients
               funs(mean(. == '1'))) %>% 
  rename_at(vars(-hospid), funs(paste0('nonlupus_', .))) %>% 
  right_join(hosp_data)

hosp_data <- lupus_data %>% 
  group_by(hospid) %>% 
  summarise_at(vars(dead, ventilator, starts_with('failure')), # Add % on ventilator and comorbs for lupus patients
               funs(mean(. == '1'))) %>% 
  rename_at(vars(-hospid), funs(paste0('lupus_', .))) %>% 
  right_join(hosp_data)

hosp_data <- (lupus_data %>% group_by(hospid) %>% summarise(lupus_sepsis = n())) %>% 
  right_join(nonlupus_data %>% group_by(hospid) %>% summarise(nonlupus_sepsis = n())) %>% # Adding sepsis frequency
  right_join(hosp_data)


saveRDS(dat, file = file.path(datadir, 'data','rda','exp_sepsis2','dat.rds'), compress = T)
saveRDS(hosp_data, file = file.path(datadir,'data','rda','exp_sepsis2','hosp_data.rds'), compress=T)
save(dat, lupus_data, lupus_data_10, nonlupus_data, hosp_data,
     file = file.path(datadir,'data','rda','exp_sepsis2','data.rda'), 
     compress = T)

##%######################################################%##
#                                                          #
####                  Data imputation                   ####
#                                                          #
##%######################################################%##

library(caret)
dat <- readRDS(file.path(datadir, 'data','rda','exp_sepsis2','dat.rds')) 
dat <- dat %>% 
  mutate(dead = as.numeric(as.character(dead)),
         race = as.factor(ifelse(race >= 4, 'other',race)))
indiv_dat <- dat %>% 
  select(dead, age, race, zipinc_qrtl, elix_score, male, ventilator, starts_with("failure"))
bl <- dummyVars(dead ~ ., data=indiv_dat, fullRank = T)
indiv_dat1 <- data.frame(predict(bl, newdata = indiv_dat))

# knn imputation of missing data
# This centers and scales continuous predictors, and then performs kNN imputation on the missing values
bl <- preProcess(indiv_dat1, method = c('center','scale','knnImpute')) 
indiv_dat1 <- predict(bl, newdata = indiv_dat1)
indiv_dat1 <- cbind(dead = dat$dead, indiv_dat1)


saveRDS(indiv_dat1, file = file.path(datadir,'data','rda','exp_sepsis2','knnData.rds'), compress = T)

# Listwise deletion of missing values

indiv_dat2 <- indiv_dat %>% filter(complete.cases(indiv_dat))
saveRDS(indiv_dat2, file = file.path(datadir, 'data','rda','exp_sepsis2','listwiseData.rds'), compress = T)

# MICE imputation
library(mice)
miceMod <- mice(indiv_dat, m = 1, method = 'rf')


# Modal value imputation
