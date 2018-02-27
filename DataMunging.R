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

lupus_data <- dat %>% filter(lupus == 1)
nonlupus_data <- dat %>% filter(lupus == 0)


# Data munging ------------------------------------------------------------

dat <- dat %>% 
  mutate_at(vars(male, dead, agecat, zipinc_qrtl, ventilator,
                 starts_with('failure'),
                 year, hospid), as.factor) %>% 
  mutate(year_scaled = scale(as.numeric(as.character(year)), center=T, scale=F)) %>% 
  mutate(payer = case_when(medicare == 1 ~ 1,
                           medicaid == 1 ~ 2,
                           private == 1 ~ 3,
                           otherins == 1 ~ 4))

lupus_data <- dat %>% filter(lupus == 1)
nonlupus_data <- dat %>% filter(lupus == 0)


lupus_data %>% dplyr::count(hospid) %>% filter(n >= 10) %>% 
  select(hospid) %>% 
  left_join(lupus_data) -> lupus_data_10

# TODO: Update this next code chunk to include the new failure variables.
# 
hosp_data <- dat %>% 
  group_by(hospid) %>% 
  summarise_at(vars(teach, highvolume, bedsize, hosp_region),
               funs(max(.))) 
hosp_data <- dat %>% group_by(hospid) %>% 
  summarise(n_sepsis = n()) %>% 
  right_join(hosp_data)

hosp_data <- nonlupus_data %>% 
  group_by(hospid) %>% 
  summarise_at(vars(dead, ventilator, starts_with('failure')),
               funs(mean(. == '1'))) %>% 
  rename_at(vars(-hospid), funs(paste0('nonlupus_', .))) %>% 
  right_join(hosp_data)

hosp_data <- lupus_data %>% 
  group_by(hospid) %>% 
  summarise_at(vars(dead, ventilator, starts_with('failure')),
               funs(mean(. == '1'))) %>% 
  rename_at(vars(-hospid), funs(paste0('lupus_', .))) %>% 
  right_join(hosp_data)

hosp_data <- (lupus_data %>% group_by(hospid) %>% summarise(lupus_sepsis = n())) %>% 
  right_join(nonlupus_data %>% group_by(hospid) %>% summarise(nonlupus_sepsis = n())) %>% 
  right_join(hosp_data)


save(dat, lupus_data, lupus_data_10, nonlupus_data, hosp_data,
     file = file.path(datadir,'data','rda','exp_sepsis2','data.rda'), 
     compress = T)

