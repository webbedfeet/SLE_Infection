
# Table 1 -------------------------------------------------------------------------------------

ProjTemplate::reload()
dat <- readRDS(file.path(datadir,'data','rda','exp_sepsis2','full_data.rds'))

## Individual level variables are age, race, zipinc_qrtl, elix_score, lupus, male, hosp_region,
## failure variables

dat %>% select( male, starts_with('failure'), ventilator, lupus) %>% # binary variables
  set_names(str_replace(names(.),'_','.')) %>% 
  group_by(lupus) %>% 
  summarize_all(funs(proportion = mean(., na.rm=T))) %>% 
  gather(key, value, -lupus) %>% 
  separate(key, c('Variable','stat'), sep = '_') %>% 
  mutate(lupus = ifelse(lupus==1, 'Lupus','No Lupus')) %>% 
  unite(header, c('lupus','stat'), sep = ' | ') %>% 
  spread(header,value) %>% 
  right_join(
    dat %>% select( male, starts_with('failure'), ventilator) %>% # binary variables
      set_names(str_replace(names(.),'_','.')) %>% 
      summarize_all(funs(proportion = mean(., na.rm=T))) %>% 
      ungroup() %>% 
      gather() %>% 
      separate(key, c('Variable','stat'), sep = '_') %>% 
      mutate(stat = paste('Overall',stat, sep = ' | ')) %>% 
      spread(stat, value)
  ) %>% 
  set_names(str_remove(names(.), ' \\| proportion')) %>% 
  mutate_at(vars(-Variable), funs(paste0(as.character(round(100*.,2)),'%')))-> props

dat %>% select(lupus, age, elix_score) %>% # Continuous variables
  set_names(str_replace(names(.), '_','.')) %>%
  group_by(lupus) %>% 
  summarize_all(funs(Mean = mean(., na.rm=T), SD = sd(., na.rm=T))) %>% 
  gather(variable, value, -lupus) %>% 
  separate(variable, c('Variable','stat'), sep = '_') %>% 
  spread(stat,value) %>% 
  mutate(out = glue::glue('{round(Mean,2)} ({round(SD,2)})')) %>% 
  select(Variable, lupus, out) %>% 
  mutate(lupus = ifelse(lupus == 0, 'No Lupus','Lupus')) %>% 
  spread(lupus, out) %>% 
  right_join(
    dat %>% select(age, elix_score) %>% 
      set_names(str_replace(names(.), '_','.')) %>% 
      summarize_all(funs(Mean = mean(., na.rm=T), SD = sd(., na.rm=T))) %>% 
      gather() %>% 
      separate(key,c('Variable','stat'),sep = '_') %>% 
      spread(stat, value) %>% 
      mutate(Overall = glue::glue('{round(Mean,2)} ({round(SD,2)})')) %>% 
      select(Variable,Overall)
  ) -> cts


library(table1)
dat_for_tab1 <- dat %>% select(age, race, male,zipinc_qrtl, hosp_region, elix_score,lupus, ventilator,
                               starts_with('failure')) %>% 
  mutate(lupus = ifelse(lupus == 0, 'No SLE','SLE'),
         male = ifelse(male == 0, 'Female','Male')) %>% 
  mutate_at(vars(ventilator:failure_renal), funs(ifelse(.==0,'No','Yes'))) %>% 
  mutate(hosp_region = case_when(hosp_region == 1 ~ 'Northeast',
                                 hosp_region == 2 ~ 'Midwest',
                                 hosp_region == 3 ~ 'South',
                                 hosp_region == 4 ~ 'West')) %>% 
  mutate_at(vars(race:hosp_region, lupus:failure_renal), as.factor)
library(labelled)
var_label(dat_for_tab1) <- list(race = 'Race', male = 'Gender', zipinc_qtrl = 'SES Quartile',
                                 hosp_region = 'Region', elix_score = 'Elixhauser score',
                                 ventilator = 'On ventilator',
                                 failure_cardiac = 'Cardiac failure',
                                 failure_neuro = 'Neurologic failure',
                                 failure_heme = 'Hematologic failure',
                                 failure_liver = 'Liver failure',
                                 failure_renal = 'Renal failure')
table1(~age + race + male + hosp_region + elix_score + ventilator + failure_cardiac + failure_neuro + failure_heme + failure_liver + failure_renal| lupus, data=dat_for_tab1,
       render.continuous = c(. = 'Mean (SD)'),
       render.categorical = c(. = 'PCT%'))
