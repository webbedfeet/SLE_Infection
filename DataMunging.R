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

lupus_data <- lupus_data %>% 
  mutate_at(vars(male, agecat,  zipinc_qrtl, #slecomb_cat, 
                 ventilator,  starts_with('failure'), year, hospid),
            as.factor) %>% 
  mutate(year_scaled = scale(as.numeric(as.character(year)),
                             center = T, scale = F)) %>% 
  mutate(payer = case_when(medicare == 1 ~ 1,
                           medicaid == 1 ~ 2,
                           private == 1 ~ 3,
                           otherins == 1 ~ 4))

lupus_data %>% dplyr::count(hospid) %>% filter(n >= 10) %>% 
  select(hospid) %>% 
  left_join(lupus_data) -> lupus_data_10

nonlupus_data <- nonlupus_data %>% 
  mutate_at(vars(male, agecat, #payer, 
                 zipinc_qrtl, ventilator,  starts_with('failure'), 
                 year,
                 hospid),
            as.factor) %>% 
  mutate(year_scaled = scale(as.numeric(as.character(year)),
                             center = T, scale = F)) %>% 
  mutate(payer = case_when(medicare == 1 ~ 1,
                           medicaid == 1 ~ 2,
                           private == 1 ~ 3,
                           otherins == 1 ~ 4))
  
# TODO: Update this next code chunk to include the new failure variables.
hosp_data <- nonlupus_data %>% 
  group_by(hospid) %>% 
  dplyr::summarise(nonlupus_admissions = n(), 
            nonlupus_mortality = mean(dead),
            nonlupus_dead = sum(dead),
            teach = max(teach),
            highvolume = max(highvolume),
            bedsize = max(bedsize),
            hosp_region = max(hosp_region),
            nonlupus_vent = mean(ventilator == '1')) %>% 
  inner_join(lupus_data %>% group_by(hospid) %>% 
              summarise(lupus_admissions = n(), 
                        lupus_mortality = mean(dead), 
                        lupus_dead = sum(dead),
                        lupus_vent = mean(ventilator == '1')),
            by = 'hospid') %>% 
  mutate(total_admissions = lupus_admissions + nonlupus_admissions,
         prop_lupus = lupus_admissions / total_admissions)# %>% 
  # select(-lupus_admissions, -nonlupus_admissions)

common_names <- intersect(names(lupus_data), names(nonlupus_data))
common_hosp <- intersect(lupus_data$hospid, nonlupus_data$hospid)

all_data <- rbind(lupus_data %>% filter(hospid %in% common_hosp) %>% select(common_names),
                  nonlupus_data %>% filter(hospid %in% common_hosp) %>% select(common_names))

save(lupus_data, lupus_data_10, nonlupus_data, hosp_data, all_data,
     file = file.path(datadir,'data','rda','exp_sepsis2','data.rda'), 
     compress = T)
