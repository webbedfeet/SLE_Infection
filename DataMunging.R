# Data acquisition and munging
source('lib/reload.R'); reload()


# Read data ---------------------------------------------------------------

lupus_data <- read_sas(file.path(datadir,'data','raw', 'sepsis_sle.sas7bdat'))
names(lupus_data) <- tolower(names(lupus_data))

nonlupus_data <- read_sas(file.path(datadir,'data','raw','sepsis_nonlupus.sas7bdat'))
names(nonlupus_data) <- tolower(names(nonlupus_data))

# Data munging ------------------------------------------------------------

lupus_data <- lupus_data %>% 
  mutate_at(vars(male, agecat, payer, zipinc_qrtl, slecomb_cat, 
                 ventilator,  year, hospid),
            as.factor) %>% 
  mutate(year_scaled = scale(as.numeric(as.character(year)),
                             center = T, scale = F))

lupus_data %>% count(hospid) %>% filter(n >= 10) %>% 
  select(hospid) %>% 
  left_join(lupus_data) -> lupus_data_10

nonlupus_data <- nonlupus_data %>% 
  mutate_at(vars(male, agecat, payer, zipinc_qrtl, ventilator, year, 
                 hospid),
            as.factor) %>% 
  mutate(year_scaled = scale(as.numeric(as.character(year)),
                             center = T, scale = F))
  
hosp_data <- nonlupus_data %>% 
  group_by(hospid) %>% 
  summarise(nonlupus_admissions = n(), 
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
            
            

save(lupus_data, lupus_data_10, nonlupus_data, hosp_data,
     file = file.path(datadir,'data','rda','data.rda'), 
     compress = T)
