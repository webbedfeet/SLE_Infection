# Data acquisition and munging
source('lib/reload.R'); reload()
source('lib/R/find_datadir.R') # Finds Dropbox data directory


# Read data ---------------------------------------------------------------

lupus_data <- read_sas(file.path(datadir,'data','sepsis_sle.sas7bdat'))
names(lupus_data) <- tolower(names(lupus_data))

nonlupus_data <- read_sas(file.path(datadir,'data','sepsis_nonlupus.sas7bdat'))
names(nonlupus_data) <- tolower(names(nonlupus_data))

# Data munging ------------------------------------------------------------

lupus_data <- lupus_data %>% 
  mutate_at(vars(male, agecat, payer, zipinc_qrtl, slecomb_cat, 
                 ventilator,  year, hospid),
            as.factor) %>% 
  mutate(year_scaled = scale(as.numeric(as.character(year)),
                             center=T, scale=F))


save(lupus_data, nonlupus_data, file = 'data/rda/data.rda', compress=T)