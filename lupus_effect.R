# Code for modeling the effect of being a lupus patient on survival when admitted for sepsis

source('lib/reload.R'); reload()
load(file.path(datadir, 'data','rda','data.rda'))

# No pooling model --------------------------------------------------------

all_data %>% select(dead, hospid,lupus, agecat, payer, elix_score, ventilator, year) %>% 
  nest(-hospid) %>% 
  mutate(nlevel_agecat = map_int(data, ~length(unique(.$agecat))),
         nlevel_payer = map_int(data, ~length(unique(.$payer))),
         nlevel_year = map_int(data, ~length(unique(.$year))),
         nlevel_lupus = map_int(data, ~length(unique(.$lupus)))) %>% 
  select(-data) -> descr

tst <- all_data %>% nest(-hospid) %>% 
  mutate(n_year = map_int(data, ~length(unique(.$year)))) %>% 
  filter(n_year > 1) %>% 
  select(-n_year) %>% 
  unnest() %>% mutate(hospid = as.character(hospid)) %>% 
  split(.$hospid)

for(h in names(tst)){
  print(h)
  try(glm(dead ~ lupus + agecat + elix_score + ventilator+year, 
          data = tst[[h]], 
          family = binomial()))
}
model_no_pooling <- all_data %>% 
  mutate(ventilator = as.numeric(as.character(ventilator))) %>% 
  nest(-hospid) %>% 
  mutate(nlevel_year = map_int(data, ~length(unique(.$year))),
         nlevel_lupus = map_int(data, ~length(unique(.$lupus)))) %>% 
  filter(nlevel_year > 1, nlevel_lupus > 1) %>% 
  mutate(mod = map(data, ~glm(dead~ lupus + 
                                agecat +  elix_score + 
                                ventilator + year, 
                              data = ., 
                              family = binomial())))


