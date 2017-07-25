# Descriptives

source('lib/reload.R'); reload()
load(file.path(datadir, 'data','rda','data.rda'))


# Compare lupus and nonlupus mortality within hospital --------------------

bl1 <- lupus_data %>% group_by(hospid) %>% summarise(p1 = mean(dead))
bl2 <- nonlupus_data %>% group_by(hospid) %>% summarise(p2 = mean(dead))
bl <- inner_join(bl1, bl2, by='hospid')
ggplot(bl, aes(x = p1, y = p2))+geom_point() + 
  labs(x = 'Lupus mortality rate', y = 'Non-lupus mortality rate')


# Proportion of admissions that are lupus ---------------------------------

bl1 <- lupus_data %>% count(hospid) %>% rename(lupus_adm = 'n')
bl2 <- nonlupus_data %>% count(hospid) %>% rename(nonlupus_adm = 'n')
bl <- inner_join(bl1,bl2, by='hospid')
bl %>% mutate(prop_lupus = lupus_adm/(lupus_adm+nonlupus_adm)) %>% pull(prop_lupus) %>% 
  hist()


# Mortality based on hospital volume --------------------------------------

bl1 <- lupus_data %>% group_by(hospid) %>% 
  summarise(lupus_adm = n(), lupus_mort = mean(dead))
bl2 <- nonlupus_data %>% group_by(hospid) %>% 
  summarise(nonlupus_adm = n(), nonlupus_mort = mean(dead))
bl <- bl1 %>% inner_join(bl2, by='hospid') %>% 
  mutate(adm_total = lupus_adm+nonlupus_adm) 

ggplot(bl, aes(x=adm_total, y = lupus_adm/adm_total))+
  geom_point(aes(color = lupus_adm < 10)) + geom_smooth() + scale_y_log10() +
  labs(x = 'Total admissions', y = 'Proportion of admissions with lupus')

bl %>% mutate(prop_adm = lupus_adm / adm_total) %>% 
  ggplot(aes(x = lupus_mort, y = nonlupus_mort, size =  5*prop_adm))+
  geom_point()

  
nonlupus_data %>% group_by(hospid) %>% summarise(v = mean(ventilator == '1'),
                                                 n = n()) %>% 
  ggplot(aes(x=n, y = v))+geom_point() + geom_smooth()

nonlupus_data %>% group_by(hospid) %>% summarise(v = mean(ventilator=='1')) %>% 
  right_join(bl, by='hospid') %>% ggplot(aes(x=adm_total, y = v))+geom_point()+geom_smooth()

bl %>% select(ends_with('mort'), adm_total) %>% gather(group, mort, -adm_total) %>% 
  mutate(group = str_replace(group, '_mort','')) %>% 
  ggplot(aes(x = adm_total, y = 100*mort, group=group, color=group))+ geom_smooth(se=F)+
  ylim(0,30) + labs(x = 'Total admissions',  y = 'Mortality Rate')

d <- bind_rows(list('Lupus' = lupus_data, 'Non-lupus' = nonlupus_data), .id = 'Group')

d %>% group_by(Group, hospid) %>% 
  summarise(v = mean(ventilator=='1'),
            mort = mean(dead),
            mean_age = mean(age),
            median_age = median(age)) -> d1
d %>% group_by(hospid) %>% 
  summarise(N = n()) -> d2
d1 <- d1 %>% left_join(d2, by='hospid')
