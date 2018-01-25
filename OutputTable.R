## Output table of summaries by O/E and absolute categories, based on plot
ProjTemplate::reload()
load('data/lupuseffect.rda')

## Creating an SMR/death rate plot
death_rate <- all_data %>% group_by(hospid) %>% summarize(rate = mean(dead))
lupus <- all_data %>% group_by(hospid) %>% summarise(n_lupus = sum(lupus))
hosp_data <- hosp_data %>% left_join(death_rate) %>% left_join(lupus) %>% left_join(oe_overall)

dat_for_plot <- hosp_data %>% select(hospid, rate, oe_ratio,n_lupus)
ggplot(dat_for_plot, aes(rate, oe_ratio))+geom_point(aes(size=n_lupus)) +
  geom_hline(yintercept = c(1,2), linetype=c(4,2), alpha = c(0.5,1)) +
  geom_vline(aes(xintercept = median(rate)), linetype=2)+
  scale_x_continuous('Death rate',
                     breaks = c(.1, .2, .3, median(hosp_data$rate)),
                     labels = c('10%','20%','30%', 'Median')) +
  scale_y_continuous('SMR for lupus', breaks = c(1,seq(0,10,by  =2))) +
  scale_size_continuous('# SLE', breaks =c(10,20,40,60))

dat_for_table <- hosp_data %>% select(hospid, new_highvolume:n_lupus, oe_ratio) %>% 
  mutate(bad_oe = ifelse(oe_ratio > 2, 'High Rel','Low Rel'),
         bad_rate = ifelse(rate > median(rate, na.rm=T), 'High Abs', 'Low Abs')) %>% 
  mutate(cats = paste(bad_oe, bad_rate, sep = ' / '))


dat_for_table %>% select(new_highvolume:region, cats) %>% 
  gather(var, value, -cats) %>% 
  split(.$var) %>% 
  lapply(tabyl,  value, cats) %>% 
  lapply(adorn_percentages, 'row') %>% 
  lapply(adorn_pct_formatting) %>% 
  dplyr::bind_rows() %>%
  mutate(value = str_to_title(value))-> bl
tmp <- dat_for_table %>% count(cats) %>% spread(cats,n) %>% 
  mutate_all(as.character)
tmp <- cbind(tibble('value'='N'), tmp)
bl <- tmp %>% bind_rows(bl) %>% 
  set_names('', names(.)[-1])
bl %>% 
  kable('html') %>% 
  # kable_styling() %>% 
  group_rows('Bedsize',2,4) %>% 
  group_rows('High SLE Volume', 5,6) %>% 
  group_rows('Teaching', 7,9) %>% 
  group_rows('Region', 10,13) %>% 
  writeLines('docs/OutputTable.html')

  
