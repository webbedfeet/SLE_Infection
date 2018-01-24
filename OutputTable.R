## Output table of summaries by O/E and absolute categories, based on plot

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
  mutate(bad_oe = ifelse(oe_ratio > 2, 1,0),
         bad_rate = ifelse(rate > median(rate, na.rm=T), 1,0)) %>% 
  mutate(cats = paste(bad_oe, bad_rate))

dat_for_table %>% 
  tabyl(new_highvolume, cats) %>% 
  adorn_percentages('col') %>% 
  adorn_pct_formatting()
