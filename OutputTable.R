#' ---
#' title: Hospital characteristics by mortality risk quadrant
#' author: Abhijit Dasgupta
#' output: html_document
#' ---
#' 
#' # Output table of summaries by O/E and absolute categories, based on plot
#' 
#+ setup, echo=F
library(knitr)
opts_chunk$set(echo = F, warning = F, message = F)

#+ preamble, echo=F, warning=F, message = F
ProjTemplate::reload()
load('data/lupuseffect.rda')

#' ## SMR/death rate plot
#+ fig1
death_rate <- all_data %>% group_by(hospid) %>% summarize(rate = mean(dead))
lupus <- all_data %>% group_by(hospid) %>% summarise(n_lupus = sum(lupus))
hosp_data <- hosp_data %>% left_join(death_rate) %>% left_join(lupus) %>% left_join(oe_overall)

dat_for_plot <- hosp_data %>% select(hospid, rate, oe_ratio,n_lupus)
mr <- median(dat_for_plot$rate)
ggplot(dat_for_plot, aes(rate, oe_ratio))+
  # geom_hline(yintercept = c(1,2), linetype=c(1,2), size=2, alpha = c(0.5,1)) +
  geom_segment(aes(x = median(rate), xend = median(rate), y=0, yend=8),
               linetype=2)+
  geom_segment(x = 0, xend=.38, y=1,yend=1,
               linetype=1, size=1, color='grey')+
  geom_segment(x = 0, xend=.38, y=2,yend=2,
               linetype=2, size=1.5, alpha=1)+
  geom_point(aes(size=n_lupus)) +
  scale_x_continuous('Death rate', 
                     breaks = c(.1, .2, .3),
                     labels = c('10%','20%','30%')) + 
  scale_y_continuous('O/E ratio for lupus', breaks = c(1,seq(0,10,by  =2))) + 
  scale_size_continuous('# SLE', breaks =c(10,20,40,60))+
  annotate('text',x = c(mr-0.03, mr+0.03, mr), 
           y = c(9,9,8.3),
           label=c(paste('\u2190','A low'),
                   paste('A high','\u2192'),'Median'), 
           hjust=c(1,0,0), angle=c(0,0,90), size=5)+
  annotate('text', 
           x = c(.4,.4),
           y = c(1.5,2.5),
           label = c(paste('R low',sprintf('\u2192')), paste(sprintf('\u2190'),'R high')),
           angle=-90, vjust = c(.5,.5), hjust=c(0,1), size=5)

#' ## Summary tables
#' 
#' We construct summary tables for hospital characteristics based on the four quadrants 
#' defined in the above figure. In particular, _High Rel_ and _Low Rel_ refer to whether
#' the O/E ratio is above or below 2. _High Abs_ and _Low Abs_ refer to whether the 
#' absolute mortality rate of the hospital is above or below the median (13%), respectively. 
#' 
#' __Table 1:__ Column-wise summaries (distribution of levels conditional on quadrant)
#' 
#+ tab1, echo=F, results='asis'
dat_for_table <- hosp_data %>% select(hospid, new_highvolume:n_lupus, oe_ratio) %>% 
  mutate(bad_oe = ifelse(oe_ratio > 2, 'High Rel','Low Rel'),
         bad_rate = ifelse(rate > median(rate, na.rm=T), 'High Abs', 'Low Abs')) %>% 
  mutate(cats = paste(bad_oe, bad_rate, sep = ' / '))

dat_for_table %>% select(new_highvolume:region, cats) %>% 
  gather(var, value, -cats) %>% 
  split(.$var) %>% 
  lapply(tabyl,  value, cats) %>% 
  lapply(adorn_percentages, 'col') %>% 
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
  group_rows('Region', 10,13) 

#' 
#' __Table 2:__ Row-wise summaries (distribution of quadrants by levels of each variable)
#' 
#+ tab2, echo=F, results = 'asis'

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
  group_rows('Region', 10,13) 

#' 