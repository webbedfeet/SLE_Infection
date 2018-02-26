ProjTemplate::reload()
extrafont::font_import()
if(Sys.info()$sysname == 'Windows'){
  extrafont::loadfont(device = 'win')
} else {
  extrafont::loadfonts()
}

# Figure 1: Variable importance plot ----------------------------------------------------------

all_data <- readRDS('data/all_data.rds')
train_set <- all_data %>% filter(lupus == 1) %>%
  select(dead, agecat, lupus, ventilator, elix_score, male,
         medicare, medicaid, private, otherins,
         hospid) %>%
  model.matrix(~.-1, data=.)
xgbmodel1 <- readRDS('data/xgb_trained.rds')

english_names <- c('ventilator1'  = "Ventilator",
                   "elix_score" = "Elixhauser Score",
                   "agecat1" = 'Age 18-29',
                   'agecat2' = 'Age 30-49',
                   'agecat3' = 'Age 50-59',
                   'agecat4' = 'Age 60-64',
                   'male1' = "Gender",
                   "medicaid" = "Insurance: Medicaid",
                   'medicare' = 'Insurance: Medicare',
                   'private' = 'Insurance: Private',
                   'otherins' = 'Insurance: Other'
                   )

varimp <- xgb.importance(feature_names = colnames(train_set)[-1], model = xgbmodel1)
varimp <- as_tibble(varimp) %>% 
  mutate(Gain = Gain / max(Gain)) %>% 
  filter(!str_detect(Feature, 'hosp')) %>%
  mutate(new_Feature = english_names[Feature]) %>% 
  mutate(new_Feature = factor(new_Feature, levels = new_Feature[order(Gain)]))

ggplot(varimp, aes(new_Feature, Gain)) + geom_bar(stat='identity') + 
  xlab('') + 
  ylab('Relative Variable importance') + 
  theme(text = element_text(family = "Palatino")) + 
  coord_flip()

ggsave('graphs/Figure1.pdf')
ggsave('graphs/Figure1.tiff',scale = 0.5,
       dpi = 300, compression = 'lzw')


# Figure 2: Histogram of O/E ------------------------------------------------------------------

load('data/lupuseffect.rda')

ggplot(oe_overall, aes(x = oe_ratio)) + 
  geom_histogram(color = 'black', fill = 'white', breaks = seq(0,11,by=1), position = 'identity') +
  xlab('O/E ratio') + ylab('Frequency')

ggsave('graphs/Figure2.pdf', scale = 0.3)
ggsave('graphs/Figure2.tiff', scale = 0.3, compression = 'lzw', dpi=300)


# Figure 3: Crude mortality and O/E ratio -----------------------------------------------------

death_rate <- all_data %>% group_by(hospid) %>% summarize(rate = mean(dead))
lupus_mortality <- all_data %>% filter(lupus==1) %>% group_by(hospid) %>%
  summarize(lupus_rate = mean(dead, na.rm=T))
lupus <- all_data %>% group_by(hospid) %>% summarise(n_lupus = sum(lupus))
hosp_data <- reduce( list(hosp_data, death_rate, lupus_mortality, lupus, oe_overall), left_join)
dat_for_plots <- hosp_data %>% 
  select(n_lupus, rate, lupus_rate, oe_ratio)

ggplot(dat_for_plots, aes(lupus_rate, oe_ratio, size = n_lupus))+
  geom_point() +
  geom_hline(yintercept = 1) +
  # geom_hline(yintercept = 2, linetype = 2) +
  scale_x_continuous('Crude Mortality', breaks = c(round(median(dat_for_plots$lupus_rate),2), seq(0, 0.6,by = 0.2))) +
  scale_y_continuous('O/E ratio', breaks = c(1, seq(0,10, by=2)))+
  geom_vline(aes(xintercept = median(lupus_rate, na.rm=T)), linetype = 2) +
  annotate('text', x = 0.12, y = 8, label = 'Median rate', angle = 90) + 
  scale_size_continuous(breaks = c(5,10,20,40,60),
                        guide = guide_legend(title = 'Number of lupus patients',
                                             title.position = 'left', 
                                             title.theme = element_text(size = 11, angle = 0),
                                             label.theme = element_text(size = 10, angle = 0),
                                             direction = 'horizontal',
                                             nrow = 1)) +
  theme(legend.position = 'top', legend.justification = 'center')
  
ggsave('graphs/Figure3.pdf', scale = 0.5)
ggsave('graphs/Figure3.tiff', scale = 0.3, compression = 'lzw')

# Figure 4: Tree ------------------------------------------------------------------------------

dat_rplot <- hosp_data %>% 
  rename(`SLE Experience` = new_highvolume, Bedsize = new_bedsize, 
         Region=region)
levels(dat_rplot$Region) = c('Northeast','Midwest','South','West')
tree <- rpart(bad_ind~ `SLE Experience` + Bedsize+ new_teach + Region,
              data=dat_rplot, control = rpart.control(minsplit=10))
rpart.plot(tree, type=4, extra = 1, box.palette='Greys')
