ProjTemplate::reload()

# Figure 1: Variable importance plot ----------------------------------------------------------

all_data <- readRDS('data/all_data.rds')
train_set <- all_data %>% filter(lupus == 1) %>%
  select(dead, agecat, lupus, ventilator, elix_score, male,
         medicare, medicaid, private, otherins,
         hospid) %>%
  model.matrix(~.-1, data=.)
xgbmodel1 <- readRDS('data/xgb_trained.rds')

english_names <- c('ventilator1'  = "Ventilator",
                   "elix_score" = "Elix Score",
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
  xlab('Features') + 
  ylab('Relative Variable importance') + 
  coord_flip()

ggsave('graphs/Figure1.pdf', scale = 0.5)
ggsave('graphs/Figure1.tiff',scale = 0.5,
       dpi = 300, compression = 'lzw')


# Figure 2: Histogram of O/E ------------------------------------------------------------------

load('data/lupuseffect.rda')

ggplot(oe_overall, aes(x = oe_ratio)) + 
  geom_histogram(color = 'black', fill = 'white', breaks = seq(0,11,by=1), position = 'identity') +
  xlab('O/E ratio') + ylab('Frequency')

ggsave('graphs/Figure2.pdf', scale = 0.3)
ggsave('graphs/Figure2.tiff', scale = 0.3, compression = 'lzw', dpi=300)
