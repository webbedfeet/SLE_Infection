## Decision tree based on O/E > 2 at hospital level
## Solves issue #8

ProjTemplate::reload()

load('data/lupuseffect.rda')
tree_data <- hosp_data %>% left_join(oe_overall) %>% mutate(high_oe = ifelse(oe_ratio > 2, 1,0)) %>% 
  select(new_highvolume:region, high_oe) %>% 
  mutate(region = fct_relabel(region, str_to_title)) %>% 
  set_names(c('High Volume','Bed Size','Teaching', 'Region', 'High O/E'))

tree_model <- rpart(`High O/E` ~ ., data=tree_data)
# tree_model_cat <- rpart(as.factor(`High O/E`)~., data=tree_data)

pdf('graphs/ObsExpTree.pdf')
prp(tree_model, type=4, extra='auto', yesno=2, fallen.leaves=F, uniform=F, varlen=0, faclen=0)
dev.off()
