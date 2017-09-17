# Code for modeling the effect of being a lupus patient on survival when admitted for sepsis

source('lib/reload.R'); reload()
load(file.path(datadir, 'data','rda','data.rda'))

fn_odds_ratio <- function(d){
  counts = d %>% count(dead, lupus)
  n = counts$n
  if(length(n) < 4) return(NA)
  return(n[1]*n[4]/n[2]/n[3])
}
# No pooling model --------------------------------------------------------


## Empirical odds ratios

all_data %>% nest(-hospid) %>% 
  mutate(N = map_int(data, ~nrow(count(dead, lupus)))) %>% 
  select(hospid, N) -> counts
## No adjustment

mod_nopool <- all_data %>% select(dead, hospid, lupus) %>% 
  mutate(lupus = as.factor(lupus),
         dead = as.factor(dead)) %>% 
  nest(-hospid) %>% 
  mutate(mod = map(data, ~glm(dead~lupus, data=., family = binomial())),
         lupus_logodds = map_dbl(mod, ~tidy(.)[2,2]))

all_data %>% select(dead, lupus, hospid) %>% 
  nest(-hospid) %>% 
  mutate(odds_ratio = map_dbl(data, fn_odds_ratio)) -> OR


myglm = function(d){
  glm(dead ~lupus * ventilator, data = d, family = binomial())
}

possibly_glm = possibly(myglm, otherwise = NA)

all_data %>% select(dead, lupus, ventilator, hospid) %>% 
  nest(-hospid) %>% 
  mutate(mods = map(data, possibly_glm)) %>% 
  mutate(res = map(mods, tidy)) -> bl



# Partial pooling ---------------------------------------------------------

mod_partial <- glmer(dead ~ (lupus|hospid), data=all_data,
                          family = binomial)
next_mod <- sampling(mod_partial, iter = 2000)

