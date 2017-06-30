# Data acquistion, descriptives and munging

source('lib/reload.R'); reload()

SLE <- read_sas('data/raw/sepsis_sle.sas7bdat') %>% 
  select(dead, male, agecat, payer, zipinc_qrtl, ventilator, YEAR, key, HOSPID, ELIX_SCORE,
         bedsize, HOSP_REGION, teach, highvolume, slecomb_cat, slecomb) %>% 
  mutate_at(vars(male, agecat, payer, zipinc_qrtl, slecomb_cat, ventilator, YEAR), as.factor)
    
SLE2 <- SLE %>% group_by(HOSPID) %>% summarise(dead = sum(dead), alive = n()-dead, n = n()) %>% ungroup()


# Descriptives ------------------------------------------------------------

SLE %>% mutate(YEAR = as.numeric(as.character(YEAR))) %>% 
  nest(-HOSPID, -YEAR) %>% 
  mutate(mortality = map_dbl(data, ~mean(.$dead)), 
         odds = mortality/(1-mortality)) %>% 
  select(-data) %>% 
  ggplot(aes(YEAR, odds))+geom_jitter()+geom_smooth()

f <- formula(dead ~ male + agecat + payer + zipinc_qrtl + slecomb_cat + ventilator + YEAR)


# Complete pooling --------------------------------------------------------

mod_pooled <- glm(f, data=SLE, family=binomial(link='logit'))
mod_pooled_cts <- glm(f, data= SLE %>% mutate(YEAR = as.numeric(as.character(YEAR))))
summary(mod_pooled)
anova(mod_pooled, test='Chisq')

mod_pooled2 <- update(mod_pooled, .~. -male -zipinc_qrtl)

# No pooling model

SLE <- SLE %>% mutate(YEAR = as.numeric(as.character(YEAR)), 
                      year_scaled = YEAR - mean(YEAR))
f2 <- formula(dead ~ agecat + payer + slecomb_cat + ventilator + year_scaled)
coef_intercept <- function(m){
  if(class(m) == 'try-error') {
    out <- NA
  } else {
    out <- plogis(coef(m)["(Intercept)"])
  }
  return(out)
}

bl <- SLE %>% nest(-HOSPID) %>% mutate(mod = map(data, ~try(glm(f2, 
                                                          family = binomial(link='logit'), 
                                                          data=.), silent=T)),
                                 intercept = map_dbl(mod, ~coef_intercept(.))) %>% 
  select(HOSPID, intercept) %>% 
  mutate(intercept = ifelse(is.na(intercept), 0, intercept))

bl2 <- glm(dead ~ -1 + factor(HOSPID) + agecat + payer + slecomb_cat + ventilator + year_scaled, 
           family=binomial(link='logit'), data = SLE)
s1 <- broom::tidy(bl2) %>% 
  filter(!grepl('HOSPID', term))
s2 <- broom::tidy(bl2) %>% 
  filter(grepl("HOSPID", term))

df_no_pooling <- lmList(cbind(dead,1-dead) ~ agecat + payer + +slecomb_cat+ventilator+year_scaled | HOSPID, 
                        data=SLE, family=binomial(link='logit')) %>% coef() %>% 
  rownames_to_column('Hospital') %>% 
  rename(Intercept = `(Intercept)`) %>% 
  select(Hospital:Intercept) %>% 
  add_column(Model = 'No pooling')

m <- glmer(dead~male + agecat + payer + zipinc_qrtl + slecomb_cat + ventilator + YEAR + (1|HOSPID), SLE, 
           family = binomial(link = 'logit'))
df_partial_pooling <- coef(m)[['HOSPID']] %>% 
  as_tibble() %>% 
  rownames_to_column("Hospital") %>% 
  rename(Intercept = `(Intercept)`) %>% mutate(Model = 'Partial pooling')
ggplot(df_partial_pooling, aes(x = Intercept))+geom_histogram()
su