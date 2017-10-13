# Descriptives using the survey package
# 
source('lib/reload.R'); reload()
load(file.path(datadir, 'data','rda','data.rda'))
# see http://faculty.washington.edu/tlumley/old-survey/exmample-lonely.html for the following
options(survey.lonely.psu = 'remove')


all_data <- all_data %>% 
  mutate(key = as.character(key),
         female = ifelse(male == 0, 1,0))

sv <- svydesign(id = ~hospid, strata=~nis_stratum, 
                weights = ~discwt, data=all_data, nest = T)
by_domain <- list('sv_sle'= subset(sv, lupus == 1),
"sv_non_sle"= subset(sv, lupus == 0))

out <- lapply(by_domain, 
  function(x) {
    svyby(~age + female + elix_score + ventilator + medicare +
            medicaid + private + otherins, 
          by = ~dead, 
          design = x,
          svymean,
          na.rm = T)
    }
  ) %>% 
  bind_rows(.id='status') %>% 
  select(-starts_with('se'), -ends_with('0')) %>% 
  mutate(status = str_replace(status, 'sv_',''),
         dead = ifelse(dead==1, 'dead','alive')) %>% 
  unite(group,status, dead, sep='_') %>% 
  gather(variable, means, -group)

out_se <- lapply(by_domain, 
              function(x) {
                svyby(~age + female + elix_score + ventilator + medicare +
                        medicaid + private + otherins, 
                      by = ~dead, 
                      design = x,
                      svymean,
                      na.rm = T)
              }
) %>% 
  bind_rows(.id='status') %>% 
  select(status, dead, starts_with('se'), -ends_with('0')) %>% 
  mutate(status = str_replace(status, 'sv_',''),
         dead = ifelse(dead==1, 'dead','alive')) %>% 
  unite(group,status, dead, sep='_') %>% 
  gather(variable,se, -group) %>% 
  mutate(variable = str_replace(variable, 'se.',''))

out <- out %>% left_join(out_se) %>% 
  mutate(means = ifelse(variable %in% c('female','ventilator1','medicare','medicaid','private','otherins'), 100*means, means),
         se = ifelse(variable %in% c('female','ventilator1','medicare','medicaid','private','otherins'), 100*se, se)) %>% 
  mutate(reports = sprintf('%.2f (%.2f)',means, se ),
         variable = str_replace(variable, '1','')) %>% 
  select(group, variable, reports) %>% 
  spread(group, reports)

# Clean up
out <- out[c(1,3,2,8,4,5,7,6),]
out <- out %>% mutate(variable = str_to_title(variable))
out <- out %>% mutate(variable = str_replace(variable, '_' ,' '))
out[7:8,1] = c('Private Ins','Other Ins')
names(out)[-1] <- str_replace(names(out)[-1],'sle','SLE')
names(out) <- str_replace(names(out), 'non_SLE','Non-SLE')
names(out) <- str_replace(names(out), '_',' ')
names(out)[1] <-  ''
# library(openxlsx)
# write.xlsx(out, 'Table1.xlsx')
