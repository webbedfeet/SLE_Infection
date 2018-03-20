##%######################################################%##
#                                                          #
####        Individual-level predictive modeling        ####
####                 for risk of death                  ####
#                                                          #
##%######################################################%##

ProjTemplate::reload()
dat <- readRDS(file.path(datadir, 'data','rda','exp_sepsis2','dat.rds'))

#' We are developing a predictive model for the risk of death upon admission with sepsis, 
#' regardless of lupus status. This is an "overall" predictive model and would give us 
#' our best guess as to what the chance of dying is purely on patient characteristics. 
#' 
#' The individual-level predictors we'll use for this analysis are
#' age, race, year, zipinc_qrtl, elix_score, male, payer, ventialator and various failures. 
#' We'll then add second-level hospital characteristics to refine estimates