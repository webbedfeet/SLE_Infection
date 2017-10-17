## Packages to load
pkgs <- c(
  'tidyverse',
  'stringr',
  'broom',
  'forcats',
  'cowplot',
  'haven',
  'janitor', # for crosstab
  'Cairo', # for unicode in ggplot + pdf
  'survey', # for weighted descriptives
  'randomForest',
  'xgboost',
  'rpart',
  'rpart.plot',
  'rstan',
  'rstanarm',
  'lme4',
  'MCMCglmm')
for(p in pkgs){
  if(!(p %in% installed.packages()[,1])){
    install.packages(p, repos='http://cran.rstudio.com')
  }
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}
