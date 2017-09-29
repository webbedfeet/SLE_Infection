## Packages to load
pkgs <- c(
  'tidyverse',
  'stringr',
  'broom',
  'forcats',
  'cowplot',
  'haven',
  'randomForest',
  'xgboost',
  'caret',
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
