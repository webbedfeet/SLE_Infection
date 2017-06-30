## Packages to load
pkgs <- c(
  'tidyverse',
  'stringr',
  'haven',
  'rstan',
  'rstanarm',
  'lme4',
  'MCMCglmm')
for(p in pkgs){
  if(!(p %in% installed.packages()[,1])){
    install.packages(p, repos='http://cran.rstudio.com')
  }
  library(p, character.only = TRUE)
}
