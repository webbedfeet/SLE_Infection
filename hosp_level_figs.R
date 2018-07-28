##%######################################################%##
#                                                          #
####               Hospital-level figures               ####
#                                                          #
##%######################################################%##

ProjTemplate::reload()
hosp_data <- readRDS(file.path(datadir,'data','rda','exp_sepsis2','hosp_risk.rds'))


# Boxplot of risk probablity vs observed RR ---------------------------------------------------

cairo_pdf('graphs/boxplot_probs.pdf')
hosp_data %>%
  mutate(high_RR = factor(ifelse(high_RR==1,'Obs RR \u2265 2', 'Obs RR < 2'))) %>% 
  ggplot(aes(x = high_RR, y = risk)) + geom_boxplot() +
  labs(x = '', y = "Boostrapped probability that RR \u2265 2 ")
dev.off()


# Plots of RR vs other measures ---------------------------------------------------------------

hosp_data %>%
  ggplot(aes(x = Mortality_rate, y = RR)) + geom_point() +
    geom_hline(yintercept = 2, linetype = 2) +
    labs(x = 'Mortality rate', y = 'Risk Ratio (RR)') -> plt1

hosp_data %>%
  ggplot(aes(x = Sepsis_yr, y = RR)) + geom_point() +
    geom_hline(yintercept = 2, linetype = 2) +
    labs(x = 'Sepsis cases per year', y = 'Risk Ratio (RR)') -> plt2

hosp_data %>%
  ggplot(aes( x = Lupus_Sepsis_yr, y = RR)) + geom_point() +
    geom_hline(yintercept = 2, linetype = 2) +
    labs(x = 'Sepsis cases with SLE per year', y = 'Risk Ratio (RR)') -> plt3

hosp_data %>%
  ggplot(aes(x = `0`, y = RR)) + geom_point()+
    geom_hline(yintercept = 2, linetype = 2) +
    labs(x = 'Observed/Expected among non-SLE', y = 'Risk Ratio (RR)') -> plt4

cairo_pdf('graphs/panel.pdf')
plot_grid(plt1, plt2, plt4, plt3, labels = c('A','B','C', 'D'), ncol = 2)
dev.off()

cairo_pdf('graphs/Mortality.pdf')
print(plt1)
dev.off()
cairo_pdf('graphs/Sepsis.pdf')
print(plt2)
dev.off()
cairo_pdf('graphs/SLE_Sepsis.pdf')
print(plt3)
dev.off()
cairo_pdf('graphs/OE.pdf')
print(plt4)
dev.off()
setwd('graphs')
system('./pdf2tiff.py -c')
setwd('..')
