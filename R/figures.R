
source('R/MASTER.R')
valerioUtils::libinv(c('dplyr','tidyr','ggplot2'))

#' load table with all atributes
tab <- read.csv('tabs/stations_all_attributes.csv') %>%
  as_tibble()

#' check bivariate plots
p <- tab %>%
  select(colnames(tab)[92:ncol(tab)]) %>%
  gather(-SR, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = SR)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ var, scales = "free",ncol = 4) +
  theme_bw()
p

p_log <- tab %>%
  select(colnames(tab)[92:ncol(tab)]) %>%
  gather(-SR, key = "var", value = "value") %>% 
  ggplot(aes(x = log10(value), y = log10(SR))) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ var, scales = "free",ncol = 4) +
  theme_bw()
p_log

ggsave('figs/check_bivariate.pdf',p,width = 200,height = 300,units = 'mm')
ggsave('figs/check_bivariate_all_log.pdf',p_log,width = 200,height = 300,units = 'mm')

