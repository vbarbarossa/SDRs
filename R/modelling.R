
source('R/MASTER.R')
valerioUtils::libinv(c('dplyr','tidyr','ggplot2'))

#' load table with all atributes
tab <- read.csv('tabs/stations_all_attributes.csv') %>%
  as_tibble()

cm <- cor(tab %>% select(colnames(tab)[92:(ncol(tab)-2)]),
          use = "pairwise.complete.obs")

#' ..and then compute the correlation matrix

valerioUtils::libinv('corrplot')
corrplot(cm, method = 'number', type = 'lower',number.cex = 1)
