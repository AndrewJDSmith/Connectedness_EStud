rm(list=ls())

library(tidyverse)
library(data.table)

source('paths.r')

for(i in c('09', 10:15)){
  
  PSPP.i <- read_csv(paste0(PSPP_path, '90-Day\\pspp20', i, '_90.csv'))
  
  assign(paste0('PSPP_20', i), PSPP.i)
  
}

PSPP.list <- mget(ls(pattern = 'PSPP_20'))

PSPP_90 <- rbindlist(PSPP.list)

write_csv(PSPP_90, 'DATA/PSPP_90.csv')
