rm(list=ls())

library(tidyverse)
library(stringr)
library(zipcodeR)
library(readxl)
# library(RcppRoll)

source('paths.r')

source('_ANALYSIS/FUNCTIONS/notin.r')

HIF <- read_csv('_DATA/HIF.csv') %>%
  filter(YEAR>=2010) %>%
  filter(ZIPCODE %!in% c(00000, 00007, 00009)) %>%
  # filter(PTSTATE=='FL') %>%
  # filter(FEMALE.patient != 9) %>%
  filter(str_sub(ATTENPHYID, 1, 2) %in% c('ME', 'OS')) %>%
  mutate(ATTENPHYID.letter = str_sub(ATTENPHYID, 1, 2),
         ATTENPHYID.numpart = str_sub(ATTENPHYID, 3, nchar(ATTENPHYID)),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 4)=="0000", str_sub(ATTENPHYID.numpart, 5, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 3)=="000", str_sub(ATTENPHYID.numpart, 4, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 2)=="00", str_sub(ATTENPHYID.numpart, 3, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 1)=="0", str_sub(ATTENPHYID.numpart, 2, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID = paste0(ATTENPHYID.letter, ATTENPHYID.numpart)
  ) %>%
  select(-c(ATTENPHYID.letter, ATTENPHYID.numpart))



# FL_Zips <- search_state('FL') %>%
#   select(zipcode)



HIF_docs_id.pairs <- HIF %>%
  distinct(ATTENPHYID, ATTEN_PHYNPI, .keep_all = F)


HIF_docs_by.lno <- HIF_docs_id.pairs %>%
  group_by(ATTENPHYID) %>%
  summarize(n=n()) %>%
  filter(n==1)

HIF_docs_by.NPI <- HIF_docs_id.pairs %>%
  group_by(ATTEN_PHYNPI) %>%
  summarize(n=n()) %>%
  filter(n==1)



LNUM_NPI_xWalk <- HIF_docs_id.pairs %>%
  filter(ATTENPHYID %in% HIF_docs_by.lno$ATTENPHYID) %>%
  filter(ATTEN_PHYNPI %in% HIF_docs_by.NPI$ATTEN_PHYNPI)


HIF_docs_id.pairs_9999999999 <- HIF_docs_id.pairs %>%
  filter(ATTEN_PHYNPI=='9999999999') %>%
  select(ATTENPHYID) %>%
  merge(HIF_docs_id.pairs, by='ATTENPHYID', all.x=T, all.y=F) %>%
  filter(ATTEN_PHYNPI != '9999999999')

HIF_docs_by.lno_9999999999 <- HIF_docs_id.pairs_9999999999 %>%
  group_by(ATTENPHYID) %>%
  summarize(n=n()) %>%
  filter(n==1)

HIF_docs_by.NPI_9999999999 <- HIF_docs_id.pairs_9999999999 %>%
  group_by(ATTEN_PHYNPI) %>%
  summarize(n=n()) %>%
  filter(n==1)

LNUM_NPI_xWalk_9999999999 <- HIF_docs_id.pairs_9999999999 %>%
  filter(ATTENPHYID %in% HIF_docs_id.pairs_9999999999$ATTENPHYID) %>%
  filter(ATTEN_PHYNPI %in% HIF_docs_id.pairs_9999999999$ATTEN_PHYNPI)

LNUM_NPI_xWalk <- rbind(LNUM_NPI_xWalk, LNUM_NPI_xWalk_9999999999)

write_csv(LNUM_NPI_xWalk, '_DATA/LNUM_NPI_xWalk.csv')

