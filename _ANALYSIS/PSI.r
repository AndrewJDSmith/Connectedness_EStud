rm(list=ls())

library(tidyverse)
library(data.table)
library(tictoc)
library(RcppRoll)

source('paths.r')



LNUM_NPI_xWalk <- read_csv('_DATA/LNUM_NPI_xWalk.csv')


PSI <- read_csv('_DATA/PSI_YEAR.QTR.csv') %>%
  select(ATTENPHYID, YEAR, QTR, PSIAny_no.raw, PSIAny_at.risk)

PSI <- PSI %>%
  mutate(ATTENPHYID.letter = str_sub(ATTENPHYID, 1, 2),
         ATTENPHYID.numpart = str_sub(ATTENPHYID, 3, nchar(ATTENPHYID)),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 4)=="0000", str_sub(ATTENPHYID.numpart, 5, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 3)=="000", str_sub(ATTENPHYID.numpart, 4, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 2)=="00", str_sub(ATTENPHYID.numpart, 3, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 1)=="0", str_sub(ATTENPHYID.numpart, 2, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID = paste0(ATTENPHYID.letter, ATTENPHYID.numpart)
  )


PSI <- PSI %>%  
  group_by(ATTENPHYID, YEAR) %>%
  summarize(PSIAny_no.raw=sum(PSIAny_no.raw),
            PSIAny_at.risk=sum(PSIAny_at.risk)) %>%
  mutate(   PSIAny_rate_1y=ifelse(PSIAny_at.risk==0, 0, PSIAny_no.raw/PSIAny_at.risk),
            PSIAny_rate_l1y=ifelse(lag(PSIAny_at.risk)==0, 0, lag(PSIAny_no.raw)/lag(PSIAny_at.risk)),
            PSIAny_no.raw_2y=roll_sum(PSIAny_no.raw,
                                      2,
                                      fill=NA,
                                      align='right'),
            PSIAny_at.risk_2y=roll_sum(PSIAny_at.risk,
                                    2,
                                    fill=NA,
                                    align='right'),
            PSIAny_rate_2y=ifelse(PSIAny_at.risk_2y==0, 0, PSIAny_no.raw_2y/PSIAny_at.risk_2y),
            PSIAny_rate_l2y=ifelse(lag(PSIAny_at.risk_2y)==0, 0, lag(PSIAny_no.raw_2y)/lag(PSIAny_at.risk_2y)),
            PSIAny_no.raw_3y=roll_sum(PSIAny_no.raw,
                                      3,
                                      fill=NA,
                                      align='right'),
            PSIAny_at.risk_3y=roll_sum(PSIAny_at.risk,
                                       3,
                                       fill=NA,
                                       align='right'),
            PSIAny_rate_3y=ifelse(PSIAny_at.risk_3y==0, 0, PSIAny_no.raw_3y/PSIAny_at.risk_3y)
  ) %>%
  select(-c(starts_with('PSIAny_no.raw'), starts_with('PSIAny_at.risk')))




# PSI.in <- PSI %>%
#   merge(LNUM_NPI_xWalk,
#         by='ATTENPHYID',
#         all=F)
# 
# PSI.out <- PSI %>%
#   merge(LNUM_NPI_xWalk,
#         by='ATTENPHYID',
#         all=T)
# 
# PSI.right <- PSI %>%
#   merge(LNUM_NPI_xWalk,
#         by='ATTENPHYID',
#         all.x=T,
#         all.y=F)
# 
# PSI.left <- PSI %>%
#   merge(LNUM_NPI_xWalk,
#         by='ATTENPHYID',
#         all.x=F,
#         all.y=T)

PSI <- PSI %>%
  merge(LNUM_NPI_xWalk,
        by='ATTENPHYID',
        all=F)

PSI <- PSI %>%
  rename(NPI=ATTEN_PHYNPI, LNUM=ATTENPHYID, year=YEAR) %>%
  filter(year>=2009)
  

write_csv(PSI, '_DATA/PSI.csv')
