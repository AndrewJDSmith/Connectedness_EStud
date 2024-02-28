rm(list=ls())

library(tidyverse)
library(RcppRoll)
library(data.table)
library(tictoc)
library(knitr)


MPL <- read_csv('_DATA/MPL.csv') 

MPL <- MPL %>%
  mutate(MPL_OCCURRENCE_DATE=as.Date(substr(MPL_OCCURRENCE_DATE, 1, nchar(MPL_OCCURRENCE_DATE)-5), tryFormats="%m/%d/%Y"),
         MPL_SUIT_DATE=as.Date(substr(MPL_SUIT_DATE, 1, nchar(MPL_SUIT_DATE)-5), tryFormats="%m/%d/%Y"),
         MPL_REPORT_DATE=as.Date(substr(MPL_REPORT_DATE, 1, nchar(MPL_REPORT_DATE)-5), tryFormats="%m/%d/%Y"),
         MPL_FIN_DATE_DISP=as.Date(substr(MPL_FIN_DATE_DISP, 1, nchar(MPL)-5), tryFormats="%m/%d/%Y")
         )

TIMES <- MPL %>%
  mutate(OCC_to_SUIT=as.double(difftime(MPL_SUIT_DATE, 
                              MPL_OCCURRENCE_DATE,
                              units = 'days'
                              ))/365.25,
         OCC_to_REP=as.double(difftime(MPL_REPORT_DATE,
                             MPL_OCCURRENCE_DATE,
                             units = 'days'
                             ))/365.25,
         OCC_to_FIN_DISP=as.double(difftime(MPL_FIN_DATE_DISP,
                                  MPL_OCCURRENCE_DATE,
                                  units = 'days'
                                  ))/365.25,
         REP_to_SUIT=as.double(difftime(MPL_SUIT_DATE,
                                        MPL_REPORT_DATE,
                                        units='days'
                                        ))/365.25,
         REP_to_FIN_DISP=as.double(difftime(MPL_FIN_DATE_DISP,
                                            MPL_REPORT_DATE,
                                            units='days'
                                            ))/365.25
         ) %>%
  select(OCC_to_SUIT, OCC_to_REP, OCC_to_FIN_DISP, REP_to_SUIT, REP_to_FIN_DISP) %>%
  
  # 2024.02.11: Relevant to what are reasonable inclusion criteria: SoL for med mal cases described at Fla. Stat Ann.
  # Sec. 95.11(4)(a)--(b)
  filter(is.na(OCC_to_SUIT)==T | is.na(OCC_to_SUIT)>=0) %>%
  filter(is.na(OCC_to_SUIT)==T | OCC_to_SUIT<=7)



summary(TIMES)

quantile(TIMES$OCC_to_SUIT, .95, na.rm=T)
quantile(TIMES$OCC_to_REP, .95)
quantile(TIMES$OCC_to_FIN_DISP, .95)

save(TIMES, file='_DATA/TIMES.RData')
