rm(list=ls())

library(tidyverse)
library(RcppRoll)
library(tictoc)

LNUM_NPI_xWalk <- read_csv('_DATA/LNUM_NPI_xWalk.csv')

HIF <- read_csv('_DATA/HIF.csv')

HIF <- HIF %>%
  mutate(ATTENPHYID.letter = str_sub(ATTENPHYID, 1, 2),
         ATTENPHYID.numpart = str_sub(ATTENPHYID, 3, nchar(ATTENPHYID)),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 4)=="0000", str_sub(ATTENPHYID.numpart, 5, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 3)=="000", str_sub(ATTENPHYID.numpart, 4, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 2)=="00", str_sub(ATTENPHYID.numpart, 3, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 1)=="0", str_sub(ATTENPHYID.numpart, 2, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
         ATTENPHYID = paste0(ATTENPHYID.letter, ATTENPHYID.numpart)
  ) %>%
  select(-c(ATTENPHYID.letter, ATTENPHYID.numpart))


DOCS <- HIF %>%
  distinct(ATTENPHYID) 
# set.seed(42069)
# size <- ceiling(.1*nrow(DOCS.vec))
# DOCS.vec <- sample(DOCS.vec$ATTENPHYID, size=size, replace=F) %>%
#    data.frame()

# colnames(DOCS.vec) <- "ATTENPHYID"

# testaroo <- DISCH %>%
# filter(ATTENPHYID %in% DOCS.vec$ATTENPHYID) %>%
#    select(SYS_RECID, YEAR, QTR, ATTENPHYID) %>%
#   group_by(ATTENPHYID, YEAR, QTR) %>%
#   summarize(pat.vol.q=n_distinct(SYS_RECID))

YEAR <- 2000:2015
lenY <- length(YEAR)
lenD <- nrow(DOCS)
DOCS <- rbindlist(replicate(lenY, DOCS, simplify=F))
YEAR <- rep(YEAR, each=lenD)

PV.placeholder <- data.frame(DOCS, YEAR) %>%
  mutate(ATTENPHYID=as.character(ATTENPHYID))

PV <- HIF %>%
  group_by(ATTENPHYID, YEAR) %>%
  summarise(pat.vol_1y = n_distinct(SYS_RECID)) %>%
  mutate(ATTENPHYID=as.character(ATTENPHYID))



PV <- merge(PV.placeholder, PV, by=c("ATTENPHYID", "YEAR"), all.x = T, all.y=T) %>%
  mutate(
    across(
      .cols=everything(),
      .fns= ~ ifelse(is.na(.)==T, 0, .)
    )
  )


PV <- PV %>%
  arrange(ATTENPHYID, YEAR) %>%
  group_by(ATTENPHYID) %>%
  mutate(pat.vol_l1y = lag(pat.vol_1y, 1),
         pat.vol_2y = roll_sum(
           pat.vol_1y,
           2,
           fill=NA,
           align = 'right'
         ),
         pat.vol_l2y = roll_sum(
           lag(pat.vol_1y),
           2,
           fill=NA,
           align='right'
         ),
         pat.vol_3y = roll_sum(
           pat.vol_1y,
           3,
           fill=NA,
           align='right'
         ),
         pat.vol_l3y = roll_sum(
           lag(pat.vol_1y),
           3,
           fill=NA,
           align='right'
         ),
         pat.vol_4y = roll_sum(
           pat.vol_1y,
           4,
           fill=NA,
           align='right'
         ),
         pat.vol_l4y = roll_sum(
           lag(pat.vol_1y),
           4,
           fill=NA,
           align='right'
         ),
         pat.vol_5y = roll_sum(
           pat.vol_1y,
           5,
           fill=NA,
           align = 'right'
         ),
         pat.vol_l5y = roll_sum(
           lag(pat.vol_1y),
           5,
           fill=NA,
           align='right'
         ),
         pat.vol_6y = roll_sum(
           pat.vol_1y,
           6,
           fill=NA,
           align = 'right'
         ),
         pat.vol_l6y = roll_sum(
           lag(pat.vol_1y),
           6,
           fill=NA,
           align='right'
         ),
         pat.vol_7y = roll_sum(
           pat.vol_1y,
           7,
           fill=NA,
           align = 'right'
         ),
         pat.vol_l7y = roll_sum(
           lag(pat.vol_1y),
           7,
           fill=NA,
           align='right'
         ),
         pat.vol_8y = roll_sum(
           pat.vol_1y,
           8,
           fill=NA,
           align='right'
         ),
         pat.vol_l8y = roll_sum(
           lag(pat.vol_1y),
           8,
           fill=NA,
           align='right'
         ),
         pat.vol_9y = roll_sum(
           pat.vol_1y,
           9,
           fill=NA,
           align = 'right'
         ),
         pat.vol_l9y = roll_sum(
           lag(pat.vol_1y),
           9,
           fill=NA,
           align='right'
         ),
         pat.vol_cum = cumsum(pat.vol_1y)-pat.vol_1y
  )



PV <- PV %>%
  filter(YEAR>=2009)


# PV.in <- PV %>%
#   merge(LNUM_NPI_xWalk,
#         by='ATTENPHYID',
#         all=F)
# 
# PV.out <- PV %>%
#   merge(LNUM_NPI_xWalk,
#         by='ATTENPHYID',
#         all=T)
# 
# PV.left <- PV %>%
#   merge(LNUM_NPI_xWalk,
#         by='ATTENPHYID',
#         all.x=T,
#         all.y=F)
# 
# PV.right <- PV %>%
#   merge(LNUM_NPI_xWalk,
#         by='ATTENPHYID',
#         all.x=F,
#         all.y=T)


PV <- PV %>%
  merge(LNUM_NPI_xWalk,
        by='ATTENPHYID',
        all=F)


PV <- PV %>%
  rename(LNUM=ATTENPHYID, 
         NPI=ATTEN_PHYNPI,
         year=YEAR)

write_csv(PV, "_DATA/PV.csv")
