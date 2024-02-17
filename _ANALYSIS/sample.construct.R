rm(list=ls())

library(tidyverse)
library(data.table)
library(tictoc)
library(pryr)

source('paths.r')


LNUM_NPI_xWalk <- read_csv('_DATA/LNUM_NPI_xWalk.csv')

# Below file takes 149 seconds to read with fread


# 2024.02.06: Commenting below just to be able to run faster. For rep package, uncomment
# and delelete line below.
# tic()
# PSPP_90 <- fread('_DATA/PSPP_90.csv')
# toc()
# 
# 
# 
# PSPP_90 <- PSPP_90 %>%
#   filter(npi1 %in% LNUM_NPI_xWalk$ATTEN_PHYNPI |
#            npi2 %in% LNUM_NPI_xWalk$ATTEN_PHYNPI)
# 
# hm <- PSPP_90 %>%
#   distinct(npi1, npi2)
# 
# 
# write_csv(PSPP_90, '_DATA/PSPP_90.Fla.csv')


PSPP_90 <- read_csv('_DATA/PSPP_90.Fla.csv')

PV <- read_csv('_DATA/PV.csv')

PSI <- read_csv('_DATA/PSI.csv')
  
MH <- read_csv('_DATA/MH.csv') 
  
  # then go to 'HERE NEXT'

MDPPAS <- read_csv('_DATA/MDPPAS.csv')

REF_SRC <- read_csv('_DATA/Referral_Source_Specialties.csv') %>%
  mutate(SoD='S')

REF_DEST <- read_csv('_DATA/Referral_Destination_Specialties.csv') %>%
  mutate(SoD='D')


MDPPAS.spec <- MDPPAS %>%
  select(NPI, year, spec_prim_1)

ANSAMP <- PSPP_90 %>%
  merge(MDPPAS.spec,
        by.x=c('npi1', 'year'),
        by.y=c('NPI', 'year'),
        all=F) %>%
  rename(spec_prim_1_1=spec_prim_1) %>%
  merge(MDPPAS.spec,
        by.x=c('npi2', 'year'),
        by.y=c('NPI', 'year'),
        all=F) %>%
  rename(spec_prim_1_2=spec_prim_1)


ANSAMP.A <- ANSAMP %>%
  filter(spec_prim_1_1 %in% REF_DEST$`Specialty Code` & spec_prim_1_2 %in% REF_SRC$`Specialty Code`)

ANSAMP.B <- ANSAMP %>%
  filter(spec_prim_1_2 %in% REF_DEST$`Specialty Code` & spec_prim_1_1 %in% REF_SRC$`Specialty Code`) %>%
  mutate(npi_holding=npi1,
         npi1=npi2,
         npi2=npi_holding) %>%
  select(-npi_holding) %>%
  mutate(spec_prim_1_holding=spec_prim_1_1,
         spec_prim_1_1=spec_prim_1_2,
         spec_prim_1_2=spec_prim_1_holding) %>%
  select(-spec_prim_1_holding)


ANSAMP <- rbind(ANSAMP.A, ANSAMP.B)


docs.ANSAMP <- ANSAMP %>%
  distinct(npi1)

idx <- sample(1:nrow(docs.ANSAMP), .01*nrow(docs.ANSAMP))

docs.samp <- docs.ANSAMP[idx,]

ANSAMP.samp <- ANSAMP %>%
  filter(npi1 %in% docs.samp)

ANSAMP.samp <- ANSAMP.samp %>%
  filter(samedaycount==0) %>%
  group_by(npi1, year) %>%
  mutate(
    vol_shared=sum(benecount)
  ) %>%
  ungroup() %>%
  mutate(share=benecount/vol_shared) %>%
  group_by(npi1, year) %>%
  summarize(n_sources=n(),
            vol_shared=sum(benecount),
            hhi=sum(share^2)) %>%
  arrange(npi1, year)




MDPPAS.ctrl <- MDPPAS %>%
  select(-c(spec_prim_1, spec_prim_1_name))

idx <- sample(1:nrow(ANSAMP), .01*nrow(ANSAMP))

ANSAMP.samp <- ANSAMP[idx,]


tic()
ANSAMP <- ANSAMP %>%
  merge(MDPPAS.ctrl,
        by.x=c('npi1', 'year'),
        by.y=c('NPI', 'year'),
        all=F) %>%
  merge(MH,
        by.x=c('npi1', 'year'),
        by.y=c('NPI', 'year'),
        all=F
        ) %>%
  merge(PV,
        by.x=c('npi1', 'year'),
        by.y=c('NPI', 'year'),
        all=F) %>%
  merge(PSI,
        by.x=c('npi1', 'year'),
        by.y=c('NPI', 'year'),
        all=F) %>%
  rename_with(name_last:PSIAny_rate_3y, .fn = function(.x){paste0(.x, '_1')}) %>%
  merge(MDPPAS.ctrl,
        by.x=c('npi2', 'year'),
        by.y=c('NPI', 'year'),
        all=F) %>%
  rename_with(name_last:tin2_legal_name, .fn = function(.x){paste0(.x, '_2')})

toc()

  
  

# write_csv(ANSAMP, '_DATA/ANSAMP.csv')
