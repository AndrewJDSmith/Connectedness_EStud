rm(list=ls())

library(tidyverse)
library(data.table)
library(tictoc)
library(pryr)

source('paths.r')


LNUM_NPI_xWalk <- read_csv('_DATA/LNUM_NPI_xWalk.csv')

# Below file takes 149 seconds to read with fread


# 2024.02.06: Commenting below just to be able to run faster. For rep package, uncomment
# and delete line below.
# tic()
# PSPP_90 <- fread('_DATA/PSPP_90.csv') %>%
#   mutate(npi1=as.double(npi1),
#          npi2=as.double(npi2))
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
# 2024.09.03 Below is version where filtered by which docs haver primary practice location in Florida
# PSPP_90 <- read_csv('_DATA/PSPP_90_way2.csv')

PV <- read_csv('_DATA/PV.csv')

PSI <- read_csv('_DATA/PSI.csv')
  
MH <- read_csv('_DATA/MH.csv') %>%
  # 2024.02.16: Doc below has 1 NPI, 3 license numbers for some reason. Not sure what's going on here; dropping for now. Figure it out later.
  filter(NPI != 1932197233)
  
 
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

# ANSAMP.B <- ANSAMP %>%
#   filter(spec_prim_1_2 %in% REF_DEST$`Specialty Code` & spec_prim_1_1 %in% REF_SRC$`Specialty Code`) %>%
#   mutate(npi_holding=npi1,
#          npi1=npi2,
#          npi2=npi_holding) %>%
#   select(-npi_holding) %>%
#   mutate(spec_prim_1_holding=spec_prim_1_1,
#          spec_prim_1_1=spec_prim_1_2,
#          spec_prim_1_2=spec_prim_1_holding) %>%
#   select(-spec_prim_1_holding)
# 
# 
# ANSAMP <- rbind(ANSAMP.A, ANSAMP.B)

# 2024.03.26--Order seems to matter. Trying this again with only ANSAMP.A. If this turns out to be folly,
#Uncomment above code and delete line below.
ANSAMP <- ANSAMP.A

ANSAMP.spec <- ANSAMP

write_csv(ANSAMP.spec, '_DATA/ANSAMP.spec.csv')

# docs.ANSAMP <- ANSAMP %>%
#   distinct(npi1)
# 
# idx <- sample(1:nrow(docs.ANSAMP), .01*nrow(docs.ANSAMP))
# 
# docs.samp <- docs.ANSAMP[idx,]
# 
# ANSAMP.samp <- ANSAMP %>%
#   filter(npi1 %in% docs.samp)
  
ANSAMP <- ANSAMP %>%
  # filter(samedaycount==0) %>%
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
  




# hm <- ANSAMP %>% group_by(npi1) %>% summarize(n=n())
# summary(hm)

nrow.ANSAMP <- nrow(ANSAMP)

tic()
ANSAMP <- ANSAMP %>%
  merge(MDPPAS,
        by.x=c('npi1', 'year'),
        by.y=c('NPI', 'year'),
        all=F) 

nrow.ANSAMP.A <- nrow(ANSAMP)

ANSAMP <- ANSAMP %>%
  merge(MH,
        by.x=c('npi1', 'year'),
        by.y=c('NPI', 'year'),
        all=F
        ) 

nrow.ANSAMP.B <- nrow(ANSAMP)

ANSAMP <- ANSAMP %>%
  merge(PV,
        by.x=c('npi1', 'year'),
        by.y=c('NPI', 'year'),
        all=F) 

nrow.ANSAMP.C <- nrow(ANSAMP)

ANSAMP <- ANSAMP %>%
  merge(PSI,
        by.x=c('npi1', 'year'),
        by.y=c('NPI', 'year'),
        all=F) 

nrow.ANSAMP.D <- nrow(ANSAMP)

ANSAMP <- ANSAMP%>%
  rename_with(name_last:PSIAny_rate_3y, .fn = function(.x){paste0(.x, '_1')}) # %>%
  # merge(MDPPAS.ctrl,
  #       by.x=c('npi2', 'year'),
  #       by.y=c('NPI', 'year'),
  #       all=F) %>%
  # rename_with(name_last:tin2_legal_name, .fn = function(.x){paste0(.x, '_2')})

toc()


ANSAMP <- ANSAMP %>%
  mutate(
    across(.cols=starts_with(c('pat.vol_l', 'PSIAny_rate_l')),
           .fns=~ifelse(year==2009, ., 0),
           .names='{.col}_pre.samp'
         )
    )%>%
  group_by(npi1) %>%
  mutate(
    across(.cols=ends_with('pre.samp'),
           .fns=~max(., na.rm=T),
           .names = '{.col}'
           )
         ) %>%
  ungroup() 

# ANSAMP <- read_csv('_DATA/ANSAMP.csv') 

ANSAMP <- ANSAMP %>%
  mutate(lhhi=log(hhi))

ANSAMP.occ <- ANSAMP %>%
  filter(always.treated_occ_1==0) %>%
  group_by(npi1) %>%
  mutate(n_years=n()) %>%
  ungroup() %>%
  filter(n_years>1) %>%
  group_by(npi1) %>%
  mutate(treated = max(ever.sued_occ_all.time_1),
         first.treat=(ever.sued_occ_all.time_1-lag(ever.sued_occ_all.time_1))*year,
         first.treat=ifelse(year==2009 & ever.sued_occ_all.time_1==1, 2009, first.treat),
         first.treat=max(first.treat, na.rm=T)
         )
  

ANSAMP.rep <- ANSAMP %>%
  filter(always.treated_rep_1==0) %>%
  group_by(npi1) %>%
  mutate(n_years=n()) %>%
  ungroup() %>%
  filter(n_years>1) %>%
  group_by(npi1) %>%
  mutate(treated = max(ever.sued_rep_all.time_1),
         first.treat=(ever.sued_rep_all.time_1-lag(ever.sued_rep_all.time_1))*year,
         first.treat=ifelse(year==2009 & ever.sued_rep_all.time_1==1, 2009, first.treat),
         first.treat=max(first.treat, na.rm=T)
  )


ANSAMP.sf <- ANSAMP %>%
  filter(always.treated_sf_1==0) %>%
  group_by(npi1) %>%
  mutate(n_years=n()) %>%
  ungroup() %>%
  filter(n_years>1) %>%
  group_by(npi1) %>%
  mutate(treated = max(ever.sued_sf_all.time_1),
         first.treat=(ever.sued_sf_all.time_1-lag(ever.sued_sf_all.time_1))*year,
         first.treat=ifelse(year==2009 & ever.sued_sf_all.time_1==1, 2009, first.treat),
         first.treat=max(first.treat, na.rm=T)
  )


ANSAMP.fdr <- ANSAMP %>%
  filter(always.treated_fdr_1==0) %>%
  group_by(npi1) %>%
  mutate(n_years=n()) %>%
  ungroup() %>%
  filter(n_years>1) %>%
  group_by(npi1) %>%
  mutate(treated = max(ever.sued_fdr_all.time_1),
         first.treat=(ever.sued_fdr_all.time_1-lag(ever.sued_fdr_all.time_1))*year,
         first.treat=ifelse(year==2009 & ever.sued_fdr_all.time_1==1, 2009, first.treat),
         first.treat=max(first.treat, na.rm=T)
  )




write_csv(ANSAMP, '_DATA/ANSAMP.csv')
write_csv(ANSAMP.occ, '_DATA/ANSAMP.occ.csv')
write_csv(ANSAMP.rep, '_DATA/ANSAMP.rep.csv')
write_csv(ANSAMP.sf, '_DATA/ANSAMP.sf.csv')
write_csv(ANSAMP.fdr, '_DATA/ANSAMP.fdr.csv')
