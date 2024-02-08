rm(list=ls())

library(tidyverse)
library(RcppRoll)
library(stringr)
library(data.table)
library(tictoc)


MPL <- read_csv('_DATA/MPL.csv')

# 2024.02.07: YOU ARE HERE; Come back and get '.q' vs '' consistent

MH_by.occ <- MPL %>%
  filter(MPL_OCCURENCE_YEAR<=2015) %>%
  mutate(year=MPL_OCCURENCE_YEAR) %>%
  group_by(LNUM, year) %>%
  summarize(no.suits_occ=n(),
            tot.dmg_occ=sum(MPL_INDEMNITY_PAID, na.rm=T)
  ) 

MH_by.suit <- MPL %>%
  filter(MPL_SUIT_YEAR<=2015) %>%
  mutate(year=MPL_SUIT_YEAR) %>%
  group_by(LNUM, year) %>%
  summarize(no.suits_sf=n(),
            tot.dmg_sf=sum(MPL_INDEMNITY_PAID, na.rm=T)
  ) 


MH_by.rep <- MPL %>%
  filter(MPL_REPORT_YEAR<=2015) %>%
  mutate(year=MPL_REPORT_YEAR) %>%
  group_by(LNUM, year) %>%
  summarize(no.suits_rep = n(),
            tot.dmg_rep = sum(MPL_INDEMNITY_PAID, na.rm=T)
  ) 

MH_by.fdr <- MPL %>%
  filter(MPL_FIN_DISP_YEAR<=2015) %>%
  mutate(year=MPL_FIN_DISP_YEAR,
         INDEMNITY_DOH = ifelse(MPL_INDEMNITY_PAID>100000, MPL_INDEMNITY_PAID, 0)) %>%
  group_by(LNUM, year) %>%
  summarize(no.suits_fdr = n(),
            tot.dmg_fdr = sum(MPL_INDEMNITY_PAID, na.rm=T),
            no.suits_NPDB = sum(MPL_INDEMNITY_PAID>0),
            no.suits_DOH.el = sum(MPL_INDEMNITY_PAID>100000)
            )


DOCS <- MPL %>%
  filter(MPL_OCCURENCE_YEAR<=2015 | MPL_REPORT_YEAR<=2015 | MPL_SUIT_YEAR <=2015 | MPL_FIN_DISP_YEAR<=2015) %>%
  distinct(LNUM)


YEAR <- 1994:2015
lenY <- length(YEAR)
lenD <- nrow(DOCS)
DOCS <- rbindlist(replicate(lenY, DOCS, simplify=F))
YEAR <- rep(YEAR, each=lenD)

MH_placeholder <- data.frame(DOCS, YEAR) %>%
  rename(year=YEAR)

MH <- MH_placeholder %>%
  merge(MH_by.occ, by=c('LNUM', 'year'), all.x=T, all.y=T) %>%
  merge(MH_by.rep, by=c('LNUM', 'year'), all.x=T, all.y=T) %>%
  merge(MH_by.fdr, by=c('LNUM', 'year'), all.x=T, all.y=T) %>%
  merge(MH_by.suit, by=c('LNUM', 'year'), all.x=T, all.y=T)

MH <- MH %>%
  mutate(
    across(
      .cols=everything(),
      .fns=~ifelse(is.na(.)==T, 0, .)
    )
  )

MH <- MH %>%
  group_by(LNUM) %>%
  arrange(LNUM, year) %>%
  mutate(
    no.suits_rep_all.time = cumsum(no.suits_rep)-no.suits_rep,
    ever.sued_rep_all.time = ifelse(no.suits_rep_all.time>0, 1, 0),

    no.suits_occ_all.time = cumsum(no.suits_occ)-no.suits_occ,
    ever.sued_occ_all.time = ifelse(no.suits_occ_all.time>0, 1, 0),

    no.suits_fdr_all.time = cumsum(no.suits_fdr) - no.suits_fdr,
    ever.sued_fdr_all.time = ifelse(no.suits_fdr_all.time>0, 1, 0),

    no.suits_sf_all.time = cumsum(no.suits_sf)-no.suits_sf,
    ever.sued_sf_all.time = ifelse(no.suits_sf_all.time>0, 1, 0)
    # ,

    # no.suits_NPDB_all.time = cumsum(no.suits_NPDB)-no.suits_NPDB,
    # ever.sued_NPDB_all.time = ifelse(no.suits_NPDB_all.time>0, 1, 0),
    # tot.dmg_NPDB_all.time = cumsum(tot.dmg_fdr)-tot.dmg_fdr,
    # no.suits_DOH = roll_sum(lag(no.suits_DOH.el), n=40, fill=NA, align = 'right'),
    # ever.sued_DOH = ifelse(no.suits_DOH>0, 1, 0),
    # tot.dmg_DOH = roll_sum(lag(tot.dmg_DOH.el), n=40, fill=NA, align = 'right')
  ) %>%
  ungroup()

MH <- MH %>%
  filter(year>=2009) %>%
  select(-ends_with('el'))


write_csv(MH, '_DATA/MH.csv')
