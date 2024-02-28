rm(list=ls())

library(tidyverse)

SPECS <- read_csv('_DATA/Specialty_Guide.csv')

ANSAMP.spec <- read_csv('_DATA/ANSAMP.spec.csv')

ANSAMP.spec.dest <- ANSAMP.spec %>%
  distinct(npi1, .keep_all = T) %>%
  group_by(spec_prim_1_1) %>%
  summarise(n=n()) %>% 
  merge(SPECS,
        by.x='spec_prim_1_1',
        by.y='Specialty Code',
        ) %>%
  arrange(desc(n)) %>%
  select(Specialty=`Specialty Description`, `Number of Physicians`=n)
  

ANSAMP.spec.src <- ANSAMP.spec %>%
  distinct(npi2, .keep_all = T) %>%
  group_by(spec_prim_1_2) %>%
  summarise(n=n()) %>% 
  merge(SPECS,
        by.x='spec_prim_1_2',
        by.y='Specialty Code',
  ) %>%
  arrange(desc(n)) %>%
  select(Specialty=`Specialty Description`, `Number of Physicians`=n)


write_csv(ANSAMP.spec.src, '_DATA/ANSAMP.spec.src.csv')
write_csv(ANSAMP.spec.dest, '_DATA/ANSAMP.spec.dest.csv')
