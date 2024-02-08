rm(list=ls())

library(tidyverse)
library(data.table)

source('paths.r')

# 2023.10.26: Return here and add TIN or other practice identifier for practice fixed
# effects.

for (i in 2009:2015) {
  
  MDPPAS.i <- read_csv(paste0(MDPPAS_path, 'PhysicianData_', i, '.csv')) %>%
    select(year=Year, NPI=npi, name_last, name_first, name_middle,spec_prim_1, spec_prim_1_name, npi_unq_benes, phy_zip1, tin1_legal_name, tin2_legal_name) %>%
    mutate(
           phy_zip1 = str_pad(as.numeric(paste0(phy_zip1)),width = 5, pad="0")
           )

  assign(paste0("MDPPAS_", i), MDPPAS.i)
  
}

MDPPAS.list <- mget(ls(pattern="MDPPAS_2"))

MDPPAS <- rbindlist(MDPPAS.list)%>%
  distinct(NPI, year, .keep_all=T)
  

write_csv(MDPPAS, '_DATA/MDPPAS.csv')


SPEC_GD <- MDPPAS %>%
  distinct(spec_prim_1, .keep_all=T) %>%
  select(`Specialty Code`=spec_prim_1, `Specialty Description`=spec_prim_1_name)

write_csv(SPEC_GD, '_DATA/Specialty_Guide.csv')
