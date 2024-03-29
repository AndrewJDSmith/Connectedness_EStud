rm(list=ls())

library(tidyverse)
library(foreign)
library(data.table)
library(zoo)
library(tictoc)
library(RcppRoll)


source('paths.r')

`%!in%` <- Negate(`%in%`)

# NOTE TO SELF: gotta go back and mutate NPI to character to account for leading zeros

# MSDRGXWLK <- read_csv(paste0(GEN.DATA_path, "msdrg2drg.csv")) %>%
#   select(DRG=drg,
#          MSDRG=msdrg,
#          DRGDESC=drgdesc)


# 2000--2004

for(y in 0:4){
  for (q in 1:4) {
  
  HIF.i <- read.dbf(paste0(HIF_path, "200", y, "_DBF\\0",y,"_Q", q, "In.dbf")) %>%
    #filter(ADMTYPE==3) %>%
    #filter(PAYER=="A") %>%
    #filter(ADMSRC=='01' | ADMSRC=='02') #%>%
    select(SYS_RECID, YEAR, QTR, ZIPCODE, 
           ATTENPHYID,
           OPERPHYID
    ) %>%
    mutate(ATTEN_PHYNPI=NA,
           OPER_PHYNPI=NA)
  
  
  assign(paste0("HIF_200", y, ".", q), HIF.i)

}
}

# 2005

for(q in 1:4){

  HIF.i <- read.dbf(paste0(HIF_path, "2005_DBF\\05Q", q, "_INPT.dbf")) %>%
    #filter(ADMTYPE==3) %>%
    #filter(PAYER=="A") %>%
    #filter(ADMSRC=='01' | ADMSRC=='02') #%>%
    select(SYS_RECID, YEAR, QTR, ZIPCODE, 
           ATTENPHYID,
           OPERPHYID
    ) %>%
    mutate(ATTEN_PHYNPI=NA,
           OPER_PHYNPI=NA)


  assign(paste0("HIF_2005.", q), HIF.i)

}



# 2006

for(q in 1:4){

  HIF.i <- read_csv(paste0(HIF_path, "2006_CSV\\INP06Q", q, ".csv")) %>%
    #filter(ADMTYPE==3) %>%
    #filter(PAYER=="A") %>%
    #filter(ADMSRC=='01' | ADMSRC=='02') %>%
    select(SYS_RECID, YEAR, QTR, ZIPCODE, 
           ATTENPHYID,
           OPERPHYID
    )%>%
    mutate(ATTEN_PHYNPI=NA,
           OPER_PHYNPI=NA)
  

  assign(paste0("HIF_2006.", q), HIF.i)

}


# 2007q1-3

for(q in 1:3){
  
  HIF.i <- read_delim(paste0(HIF_path, "2007_TAB_DEL\\07Q", q, ".dat"), delim = '\t') %>%
    select(SYS_RECID, YEAR, QTR, ZIPCODE,  
           ATTENPHYID,
           OPERPHYID
           )%>%
    mutate(ATTEN_PHYNPI=NA,
           OPER_PHYNPI=NA)
  
  
  
  assign(paste0("HIF_2007.", q), HIF.i)
  
}


# rm(HIF.i)
# 
# DISCH.1 <- rbindlist(mget(ls(pattern="HIF_")), use.names = T)
# 
# print(nrow(DISCH.1))
# 
# rm(list=ls(pattern='HIF_'))
# 
# DISCH.1 <- merge(DISCH.1, MSDRGXWLK, by='DRG', all.x=T)
# 
# print(nrow(DISCH.1))

#2007q4

HIF_2007.4 <- read_delim(paste0(HIF_path, "2007_TAB_DEL\\07Q4.dat"), delim = '\t') %>%
  select(SYS_RECID, YEAR, QTR, ZIPCODE,  
         ATTENPHYID,
         OPERPHYID
  ) %>%
  mutate(ATTEN_PHYNPI=NA,
         OPER_PHYNPI=NA)


# 2008

for(q in 1:4) {
  
  HIF.i <- read_csv(paste0(HIF_path, "2008_CSV\\08Q", q, "_IN.csv")) %>%
    select(SYS_RECID, YEAR, QTR, ZIPCODE,  
           ATTENPHYID,
           OPERPHYID
    ) %>%
    mutate(ATTEN_PHYNPI=NA,
           OPER_PHYNPI=NA)
  
  
  
  assign(paste0("HIF_2008.", q), HIF.i)
  
}


# 2009

for (q in 1:4){
  
  HIF.i <- read_delim(paste0(HIF_path, "2009_TAB_DEL\\Inp09Q", q, ".dat"), delim='\t') %>%
    select(SYS_RECID, YEAR, QTR, ZIPCODE,  
           ATTENPHYID,
           OPERPHYID
           )%>%
    mutate(ATTEN_PHYNPI=NA,
           OPER_PHYNPI=NA)
  
  
  
  assign(paste0("HIF_2009.", q), HIF.i)
}




# 2010--11

for (y in 10:11) {
  for (q in 1:4) {
    
    full.year <- as.character(2000 + y)
    short.year <- substr(full.year, 3, 4)
    HIF.i <- read_delim(paste0(HIF_path, full.year, "_TAB_DEL\\Inp", short.year, "Q", q, ".dat"), delim='\t') %>%
      select(SYS_RECID, YEAR, QTR, ZIPCODE, 
             ATTENPHYID=ATTEN_PHYID,
             ATTEN_PHYNPI,
             OPERPHYID=OPER_PHYID,
             OPER_PHYNPI
      ) 
    
    assign(paste0("HIF_", full.year, ".", q), HIF.i)
  }
}




# 2012--13

for(y in 12:13){
  for(q in 1:4){
    
    full.year <- as.character(2000+y)
    short.year <- substr(full.year, 3, 4)
    HIF.i <- read_delim(paste0(HIF_path, full.year, "_TAB_DEL\\Inpt", short.year, "Q", q, ".dat"), delim = '\t') %>%
      select(SYS_RECID, YEAR, QTR, ZIPCODE,  
             ATTENPHYID=ATTEN_PHYID,
             ATTEN_PHYNPI,
             OPERPHYID=OPER_PHYID,
             OPER_PHYNPI
      ) 
    
    assign(paste0("HIF_", full.year, ".", q), HIF.i)
  }
}

# 2014

for(q in 1:4){
  
  HIF.i <- read_csv(paste0(HIF_path, "2014_CSV\\IN2014Q", q, ".csv")) %>%
    select(SYS_RECID, YEAR, QTR, ZIPCODE, 
           ATTENPHYID=ATTEN_PHYID,
           ATTEN_PHYNPI,
           OPERPHYID=OPER_PHYID,
           OPER_PHYNPI,
    )
  
  assign(paste0("HIF_2014.", q), HIF.i)
  
} 

# 2015

for(q in c(1:2, 4)) {
  
  HIF.q <- read_csv(paste0(HIF_path, "2015_CSV\\15Q", q, "_IN.csv")) %>%
    #filter(ADM_PRIOR==3) %>%
    # filter(PAYER=='A') %>%
    #filter(ADMSRC=='01' | ADMSRC=='02') %>%
    select(SYS_RECID, YEAR, QTR, ZIPCODE, 
           ATTENPHYID=ATTEN_PHYID,
           ATTEN_PHYNPI,
           OPERPHYID=OPER_PHYID,
           OPER_PHYNPI
    )
  
  
  assign(paste0("HIF_2015.", q), HIF.q)
}


# 2015Q3

HIF_2015.3 <- read_csv(paste0(HIF_path, "2015_CSV\\15Q3_INPT.csv")) %>%
  #filter(ADM_PRIOR==3) %>%
  # filter(PAYER=='A') %>%
  #filter(ADMSRC=='01' | ADMSRC=='02') %>%
  select(SYS_RECID, YEAR, QTR, ZIPCODE, 
         ATTENPHYID=ATTEN_PHYID,
         ATTEN_PHYNPI,
         OPERPHYID=OPER_PHYID,
         OPER_PHYNPI
  )




remove(HIF.i, HIF.q)

HIF.list <- mget(ls(pattern='HIF_2'))

HIF <- rbindlist(HIF.list, use.names = T) %>%
  filter(substr(ATTENPHYID, 1, 2) %in% c('ME', 'OS')) %>%
  filter(ATTENPHYID != 'ME')


write_csv(HIF, '_DATA/HIF.csv')

# rm(HIF.i)
# 
# DISCH.2 <- rbindlist(mget(ls(pattern="HIF_")), use.names = T)
# 
# print(nrow(DISCH.2))
# 
# rm(list=ls(pattern='HIF_'))
# 
# DISCH.2 <- merge(DISCH.2, MSDRGXWLK, by='MSDRG', all.x=T)
# 
# print(nrow(DISCH.2))
# 
# DISCH <- rbindlist(mget(ls(pattern = "DISCH.")), use.names = T)
# 
# rm(DISCH.1, DISCH.2, MSDRGXWLK)
# 
# DISCH <- DISCH %>%
#   mutate(ADMITDIAG=ifelse(YEAR<2006, 999, ADMITDIAG),
#          ETHNICITY=case_when(YEAR >= 2010 ~ ETHNICITY,
#                              YEAR < 2010 & RACE %in% c(5, 6) ~ "E1",
#                              YEAR < 2010 & RACE %in% c(1,2,3,4) ~ "E2",
#                              YEAR < 2010 & RACE %in% c(7,8) ~ "E7"
#          ),
#          FEMALE=case_when(GENDER==2 | SEX=='F' ~ 1,
#                           GENDER==1 | SEX=='M' ~ 0,
#                           GENDER==3 | SEX=='U' ~ 9),
#          RACE=case_when(RACE == 1 ~ 1,
#                         YEAR < 2010 & RACE==2 ~ 2,
#                         YEAR >=2010 & RACE %in% c(2,4) ~ 2,
#                         YEAR < 2010 & RACE %in% c(3,6) ~ 3,
#                         YEAR >=2010 & RACE==3 ~ 3,
#                         YEAR < 2010 & RACE %in% c(4,5) ~ 4,
#                         YEAR >=2010 & RACE==5 ~ 4,
#                         YEAR < 2010 & RACE==7 ~ 5,
#                         YEAR >=2010 & RACE==6 ~ 5,
#                         YEAR < 2010 & RACE==8 ~ 6,
#                         YEAR >=2010 & RACE==7 ~ 6
#          )
#   )
# 
# 
# write_csv(DISCH, '_DATA/HIF_2005-14_All.Payers.csv')
# 
# # Below is attempt to get total volume over previous three years. It is not working well
# 
# DISCH <- DISCH %>%
#   select(SYS_RECID, YEAR, QTR, ATTENPHYID) %>%
#   filter(substr(ATTENPHYID, 1, 1) %!in% c("A", "B", "C", "D", "G", "H", "L")) %>%
#   filter(substr(ATTENPHYID, 1, 3) != "MFC") %>%
#   mutate(ATTENPHYID = ifelse(substr(ATTENPHYID, 1, 2)=="FL", 
#                              substr(ATTENPHYID, 3, nchar(ATTENPHYID)),
#                              ATTENPHYID)
#          ) %>%
#   mutate(ATTENPHYID.letter = str_sub(ATTENPHYID, 1, 2),
#          ATTENPHYID.numpart = str_sub(ATTENPHYID, 3, nchar(ATTENPHYID)),
#          ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 4)=="0000", str_sub(ATTENPHYID.numpart, 5, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
#          ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 3)=="000", str_sub(ATTENPHYID.numpart, 4, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
#          ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 2)=="00", str_sub(ATTENPHYID.numpart, 3, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
#          ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 1)=="0", str_sub(ATTENPHYID.numpart, 2, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
#          ATTENPHYID = paste0(ATTENPHYID.letter, ATTENPHYID.numpart)
#   )





