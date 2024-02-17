rm(list=ls())

library(tidyverse)
library(stringr)
library(data.table)
library(tictoc)

source('paths.r')
source('_ANALYSIS/FUNCTIONS/notin.r')
source('_ANALYSIS/FUNCTIONS/getdate.r')
source('_ANALYSIS/FUNCTIONS/qtr.r')

# 2023.07.13---The below MPL is from the 2021 version. You already have the 2022
# version, and you have a request out for the 2023 update. For now, just keep going
# with 2021 (you are very behind right now). But consider coming back with updated data.

MPL <- read_csv(paste0(MPL_path, "Most_Recent_MPL_Closed_Claims\\MPL_Current_6-15-2021.csv"))

BCD <- read_csv(paste0(MPL_path, "Most_Recent_MPL_Closed_Claims\\Business_Code_Descriptions.csv"))
SCD <- read_csv(paste0(MPL_path, "Most_Recent_MPL_Closed_Claims\\Specialty_Code_Descriptions.csv"))


# 2023.07.13---I'm pretty sure I only imported the HIF to reduce the number of docs I was
# collecting history for because the original process was extremely time consuming.
# I don't think I need to do this anymore, so below code is commented.
# HIF <- read_csv('_DATA/HIF_10-14.csv') %>%
#   mutate(ATTENPHYID.letter = str_sub(ATTENPHYID, 1, 2),
#          ATTENPHYID.numpart = str_sub(ATTENPHYID, 3, nchar(ATTENPHYID)),
#          ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 4)=="0000", str_sub(ATTENPHYID.numpart, 5, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
#          ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 3)=="000", str_sub(ATTENPHYID.numpart, 4, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
#          ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 2)=="00", str_sub(ATTENPHYID.numpart, 3, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
#          ATTENPHYID.numpart = ifelse(str_sub(ATTENPHYID.numpart, 1, 1)=="0", str_sub(ATTENPHYID.numpart, 2, nchar(ATTENPHYID.numpart)), ATTENPHYID.numpart),
#          ATTENPHYID = paste0(ATTENPHYID.letter, ATTENPHYID.numpart)
#   )



#HIF <- read_csv('_DATA/HIF_LONG.csv')
#HIF <- HIF %>%
#  distinct(ATTENPHYID, .keep_all = F)



MPL <- MPL %>%
  mutate(MPL_OCCURRENCE_DATE_p=getdate(MPL_OCCURRENCE_DATE),
         MPL_REPORT_DATE_p=getdate(MPL_REPORT_DATE),
         MPL_SUIT_DATE_p=getdate(MPL_SUIT_DATE),
         MPL_FIN_DATE_DISP_p=getdate(MPL_FIN_DATE_DISP)) %>%
  separate(MPL_OCCURRENCE_DATE_p, sep="/", into=c("MPL_OCCURENCE_MONTH", NA, "MPL_OCCURENCE_YEAR")) %>%
  separate(MPL_REPORT_DATE_p, sep="/", into=c("MPL_REPORT_MONTH", NA, "MPL_REPORT_YEAR")) %>%
  separate(MPL_SUIT_DATE_p, sep="/", into=c("MPL_SUIT_MONTH", NA, "MPL_SUIT_YEAR")) %>%
  separate(MPL_FIN_DATE_DISP_p, sep="/", into=c("MPL_FIN_DISP_MONTH", NA, "MPL_FIN_DISP_YEAR")) %>%
  mutate(MPL_OCCURENCE_QTR=qtr(MPL_OCCURENCE_MONTH),
         MPL_OCCURENCE_YEAR=as.double(MPL_OCCURENCE_YEAR),
         MPL_REPORT_QTR=qtr(MPL_REPORT_MONTH),
         MPL_REPORT_YEAR=as.double(MPL_REPORT_YEAR),
         MPL_SUIT_QTR=qtr(MPL_SUIT_MONTH),
         MPL_SUIT_YEAR=as.double(MPL_SUIT_YEAR),
         MPL_FIN_DISP_QTR=qtr(MPL_FIN_DISP_MONTH),
         MPL_FIN_DISP_YEAR=as.double(MPL_FIN_DISP_YEAR)
         )


MPL <- merge(MPL, BCD, by="INSD_MPL_PROF_BUS", all.x = T)
MPL <- merge(MPL, SCD, by.x="INSD_MPL_SEPC_CODE", by.y = "SPECIALTY_CODE", all.x = T)


MPL <- MPL %>%
  filter(MPL_OCCURENCE_YEAR >= 1994) %>%
  filter(is.na(INSD_ENTITY)==T) %>%
  filter(INSD_MPL_PROF_BUS %in% c(100, 108, 111) & ((SPECIALTY_TYPE %in% c("MD", "DO") | is.na(SPECIALTY_TYPE)==T))) %>%
  filter(str_sub(INSD_MPL_PROF_LNUM, 1, 2) != "PA" & str_sub(INSD_MPL_PROF_LNUM, -2, -1) != "PA") %>%
  filter(str_sub(INSD_MPL_PROF_LNUM, 1, 2) != "RN" & str_sub(INSD_MPL_PROF_LNUM, -2, -1) != "RN") %>%
  filter(str_sub(INSD_MPL_PROF_LNUM, 1, 1) %!in% c("A", "B", "C", "T", "P")) %>%
  filter(INSD_MPL_PROF_LNUM != "0") %>%
  mutate(suit_filed=ifelse(is.na(MPL_SUIT_YEAR)==F & is.na(MPL_SUIT_QTR)==F & is.na(MPL_CASENUM)==F & is.na(MPL_SUIT_COUNTY)==F, 1, 0),
         MPL_INDEMNITY_PAID=ifelse(is.na(MPL_INDEMNITY_PAID)==T, 0, MPL_INDEMNITY_PAID),
         MPL_INDEMNITY_PAID=abs(MPL_INDEMNITY_PAID),
         dmg_prop=MPL_INDEMNITY_PAID/INSD_PER_CLAIM_LIMIT
         ) %>%
# 2023.07.13: The code below does a pretty good job of cleaning up license numbers.
# It could be better still. At some point, come back and really clean this shit up.
    mutate(INSD_MPL_PROF_LNUM=ifelse(str_sub(INSD_MPL_PROF_LNUM, 1, 5)=="FL ME" & SPECIALTY_TYPE=="MD",
                                   str_sub(INSD_MPL_PROF_LNUM, 4, nchar(INSD_MPL_PROF_LNUM)),
                                   INSD_MPL_PROF_LNUM),
         INSD_MPL_PROF_LNUM=ifelse(str_sub(INSD_MPL_PROF_LNUM, 1, 4)=="FLME" & SPECIALTY_TYPE=="MD",
                                   str_sub(INSD_MPL_PROF_LNUM, 3, nchar(INSD_MPL_PROF_LNUM)),
                                   INSD_MPL_PROF_LNUM),
         INSD_MPL_PROF_LNUM=ifelse(str_sub(INSD_MPL_PROF_LNUM, 1, 3)=="FL " & SPECIALTY_TYPE=="MD",
                                   paste0("ME", str_sub(INSD_MPL_PROF_LNUM, 4, nchar(INSD_MPL_PROF_LNUM))),
                                   INSD_MPL_PROF_LNUM),
         INSD_MPL_PROF_LNUM=ifelse(str_sub(INSD_MPL_PROF_LNUM, 1, 2)=="FL" & SPECIALTY_TYPE=="MD",
                                   paste0("ME", str_sub(INSD_MPL_PROF_LNUM, 3, nchar(INSD_MPL_PROF_LNUM))),
                                   INSD_MPL_PROF_LNUM),
         INSD_MPL_PROF_LNUM=ifelse(str_sub(INSD_MPL_PROF_LNUM, 1, 2) %!in% c("ME", "OS") & SPECIALTY_TYPE=="MD",
                                   paste0("ME", INSD_MPL_PROF_LNUM),
                                   INSD_MPL_PROF_LNUM),
         INSD_MPL_PROF_LNUM=ifelse(str_sub(INSD_MPL_PROF_LNUM, 1, 2) %!in% c("ME", "OS") & SPECIALTY_TYPE=="DO",
                                   paste0("OS", INSD_MPL_PROF_LNUM),
                                   INSD_MPL_PROF_LNUM),
         INSD_MPL_PROF_LNUM=ifelse(str_sub(INSD_MPL_PROF_LNUM, 1, 3) %in% c("ME ","ME-") & SPECIALTY_TYPE=="MD",
                                   paste0("ME", str_sub(INSD_MPL_PROF_LNUM, 4, nchar(INSD_MPL_PROF_LNUM))),
                                   INSD_MPL_PROF_LNUM
                                   ),
         INSD_MPL_PROF_LNUM=ifelse(str_sub(INSD_MPL_PROF_LNUM, 1, 3) %in% c("OS ","OS-") & SPECIALTY_TYPE=="DO",
                                   paste0("OS", str_sub(INSD_MPL_PROF_LNUM, 4, nchar(INSD_MPL_PROF_LNUM))),
                                   INSD_MPL_PROF_LNUM
                                   ),
         INSD_MPL_PROF_LNUM=gsub(' ', '', INSD_MPL_PROF_LNUM, fixed = T)
  ) %>%
  filter(is.na(INSD_MPL_PROF_LNUM)==F) %>%
# 2023.07.13: I originally swithed the names a long time ago and wrote these two
# string cleaning processes (the one above and the one below) each with a different
# name. At some point, it would be good to make this consistent, but not right now.
  rename(LNUM=INSD_MPL_PROF_LNUM) %>%
  mutate(LNUM=gsub('<E', '', LNUM)) %>%
  mutate(LNUM.letter = str_sub(LNUM, 1, 2),
         LNUM.numpart = str_sub(LNUM, 3, nchar(LNUM)),
         LNUM.numpart = ifelse(str_sub(LNUM.numpart, 1, 4)=="0000", str_sub(LNUM.numpart, 5, nchar(LNUM.numpart)), LNUM.numpart),
         LNUM.numpart = ifelse(str_sub(LNUM.numpart, 1, 3)=="000", str_sub(LNUM.numpart, 4, nchar(LNUM.numpart)), LNUM.numpart),
         LNUM.numpart = ifelse(str_sub(LNUM.numpart, 1, 2)=="00", str_sub(LNUM.numpart, 3, nchar(LNUM.numpart)), LNUM.numpart),
         LNUM.numpart = ifelse(str_sub(LNUM.numpart, 1, 1)=="0", str_sub(LNUM.numpart, 2, nchar(LNUM.numpart)), LNUM.numpart),
         LNUM = paste0(LNUM.letter, LNUM.numpart)
  ) %>%
  filter(LNUM != 'ME') %>%
  distinct(MPL_SEQID_MPL, .keep_all=T)


SEV_Guide <- MPL %>%
  distinct(MPL_SEV_OF_INJURY_PI, .keep_all=T) %>%
  select(MPL_SEV_OF_INJURY_PI, SEVERITY_DESC) %>%
  arrange(desc(MPL_SEV_OF_INJURY_PI))

write_csv(MPL, '_DATA/MPL.csv')
write_csv(SEV_Guide, '_DATA/SEV_Guide.csv')


# 2023.07.23: I don't think creating this other dataframe was a great idea. Getting rid of this.
# The code that removes leading zeros was copied and pasted above.
# SHORTER <- MPL %>%
#   select(LNUM=INSD_MPL_PROF_LNUM, MPL_SEQID_MPL, MPL_OCCURENCE_QTR, 
#          MPL_OCCURENCE_YEAR, MPL_REPORT_QTR, MPL_REPORT_YEAR, MPL_SUIT_QTR, 
#          MPL_SUIT_YEAR, MPL_FIN_DISP_QTR, MPL_FIN_DISP_YEAR, MPL_IP_SEX, 
#          MPL_IP_STATE,MPL_SEV_OF_INJURY_PI, SEVERITY_DESC, MPL_STAGE_OF,
#          STAGE_OF_DESC, MPL_SETTLE, MPL_INDEMNITY_PAID, INSD_PER_CLAIM_LIMIT,
#          SUIT_FILED=suit_filed, DMG_PROP=dmg_prop
#          ) %>%
  # mutate(LNUM.letter = str_sub(LNUM, 1, 2),
  #        LNUM.numpart = str_sub(LNUM, 3, nchar(LNUM)),
  #        LNUM.numpart = ifelse(str_sub(LNUM.numpart, 1, 4)=="0000", str_sub(LNUM.numpart, 5, nchar(LNUM.numpart)), LNUM.numpart),
  #        LNUM.numpart = ifelse(str_sub(LNUM.numpart, 1, 3)=="000", str_sub(LNUM.numpart, 4, nchar(LNUM.numpart)), LNUM.numpart),
  #        LNUM.numpart = ifelse(str_sub(LNUM.numpart, 1, 2)=="00", str_sub(LNUM.numpart, 3, nchar(LNUM.numpart)), LNUM.numpart),
  #        LNUM.numpart = ifelse(str_sub(LNUM.numpart, 1, 1)=="0", str_sub(LNUM.numpart, 2, nchar(LNUM.numpart)), LNUM.numpart),
  #        LNUM = paste0(LNUM.letter, LNUM.numpart)
  #        )

# Undated---Make sure the following dataframe gets renamed "TEST"
# 2023.07.13---I think making TEST was silly. Commenting it for now.
# TEST <- MPL %>%
#   select(LNUM=INSD_MPL_PROF_LNUM, MPL_REPORT_QTR, MPL_REPORT_YEAR, MPL_INDEMNITY_PAID, SUIT_FILED=suit_filed, DMG_PROP=dmg_prop
#          )%>%
#   group_by(LNUM) %>%
#   mutate(no.suits=n()) %>%
#   filter(LNUM %in% c("ME50893", "ME72697", "ME36594")) %>%
#   arrange(LNUM, MPL_REPORT_YEAR, MPL_REPORT_QTR)


# DOCS_HIF <- HIF %>%
#   distinct(ATTENPHYID) %>%
#   rename(LNUM=ATTENPHYID)


# DOCS_I <- intersect(DOCS_HIF, DOCs_MPL)


# DOCS_S <- as.data.frame(sample(DOCS_I$LNUM, floor(.01*nrow(DOCS_I)))) %>%
#   rename(LNUM=`sample(DOCS_I$LNUM, floor(0.01 * nrow(DOCS_I)))`)


# TEST_by.report <- MPL %>%
#   filter(LNUM %in% c("ME50893", "ME72697", "ME36594")) %>%
#   arrange(LNUM, MPL_REPORT_YEAR, MPL_REPORT_QTR) %>%
#   group_by(LNUM, MPL_REPORT_YEAR, MPL_REPORT_QTR) %>%
#   summarize(n=n()) %>%
#   ungroup() %>%
#   group_by(LNUM) %>%
#   mutate(n_total = cumsum(n)) %>%
#   ungroup()
  




