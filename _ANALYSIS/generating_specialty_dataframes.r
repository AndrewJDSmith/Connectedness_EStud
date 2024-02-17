rm(list=ls())

library(tidyverse)

SPEC_GUIDE <- read_csv('_DATA/Specialty_Guide.csv')

REF_SRC <- SPEC_GUIDE %>%
  filter(`Specialty Code` %in% c('41',
                                 '08',
                                 '25',
                                 '16',
                                 '12',
                                 '38',
                                 '23',
                                 '37',
                                 '01')
         )

REF_DEST <- SPEC_GUIDE %>%
  filter(`Specialty Code` %in% c('02',
                                 '20',
                                 # '46',
                                 # '66',
                                 # '13',
                                 '40',
                                 '06',
                                 # '18',
                                 'C3',
                                 # '04',
                                 '10',
                                 '33',
                                 # '48',
                                 '83',
                                 '39',
                                 '14',
                                 '78',
                                 '90',
                                 '92',
                                 '77',
                                 '94',
                                 '28',
                                 '36',
                                 'C7',
                                 '91',
                                 'C9',
                                 # '82',
                                 '76',
                                 '98')
         )


write_csv(REF_SRC, '_DATA/Referral_Source_Specialties.csv')
write_csv(REF_DEST, '_DATA/Referral_Destination_Specialties.csv')
