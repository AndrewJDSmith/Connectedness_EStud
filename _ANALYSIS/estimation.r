rm(list=ls())

library(tidyverse)
library(did)
library(staggered)
library(data.table)

ANSAMP.occ <- read_csv('_DATA/ANSAMP.occ.csv') %>%
  mutate(spec_prim_1_1 = as.factor(spec_prim_1_1),
         first.treat.rsa = ifelse(first.treat==0, Inf, first.treat)
         )
ANSAMP.rep <- read_csv('_DATA/ANSAMP.rep.csv') %>%
  mutate(spec_prim_1_1 = as.factor(spec_prim_1_1),
         first.treat.rsa = ifelse(first.treat==0, Inf, first.treat)
         )
ANSAMP.sf <- read_csv('_DATA/ANSAMP.sf.csv') %>%
  mutate(spec_prim_1_1 = as.factor(spec_prim_1_1),
         first.treat.rsa = ifelse(first.treat==0, Inf, first.treat)
         )
ANSAMP.fdr <- read_csv('_DATA/ANSAMP.fdr.csv') %>%
  mutate(spec_prim_1_1 = as.factor(spec_prim_1_1),
         first.treat.rsa = ifelse(first.treat==0, Inf, first.treat)
         )




ggplot() +
  geom_histogram(data=filter(ANSAMP.rep, treated==0),
                 aes(x=pat.vol_l2y_1_pre.samp, y = stat(count / sum(count))),
                 color='blue') +
  geom_histogram(data=filter(ANSAMP.rep, treated==1),
                 aes(x=pat.vol_l2y_1_pre.samp, y = stat(count / sum(count))),
                 color='red')


ggplot() +
  geom_histogram(data=filter(ANSAMP.rep, treated==0),
                 aes(x=PSIAny_rate_l2y_1_pre.samp, y = stat(count / sum(count))),
                 color='blue') +
  geom_histogram(data=filter(ANSAMP.rep, treated==1),
                 aes(x=PSIAny_rate_l2y_1_pre.samp, y = stat(count / sum(count))),
                 color='red')



timing <- c('occ', 'rep', 'sf', 'fdr')
outcomes <- c('n_sources', 'vol_shared', 'hhi', 'lhhi')

xformula.tiv <- ~ pat.vol_l2y_1_pre.samp + PSIAny_rate_l2y_1_pre.samp + spec_prim_1_1
xformula.tv <- ~ pat.vol_l2y_1 + PSIAny_rate_l2y_1 + spec_prim_1_1



for (i in timing){
  for (j in outcomes){
    
    att.i.j.uncond <- att_gt(yname=j,
                             tname='year',
                             idname='npi1',
                             gname='first.treat',
                             xformla=NULL,
                             data=get(paste0('ANSAMP.', i))
                             )
    
    att.i.j.cond.tiv <- att_gt(yname=j,
                               tname='year',
                               idname='npi1',
                               gname='first.treat',
                               xformla=xformula.tiv,
                               data=get(paste0('ANSAMP.', i))
                               )
    
    att.i.j.cond.tv <- att_gt(yname=j,
                              tname='year',
                              idname='npi1',
                              gname='first.treat',
                              xformla=xformula.tv,
                              data=get(paste0('ANSAMP.', i))
                              )
    
  
  
    assign(paste0('att.gt.', i, '.', j, '.uncond'), att.i.j.uncond)
    assign(paste0('att.gt.', i, '.', j, '.cond.tiv'), att.i.j.cond.tiv)
    assign(paste0('att.gt.', i, '.', j, '.cond.tv'), att.i.j.cond.tv)
    
  }
}

rm(att.i.j.cond.tiv, att.i.j.cond.tv, att.i.j.uncond)

results.att.gt <- mget(ls(pattern='att.gt.'))

for (i in 1:length(results.att.gt)) {
  
  a <- aggte(results.att.gt[[i]], type='group')
  b <- a$overall.att
  c <- a$overall.se
  
  d <- data.frame(estimate=b, se=c) %>%
    mutate(lower=estimate-1.96*se,
           upper=estimate+1.96*se,
           significant=ifelse(abs(estimate)-1.96*se>0, 1, 0),
           sign = ifelse(estimate>0, 'positive', 'negative'),
           model=names(results.att.gt)[[i]]
           )
  
  assign(paste0('results.overall.att.gt.', names(results.att.gt)[[i]]), d)
  
  a.p <- aggte(results.att.gt[[i]], type='dynamic') 
  
  e <- a.p$att.egt
  f <- a.p$se.egt
  g <- a.p$crit.val.egt
  h <- data.frame(estimate=e, se=f) %>%
    mutate(lower=estimate-g*se,
           upper=estimate+g*se,
           significant=ifelse(abs(estimate)-g*se>0, 1, 0),
           sign = ifelse(estimate>0, 'positive', 'negative'),
           model=names(results.att.gt)[[i]]
           )
           
           assign(paste0('results.estud.cs.', names(results.att.gt)[[i]]), h)

}


results.cs.overall.tab <- rbindlist(mget(ls(pattern='results.overall.att.gt.')))

results.cs.estud.tab <- rbindlist(mget(ls(pattern='results.estud.cs.')))

rm(list=ls(pattern='results.overall.att.'))
rm(list=ls(pattern='results.estud.cs.att.'))



# Below code shows that no event-study has more than one statistically significant estimate

cs.estud.overview <- results.cs.estud.tab %>%
  group_by(model) %>%
  summarize(n_significant=sum(significant))



for (i in timing){
  for (j in outcomes){
    
    
    r_sa.2023.i.j.overall.all <- staggered(get(paste0('ANSAMP.', i)),
                                      i='npi1',
                                      t='year',
                                      g='first.treat.rsa',
                                      y=j,
                                      estimand='cohort'
                                      ) %>%
      mutate(significant=ifelse(abs(estimate)-1.96*se>0, 1, 0),
             sign = ifelse(estimate>0, 'positive', 'negative'),
             model=paste0('r_sa.2023.', i, '.', j, '.overall.all')
             )
    
    r_sa.2023.i.j.overall.trtonly <- staggered(filter(get(paste0('ANSAMP.', i)), 
                                                     is.infinite(first.treat.rsa)==F),
                                              i='npi1',
                                              t='year',
                                              g='first.treat.rsa',
                                              y=j,
                                              estimand='cohort'
                                              ) %>%
      mutate(significant=ifelse(abs(estimate)-1.96*se>0, 1, 0),
             sign = ifelse(estimate>0, 'positive', 'negative'),
             model=paste0('r_sa.2023.', i, '.', j, '.overall.trt')
             )
    
    assign(paste0('r_sa.2023.', i, '.', j, '.overall.all'), r_sa.2023.i.j.overall.all)
    assign(paste0('r_sa.2023.', i, '.', j, '.overall.trt'), r_sa.2023.i.j.overall.trtonly)
    
      
  }
}


rm(r_sa.2023.i.j.overall.all, r_sa.2023.i.j.overall.trtonly)

results.rsa.overall.tab <- rbindlist(mget(ls(pattern = 'r_sa.2023.')))

rm(list=ls(pattern='r_sa.2023.(occ|rep|sf|fdr).(hhi|lhhi|n_sources|vol_shared).overall.'))


for (i in timing){
  for (j in outcomes){
    
    r_sa.2023.i.j.estud.all <- staggered(get(paste0('ANSAMP.', i)),
                                         i='npi1',
                                         t='year',
                                         g='first.treat.rsa',
                                         y=j,
                                         estimand='eventstudy',
                                         eventTime = c(-5:5, 1)
    ) %>%
      mutate(significant=ifelse(abs(estimate)-1.96*se>0, 1, 0),
             sign = ifelse(estimate>0, 'positive', 'negative'),
             model=paste0('r_sa.2023.', i, '.', j, '.estud.all')
      )
    
    
    
    r_sa.2023.i.j.estud.trtonly <- staggered(filter(get(paste0('ANSAMP.', i)), 
                                            is.infinite(first.treat.rsa)==F),
                                     i='npi1',
                                     t='year',
                                     g='first.treat.rsa',
                                     y=j,
                                     estimand='eventstudy',
                                     eventTime = c(-5:5, 1)
    ) %>%
      mutate(significant=ifelse(abs(estimate)-1.96*se>0, 1, 0),
             sign = ifelse(estimate>0, 'positive', 'negative'),
             model=paste0('r_sa.2023.', i, '.', j, '.estud.trt')
      )
    
    
    assign(paste0('r_sa.2023.', i, '.', j, '.estud.trt'), r_sa.2023.i.j.estud.trtonly)
    assign(paste0('r_sa.2023.', i, '.', j, '.estud.all'), r_sa.2023.i.j.estud.all)
    
  }
}

rm(r_sa.2023.i.j.estud.all, r_sa.2023.i.j.estud.trtonly)

results.to.get <- ls(pattern='r_sa.2023.(occ|rep|sf|fdr).(hhi|lhhi|n_sources|vol_shared).estud.')

results.rsa.estud.tab <- rbindlist(mget(results.to.get))

rsa.estud.overview <- results.rsa.estud.tab %>%
  group_by(model) %>%
  summarize(n_significant=sum(significant),
            n_significant.pos=sum(significant==1 & sign=='positive'),
            n_significant.neg=sum(significant==1 & sign=='negative')
            )

rm(list=ls(pattern='r_sa.2023.(occ|rep|sf|fdr).(hhi|lhhi|n_sources|vol_shared).estud.'))



# Code below checks through event studies with significant results to see if there
# is anything interesting to report. There is not.

ggplot(	
  # r_sa.2023.fdr.lhhi.estud.trt
) +
  geom_pointrange(aes(x=eventTime,
                      y=estimate,
                      ymin=estimate-1.96*se,
                      ymax=estimate+1.96*se
                      )
                  ) +
  geom_hline(yintercept = 0)


save(results.cs.overall.tab,
     results.rsa.overall.tab,
     results.att.gt,
     results.rsa.estud.tab,
     file='_DATA/results.overall.RData')
