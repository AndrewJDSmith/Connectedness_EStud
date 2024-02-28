rm(list=ls())

library(tidyverse)
library(did)
library(staggered)
library(data.table)


# Overall conclusion for GENSURG: Nothing here, folks

ANSAMP.GENSURG.occ <- read_csv('_DATA/ANSAMP.occ.csv') %>%
  filter(spec_prim_1_1=='02') %>%
  mutate(
         first.treat.rsa = ifelse(first.treat==0, Inf, first.treat)
         )
ANSAMP.GENSURG.rep <- read_csv('_DATA/ANSAMP.rep.csv') %>%
  filter(spec_prim_1_1=='02') %>%
  mutate(
         first.treat.rsa = ifelse(first.treat==0, Inf, first.treat)
         )
ANSAMP.GENSURG.sf <- read_csv('_DATA/ANSAMP.sf.csv') %>%
  filter(spec_prim_1_1=='02') %>%
  mutate(
         first.treat.rsa = ifelse(first.treat==0, Inf, first.treat)
         )
ANSAMP.GENSURG.fdr <- read_csv('_DATA/ANSAMP.fdr.csv') %>%
  filter(spec_prim_1_1=='02') %>%
  mutate(
         first.treat.rsa = ifelse(first.treat==0, Inf, first.treat)
         )




ggplot() +
  geom_histogram(data=filter(ANSAMP.GENSURG.rep, treated==0),
                 aes(x=pat.vol_l2y_1_pre.samp, y = stat(count / sum(count))),
                 color='blue') +
  geom_histogram(data=filter(ANSAMP.GENSURG.rep, treated==1),
                 aes(x=pat.vol_l2y_1_pre.samp, y = stat(count / sum(count))),
                 color='red')


ggplot() +
  geom_histogram(data=filter(ANSAMP.GENSURG.rep, treated==0),
                 aes(x=PSIAny_rate_l2y_1_pre.samp, y = stat(count / sum(count))),
                 color='blue') +
  geom_histogram(data=filter(ANSAMP.GENSURG.rep, treated==1),
                 aes(x=PSIAny_rate_l2y_1_pre.samp, y = stat(count / sum(count))),
                 color='red')



timing <- c('occ', 'rep', 'sf', 'fdr')
outcomes <- c('n_sources', 'vol_shared', 'hhi', 'lhhi')

xformula.tiv <- ~ pat.vol_l2y_1_pre.samp + PSIAny_rate_l2y_1_pre.samp
xformula.tv <- ~ pat.vol_l2y_1 + PSIAny_rate_l2y_1



for (i in timing){
  for (j in outcomes){
    
    att.i.j.uncond <- att_gt(yname=j,
                             tname='year',
                             idname='npi1',
                             gname='first.treat',
                             xformla=NULL,
                             data=get(paste0('ANSAMP.GENSURG.', i))
                             )
    
    att.i.j.cond.tiv <- att_gt(yname=j,
                               tname='year',
                               idname='npi1',
                               gname='first.treat',
                               xformla=xformula.tiv,
                               data=get(paste0('ANSAMP.GENSURG.', i))
                               )
    
    att.i.j.cond.tv <- att_gt(yname=j,
                              tname='year',
                              idname='npi1',
                              gname='first.treat',
                              xformla=xformula.tv,
                              data=get(paste0('ANSAMP.GENSURG.', i))
                              )
    
  
  
    assign(paste0('att.gt.', i, '.', j, '.uncond'), att.i.j.uncond)
    assign(paste0('att.gt.', i, '.', j, '.cond.tiv'), att.i.j.cond.tiv)
    assign(paste0('att.gt.', i, '.', j, '.cond.tv'), att.i.j.cond.tv)
    
  }
}

rm(att.i.j.cond.tiv, att.i.j.cond.tv, att.i.j.uncond)

results.att.gt.GENSURG <- mget(ls(pattern='att.gt.'))

for (i in 1:length(results.att.gt.GENSURG)) {
  
  a <- aggte(results.att.gt.GENSURG[[i]], type='group')
  b <- a$overall.att
  c <- a$overall.se
  
  d <- data.frame(estimate=b, se=c) %>%
    mutate(lower=estimate-1.96*se,
           upper=estimate+1.96*se,
           significant=ifelse(abs(estimate)-1.96*se>0, 1, 0),
           sign = ifelse(estimate>0, 'positive', 'negative'),
           model=names(results.att.gt.GENSURG)[[i]]
           )
  
  assign(paste0('results.overall.att.gt.', names(results.att.gt.GENSURG)[[i]]), d)
  
  a.p <- aggte(results.att.gt.GENSURG[[i]], type='dynamic') 
  
  e <- a.p$att.egt
  f <- a.p$se.egt
  g <- a.p$crit.val.egt
  h <- data.frame(estimate=e, se=f) %>%
    mutate(lower=estimate-g*se,
           upper=estimate+g*se,
           significant=ifelse(abs(estimate)-g*se>0, 1, 0),
           sign = ifelse(estimate>0, 'positive', 'negative'),
           model=names(results.att.gt.GENSURG)[[i]]
           )
           
           assign(paste0('results.estud.cs.', names(results.att.gt.GENSURG)[[i]]), h)

}


results.cs.overall.GENSURG.tab <- rbindlist(mget(ls(pattern='results.overall.att.gt.')))

results.cs.estud.GENSURG.tab <- rbindlist(mget(ls(pattern='results.estud.cs.')))

rm(list=ls(pattern='results.overall.att.'))
rm(list=ls(pattern='results.estud.cs.att.'))


# Below code shows no CS event studies have more than one significant estimate

cs.estud.overview.GENSURG <- results.cs.estud.GENSURG.tab %>%
  group_by(model) %>%
  summarize(n_significant=sum(significant)) %>%
  arrange(desc(n_significant))



for (i in timing){
  for (j in outcomes){
    
    
    r_sa.2023.i.j.overall.all <- staggered(get(paste0('ANSAMP.GENSURG.', i)),
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
    
    r_sa.2023.i.j.overall.trtonly <- staggered(filter(get(paste0('ANSAMP.GENSURG.', i)), 
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

# Below code shows only no rsa DID estiamte is significant. 

results.rsa.overall.GENSURG.tab <- rbindlist(mget(ls(pattern = 'r_sa.2023.')))

rm(list=ls(pattern='r_sa.2023.(occ|rep|sf|fdr).(hhi|lhhi|n_sources|vol_shared).overall.'))


# sig.rsa.overall.GENSURG <- results.rsa.overall.GENSURG.tab %>%
#   filter(significant==1)

for (i in timing){
  for (j in outcomes){
    
    r_sa.2023.i.j.estud.all <- staggered(get(paste0('ANSAMP.GENSURG.', i)),
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
    
    
    
    r_sa.2023.i.j.estud.trtonly <- staggered(filter(get(paste0('ANSAMP.GENSURG.', i)), 
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


results.rsa.estud.GENSURG.tab <- rbindlist(mget(results.to.get))

rsa.estud.GENSURG.overview <- results.rsa.estud.GENSURG.tab %>%
  group_by(model) %>%
  summarize(n_significant=sum(significant),
            n_significant.pos=sum(significant==1 & sign=='positive'),
            n_significant.neg=sum(significant==1 & sign=='negative')
            ) %>%
  arrange(desc(n_significant))



ggplot(	
  
  r_sa.2023.fdr.hhi.estud.all
) +
  geom_pointrange(aes(x=eventTime,
                      y=estimate,
                      ymin=estimate-1.96*se,
                      ymax=estimate+1.96*se
                      )
                  ) +
  geom_hline(yintercept = 0)


