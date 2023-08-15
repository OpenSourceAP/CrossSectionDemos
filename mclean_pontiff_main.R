# 2021 12 Andrew Chen: replicate the main result of McLean-Pontiff 

# ENVIRONMENT ====

rm(list = ls())
library(tidyverse)
library(data.table)
library(googledrive)
library(readxl)
library(RColorBrewer)
library(lubridate)

### USER ENTRY

# root of March 2022 release
pathRelease = 'https://drive.google.com/drive/folders/1O18scg9iBTiBaDiQFhoGxdn4FdsbMqGo'

# root of August 2023 
pathRelease = 'https://drive.google.com/drive/u/0/folders/1EP6oEabyZRamveGNyzYU0u6qJ-N43Qfq'

# login to gdrive
# this prompts a login
pathRelease %>% drive_ls()

# create temporary directory 
dir.create('temp/')

# DOWNLOAD DATA =====

## download signal documentation and show user
target_dribble = pathRelease %>% drive_ls() %>% 
  filter(name=='SignalDoc.csv')

drive_download(target_dribble, path = 'temp/deleteme.csv', overwrite = T)

signaldoc = fread('temp/deleteme.csv') %>% 
  mutate(
    signalname = Acronym
    , pubdate = as.Date(paste0(Year, '-12-31'))
    , sampend = as.Date(paste0(SampleEndYear, '-12-31'))
    , sampstart = as.Date(paste0(SampleStartYear, '-01-01'))
  ) %>% 
  arrange(signalname) %>% 
  select(signalname, pubdate, sampend, sampstart)

## download all long-short returns (OP) ====
target_dribble = pathRelease %>% drive_ls() %>% 
  filter(name=='Portfolios') %>% drive_ls() %>% 
  filter(name=='Full Sets OP') %>% drive_ls() %>% 
  filter(name=='PredictorLSretWide.csv')

drive_download(target_dribble[1,], path = 'temp/deleteme.csv', overwrite = T)

ret0 = fread('temp/deleteme.csv') %>% 
  pivot_longer(-c(date),names_to = 'signalname', values_to = 'ret') %>% 
  filter(!is.na(ret)) 


# MERGE AND FIND OUT OF SAMPLE RETURNS ====
ret1 = ret0 %>% 
  left_join(signaldoc) %>% 
  mutate(
    samptype = case_when(
      (date >= sampstart) & (date <= sampend) ~ 'in-samp'
      , (date > sampend) & (date <= pubdate) ~ 'out-of-samp'
      , (date > pubdate) ~ 'post-pub'
      , TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(samptype))

sumsignal = ret1 %>%   
  group_by(signalname, samptype) %>% 
  summarize(
    rbar = mean(ret)
    , tstat = rbar/sd(ret)*sqrt(n())
  ) 

# remove bad reproductions / bad predictors (if desired)
signalok = sumsignal %>% 
  filter(samptype=='in-samp') %>% 
  filter(tstat > -Inf) %>% 
  transmute(signalname)
sumsignal = sumsignal %>% inner_join(signalok)

# check out of sample decay simple way ====
sumsamp = sumsignal %>% 
  group_by(samptype) %>% 
  summarize(rbar = mean(rbar), nsignal = n())


baseline = sumsamp %>% filter(samptype == 'in-samp') %>% select(rbar) %>% as.numeric()

sumsamp = sumsamp %>% 
  mutate(
    decay = (baseline - rbar)/baseline
  )

sumsamp


# BOOTSTRAP MEAN DISTRIBUTIONS ====
# clustered by month

set.seed(6)

nboot = 200

bootfun = function(sampname){

  # make wide dataset, use NA if not correct sample
  wide_is = ret1 %>%
    filter(samptype == sampname) %>% 
    select(signalname, date, ret) %>% 
    pivot_wider(
      names_from = signalname, values_from = ret
    ) %>% 
    select(-date) %>% 
    as.matrix()
  
  # make array that only has enough signals in each month (10)
  tgood = rowSums(!is.na(wide_is), na.rm=T) > 10
  mat = wide_is[tgood, ]
  nmonth = dim(mat)[1]
  
  # bootstrap pooled mean
  rboot = rep(NA_real_, nboot)
  for (i in 1:nboot){
    tempt = sample(1:nmonth, replace = T)
    rboot[i] = mat[tempt,]  %>% as.vector %>% mean(na.rm=T)
  }
  
  return(rboot)

} # end bootfun


# bootstrap for each sample type
rboot1 = bootfun('in-samp')
rboot2 = bootfun('out-of-samp')

# compile and plot
bootdat = data.frame(
  pooled_mean_ret = rboot1, samptype = 'in-samp' 
) %>% 
  rbind(
    data.frame(
      pooled_mean_ret = rboot2, samptype = 'out-of-samp' 
    )
  )


bootdat  %>% 
  ggplot(aes(x=pooled_mean_ret, fill=samptype)) +
  geom_histogram(
    alpha = 0.6, position = 'identity', breaks = seq(0,1,0.025), aes(y=..density..)
  ) +
  ggtitle('bootstrapped distribution') +
  labs(x='pooled mean return (% monthly)') +
  geom_vline(xintercept = 0)


