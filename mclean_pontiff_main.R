# 2021 12 Andrew Chen: replicate the main result of McLean-Pontiff 

# ==== ENVIRONMENT ====

rm(list = ls())
library(tidyverse)
library(data.table)
library(googledrive)
library(readxl)
library(RColorBrewer)
library(lubridate)

### USER ENTRY

# root of April 2021 release on Gdrive
pathRelease = 'https://drive.google.com/drive/folders/1I6nMmo8k_zGCcp9tUvmMedKTAkb9734R'
url_prefix = 'https://drive.google.com/uc?export=download&id='


# login to gdrive
# this prompts a login
pathRelease %>% drive_ls()


# ==== DOWNLOAD DATA =====

## download signal documentation and show user
target_dribble = pathRelease %>% drive_ls() %>% 
  filter(name=='SignalDocumentation.xlsx')

drive_download(target_dribble, path = 'temp/deleteme.xlsx', overwrite = T)

signaldoc = left_join(
  read_excel('temp/deleteme.xlsx',sheet = 'BasicInfo')  
  , read_excel('temp/deleteme.xlsx',sheet = 'AddInfo')
) %>% 
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


# ==== MERGE AND FIND OUT OF SAMPLE RETURNS ====
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

# ==== check out of sample decay simple way ====
sumsamp = sumsignal %>% 
  group_by(samptype) %>% 
  summarize(rbar = mean(rbar))



baseline = sumsamp %>% filter(samptype == 'in-samp') %>% select(rbar) %>% as.numeric()

sumsamp = sumsamp %>% 
  mutate(
    decay = (baseline - rbar)/baseline
  )


sumsamp

# ==== PLOT IN-SAMPLE VS OUT OF SAMPLE ====


sumsignal  %>% 
  filter(samptype != 'post-pub') %>% 
  ggplot(aes(x=rbar, fill=samptype)) +
  geom_histogram(
    alpha = 0.6, position = 'identity', breaks = seq(-2.5,5,0.25), aes(y=..density..)
  ) 

