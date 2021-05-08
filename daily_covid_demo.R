# plot anomaly returns during the Covid crash.  Andrew Chen 2021 04

# ==== ENVIRONMENT ====
rm(list = ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(googledrive)

# root of April 2021 release on Gdrive
pathRelease = 'https://drive.google.com/drive/folders/1I6nMmo8k_zGCcp9tUvmMedKTAkb9734R'
url_prefix = 'https://drive.google.com/uc?export=download&id='


# login to gdrive
# this prompts a login
target_dribble = pathRelease %>% drive_ls() %>% 
  filter(name=='DailyPortfolios') %>%  drive_ls() %>% 
  filter(name=='PredictorVW.zip') 

# temp folder for output and working space
dir.create('temp/')

# ==== IMPORT DATA ====

## OPEN AP DATA

# download, unzip
# this is a pretty big file, can take a minute
drive_download(
    as_id(target_dribble$id), path = 'temp/deleteme.zip', overwrite = T
)
unzip('temp/deleteme.zip', exdir = 'temp')


# import all csvs into raw, but only a subset of data for speed
csvlist = list.files('temp/PredictorVW/', pattern = '.csv')
signallist = csvlist %>% str_remove('_ret.csv') %>% str_remove('temp/PredictorVW/')

raw = data.frame()
for (i in 1:length(csvlist)){
  temp = fread(paste0('temp/PredictorVW/', csvlist[i])) %>%  
                 select(date,portLS)  %>% 
                mutate(signalname = signallist[i]) %>%
          filter(date >= '2019-01-01', !is.na(portLS))
  raw = rbind(raw,temp)
}

all = raw %>%  
transmute(
  date = as.Date(date)
  , ret = portLS
  , port = signalname
) %>% 
  filter(!is.na(ret))

agg = all %>% 
  group_by(date) %>% 
  summarize(ret = mean(ret)) %>% 
  mutate(port = 'mean_anomaly')

all = rbind(agg,all)

## KEN FRENCH DATA DAILY
download.file(
  "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
  ,'temp/deleteme.zip'
)
unzip('temp/deleteme.zip', exdir = 'temp')

ffd = read.csv('temp/F-F_Research_Data_Factors_daily.CSV', skip=4) %>%
  rename(date = X) %>%
  as_tibble() %>%
  mutate_all(list(as.numeric)) %>%
  gather(-date,key='port',value='ret') %>%
  filter(!is.na(ret)) %>% 
  mutate(date = as.Date(as.character(date), "%Y%m%d")) 

all = rbind(all,ffd)


# ==== PLOT SELECTED ====

signallist =c(
  'Mkt.RF'
  , 'BMdec'
  , 'EarningsSurprise'
  , 'OPLeverage'
)
labellist = c(
  'CRSP Index'
  ,'Book / Market'  
  ,'Earnings Surprise'
  , 'Operating Leverage'
)
colorlist = c(
  'grey'
  , 'red'
  , 'blue'
  , 'green'
  , 'magenta'
)
sizelist = c(
  1.5, 1, 1, 1, 1, 1
)

small = all %>%   
  filter(port %in% signallist) %>% 
  filter(date >= '2020-02-01') %>%   
  filter(date <= '2020-05-30') %>%     
  group_by(port) %>% 
  mutate(cret = cumprod(1+ret/100)-1, cret = cret*100) %>% 
  mutate(
    port = factor(port, levels = signallist, labels = labellist)
  )

# grouped plot
ggplot(small, aes(x=date,y=cret)) +
  geom_line(aes(color = port, size=port)) +
  xlab("2020") + ylab('VW Long-Short Cum Return (%)')  +
  theme_minimal(base_size = 18) +
  scale_color_manual(breaks = labellist, values= colorlist)  +
  scale_size_manual(breaks = labellist, values= sizelist) +
  theme(legend.title = element_blank())

temp = 1
ggsave(filename = paste0("temp/covid_anom_select.png")
       , width = 10*temp, height = 6*temp)

# ==== PLOT ALL VS VW ====

plotme = all %>% 
  filter(date >= '2020-01-01') %>%   
  filter(date <= '2020-05-30') %>%       
  group_by(port) %>% 
  mutate(cret = cumprod(1+ret/100)-1, cret = cret*100) 


plotme2 = all %>% 
  filter(
    port %in% 'mean_anomaly'
  ) %>% 
  filter(date >= '2020-01-01') %>%   
  filter(date <= '2020-05-30') %>%         
  group_by(port) %>% 
  mutate(cret = cumprod(1+ret/100)-1, cret = cret*100)  

plotme3 = all %>% 
  filter(
    port %in% 'Mkt.RF'
  ) %>% 
  filter(date >= '2020-01-01') %>%   
  filter(date <= '2020-05-30') %>%         
  group_by(port) %>% 
  mutate(cret = cumprod(1+ret/100)-1, cret = cret*100) 


ggplot(plotme, aes(x=date,y=cret)) +
  geom_line(aes(group = port, color='a')) +
  geom_line(data=plotme2, aes(color = 'b'), size = 1.5) +  
  geom_line(data=plotme3, aes(color = 'c'), size = 1.5) +    
  scale_color_manual(name = 'data source', 
                   values =c(
                     'a'='grey'
                     ,'b'='blue'
                     ,'c'='black'                     
                     ), 
                   labels = c(
                     '193 Anomalies'
                     ,'Average Anomaly'                     
                     ,'CRSP VW'
                     )) +
  xlab("2020") + ylab('VW Long-Short Cum Return (%)')+
  theme_minimal(base_size = 18) +
  theme(legend.title = element_blank())  +
  ylim(-50,50)

temp = 1
ggsave(filename = paste0("temp/covid_anom_all.png")
       , width = 10*temp, height = 6*temp)


