# 2021 03 Andrew Chen: plot selected anomal(ies)


# ==== ENVIRONMENT ====

rm(list = ls())
library(tidyverse)
library(data.table)
library(googledrive)
library(readxl)
library(RColorBrewer)
library(lubridate)

### USER ENTRY
# first signal will be the focus
# use 'mkt_rf' to compare with market excess return (CRSPVW)
signallist = c('TrendFactor','mkt_rf')
# signallist = c('IndIPO','CompEquIss','ShareIss1Y')
years_presamp = 15 # for x axis limits


# root of April 2021 release on Gdrive
# pathRelease = 'https://drive.google.com/drive/folders/1I6nMmo8k_zGCcp9tUvmMedKTAkb9734R'

# root of March 2022 release on Gdrive
pathRelease = 'https://drive.google.com/drive/folders/1O18scg9iBTiBaDiQFhoGxdn4FdsbMqGo'

# login to gdrive
# this prompts a login
pathRelease %>% drive_ls()


# ==== DOWNLOAD DATA =====

## download signal documentation and show user
#   first try xlsx
target_dribble = pathRelease %>% drive_ls() %>% 
  filter(name=='SignalDocumentation.xlsx')

if (nrow(target_dribble) > 0){
  drive_download(target_dribble, path = 'temp/deleteme.xlsx', overwrite = T)
  
  signaldoc = left_join(
    read_excel('temp/deleteme.xlsx',sheet = 'BasicInfo')  
    , read_excel('temp/deleteme.xlsx',sheet = 'AddInfo')
  ) 
  
} else {
  #   then try csv (used as of March 2021)
  target_dribble = pathRelease %>% drive_ls() %>% 
    filter(name=='SignalDoc.csv')
  
  drive_download(target_dribble, path = 'temp/deleteme.csv', overwrite = T)
  
  signaldoc = fread('temp/deleteme.csv')
    
} # end if nrow(target_dribble)

signaldoc = signaldoc %>% 
  mutate(
    signalname = Acronym
    , pubdate = as.Date(paste0(Year, '-12-31'))
    , sampend = as.Date(paste0(SampleEndYear, '-12-31'))
  ) %>% 
  arrange(signalname)


print('Here are some signals you can choose from')
signaldoc %>% select(signalname,Authors,LongDescription) %>% print(n=20)


# ==== IMPORT DATA ====

signaldribble = signallist[signallist != 'mkt_rf'] # mkt_rf is dl from ff, not here

doctarget = signaldoc %>% filter(signalname == signaldribble[1])

# download
csvlist = signaldribble %>% paste0('.csv')
target_dribble = pathRelease %>% drive_ls() %>% 
  filter(name=='Portfolios') %>%  drive_ls() %>% 
  filter(name=='Individual') %>%  drive_ls() %>% 
  filter(name=='Original_Cuts') %>%  drive_ls() %>% 
  filter(
    name %in% csvlist
  )

port = tibble()
for (i in 1:length(csvlist)){
  drive_download(target_dribble[i,], path = 'temp/temp.csv', overwrite = T)
  port = rbind(
    port
    ,fread('temp/temp.csv') %>% 
    transmute(date, ret = portLS, signalname = signallist[i])
  )
}

# ==== ADD MARKET BENCHMARK ====
ffweb = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip'
download.file(ffweb,'temp/deleteme.zip')
unzip('temp/deleteme.zip', exdir = 'temp')

ff = read.csv('temp/F-F_Research_Data_Factors.csv', skip=3, nrows = 1141 - 3 - 1) %>% 
  as_tibble() %>% 
  mutate_all(funs(as.numeric)) %>% 
  transmute(
    yearm = X, ret = Mkt.RF, signalname = 'mkt_rf'
  )

ff = ff %>% 
  left_join(
    port %>% transmute(date, yearm = year(date)*100+month(date))
  ) %>% 
  filter(!is.na(date)) %>% 
  select(date, ret, signalname)

port = rbind(port,ff)

# ==== PLOT ====

## standardize and accumulate
plotme = port %>%
  filter(
    date >= doctarget$sampend - years(years_presamp)
    , !is.na(ret)
  ) 
plotme = plotme %>% 
  arrange(signalname,date) %>%
  group_by(signalname) %>% 
  mutate(cret = cumprod(1+ret/100)) %>%
  ungroup %>% 
  mutate(signalname = factor(signalname, levels = signallist))


yloc = (max(plotme$cret)-1)*0.75
papername = paste0(doctarget$Authors, ' ', doctarget$Year, ' (', signallist[1], ') ')

ggplot(plotme, aes(x=date,y=cret)) +
  geom_line(aes(linetype = signalname, color = signalname), size=1.2) + 
  xlab("") + ylab('Cummulative Long-Short Return') +
  geom_vline(xintercept = doctarget$pubdate, color = 'red') +
  geom_text(
    aes(x=doctarget$pubdate, label=paste0("\n ", papername, ' Published'), y=yloc)
        , colour="red", angle=90) +
  geom_vline(xintercept = doctarget$sampend, color = 'blue') +
  geom_text(
    aes(x=doctarget$sampend, label=paste0("\n ", papername, ' Sample Ends'), y=yloc)
    , colour="blue", angle=90) + 
  theme_minimal(base_size = 10) +
  theme(legend.title = element_blank())  +  
  scale_color_brewer(palette = 'Dark2')


ggsave('temp/plot_anomaly.png', width = 6, height = 4)

# ==== SUMMARY STATS ====


# check summary stats
port %>% 
  group_by(signalname) %>% 
  summarize( mean(ret, na.rm=T), sd(ret, na.rm=T), n(), na.rm=T ) %>% 
  print(n=100)
