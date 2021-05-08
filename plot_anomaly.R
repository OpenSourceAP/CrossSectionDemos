# 2021 03 Andrew Chen: plot selected anomal(ies)


# ==== ENVIRONMENT ====

rm(list = ls())
library(tidyverse)
library(data.table)
library(googledrive)
library(readxl)
library(RColorBrewer)

### USER ENTRY
# first signal will be the focus
signallist = c('IndIPO')
# signallist = c('IndIPO','CompEquIss','ShareIss1Y')
years_presamp = 15 # for x axis limits


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
  ) %>% 
  arrange(signalname)

print('Here are some signals you can choose from')
signaldoc %>% select(signalname,Authors,LongDescription) %>% print(n=20)


# ==== IMPORT DATA ====

doctarget = signaldoc %>% filter(signalname == signallist[1])

# download
csvlist = signallist %>% paste0('.csv')
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


ggsave('temp/plot_anomaly.pdf', width = 6, height = 4)

