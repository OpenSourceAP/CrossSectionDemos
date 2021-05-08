# compare to Ken French's data
# Andrew Chen 2021 04

# ENVIRONMENT ====
rm(list = ls())
library(tidyverse)
library(data.table)
library(googledrive)

dir.create('temp/')

# root of April 2021 release on Gdrive
pathRelease = 'https://drive.google.com/drive/folders/1I6nMmo8k_zGCcp9tUvmMedKTAkb9734R'
url_prefix = 'https://drive.google.com/uc?export=download&id='


# login to gdrive
# this prompts a login
pathRelease %>% drive_ls()

# DOWNLOAD AND PROCESS French data (ff) ====
## download monthly
ffweb = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_BE-ME_CSV.zip'
download.file(ffweb,'temp/deleteme.zip')
unzip('temp/deleteme.zip', exdir = 'temp')

ffweb2 = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip'
download.file(ffweb2,'temp/deleteme.zip')
unzip('temp/deleteme.zip', exdir = 'temp')

# Equal Weight returns begin on line 1165 
ff1 = read.csv('temp/Portfolios_Formed_on_BE-ME.csv', skip=1164, nrows = 2302 - 1164 - 1) %>% 
  as_tibble() %>% 
  mutate_all(funs(as.numeric)) %>% 
  transmute(
    yearm = X, ret = Hi.20 - Lo.20, port = 'ff_ew5'
  )

# VW returns
ff2 = read.csv('temp/Portfolios_Formed_on_BE-ME.csv', skip=23, nrows = 1161 - 23 - 1) %>% 
  as_tibble() %>% 
  mutate_all(funs(as.numeric)) %>% 
  transmute(
    yearm = X, ret = Hi.20 - Lo.20, port = 'ff_vw5'
  ) 

# HML
ff3 = read.csv('temp/F-F_Research_Data_Factors.csv', skip=3, nrows = 1141 - 3 - 1) %>% 
  as_tibble() %>% 
  mutate_all(funs(as.numeric)) %>% 
  transmute(
    yearm = X, ret = HML, port = 'ff_hml'
  )

ff = rbind(ff1,ff2,ff3)

# DOWNLOAD AND PROCESS OPENAP DATA (cz)====

# download
signallist = c('BM','BMdec')
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

cz = port %>% 
  transmute(
    yearm = year(as.Date(date))*100 + month(as.Date(date))
    , ret
    , port = signalname
  ) %>% 
  as_tibble()


# MERGE AND CHECK ====
both = rbind(ff,cz) 


info = data.frame(
  levels      = c('ff_hml', 'ff_vw5', 'ff_ew5', 'BMdec', 'BM')
  , labels    = c('Ken French HML (FF93)', 'Ken French VW', 'Ken French EW','Open AP BMdec (a la FF92)','Open AP BM (a la RRL85)')
  , colors    = c('gray', 'black','blue','red','magenta')
  , linetypes = c('solid', 'solid','solid','twodash','dotted')
  , rank      = c(3,4,5,1,2)
) %>% arrange(rank)

plotme = both %>% 
  mutate(
    date = as.Date(as.character(yearm*100+28), '%Y%m%d')
  ) %>% 
  mutate(
    port = factor(port, levels = info$levels, labels = info$labels)
  )  

# plot 
datebegin = as.Date('2019-01-01')
ggplot(
  plotme %>% 
    filter(date >= datebegin, date <= '2020-12-31') %>% 
    group_by(port) %>% arrange(date) %>% 
    mutate(
      ret = if_else(abs(date - datebegin) <= 31 , 0, ret)
      , cret = 100*(cumprod(1+ret/100) - 1)
    )
  , aes(x=date, y=cret, group=port)) +
  geom_line(aes(linetype = port, color = port), size = 0.75) +
  ylab('Cumulative Return on Value (%)')+
  ggtitle('Open Asset Pricing vs Ken French') +
  theme_minimal(base_size = 18) +
  theme(
    legend.title = element_blank()
    , legend.position = c(0.25, 0.25)
    , legend.background = element_rect(fill='white')
  )  +
  scale_color_manual(
    breaks = info$labels
    , values= info$colors
  ) +
  scale_linetype_manual(
    values = info$linetypes
  ) 

temp = 1
ggsave(filename = paste0("temp/openap_vs_french_close.png")
       , width = 10*temp, height = 6*temp)


