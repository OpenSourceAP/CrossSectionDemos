# for making FF1993 style factors from individual csvs on gdrive
# andrew 2021 05

# FF1993 style is based on WRDS:
# https://wrds-www.wharton.upenn.edu/pages/support/applications/risk-factors-and-industry-benchmarks/fama-french-factors/

# ==== ENVIRONMENT ====

library(tidyverse)
library(data.table)
library(googledrive)
library(getPass)
library(RPostgres)

# name signal to make factor from
signalname = 'BMdec'

# root of April 2021 release on Gdrive
pathRelease = 'https://drive.google.com/drive/folders/1I6nMmo8k_zGCcp9tUvmMedKTAkb9734R'
url_prefix = 'https://drive.google.com/uc?export=download&id='

# login to wrds
user = getPass('wrds username: ')
pass = getPass('wrds password: ')

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  user=user,
                  password=pass,
                  sslmode='require')


# login to gdrive
# this prompts a login
target_dribble = pathRelease %>% drive_ls() %>% 
  filter(name=='Firm Level Characteristics') %>%  drive_ls() %>% 
  filter(name=='Individual') %>%  drive_ls() %>% 
  filter(name=='Predictors') %>%  drive_ls() %>% 
  filter(name==paste0(signalname,'.csv'))


# ==== DOWNLOAD DATA ====

# import signal from open ap gdrive 
signal = fread(
  paste0(url_prefix, target_dribble$id)
) %>% 
  rename(signal =!!signalname )


# CRSP monthly
# Follows in part: https://wrds-www.wharton.upenn.edu/pages/support/research-wrds/macros/wrds-macro-crspmerge/
crspraw = dbSendQuery(conn = wrds, statement =
                      "select a.permno, a.permco, a.date, a.ret, a.retx, a.vol, a.shrout, a.prc, a.cfacshr, a.bidlo, a.askhi,
                     b.shrcd, b.exchcd, b.siccd, b.ticker, b.shrcls,  -- from identifying info table
                     c.dlstcd, c.dlret                                -- from delistings table
                     from crsp.msf as a
                     left join crsp.msenames as b
                     on a.permno=b.permno
                     and b.namedt<=a.date
                     and a.date<=b.nameendt
                     left join crsp.msedelist as c
                     on a.permno=c.permno
                     and date_trunc('month', a.date) = date_trunc('month', c.dlstdt)
                     "
) %>%
  # Pull data
  dbFetch(n = -1) %>%
  setDT()


# incorporate delisting return
# follows WRDS
crsp = crspraw %>%
  mutate(
    dlret = if_else(is.na(dlret),0,dlret)
    , ret = ret + dlret
    , ret = ifelse(
      is.na(ret) & ( dlret != 0)
      , dlret
      , ret
    )
  ) %>% 
# convert ret to pct, other formatting
  mutate(
    ret = 100*ret
    , date = as.Date(date)
    , me = abs(prc) * shrout
    , yyyymm = year(date) * 100 + month(date)
  ) 

# ==== ASSIGN TO 2X3 PORTFOLIOS ====

# full join to keep as many me obs as possible, ff1993, page 8
signaljune = signal%>% 
  filter(yyyymm %% 100 == 6) %>% 
  full_join(
    crsp %>% 
      filter(yyyymm %% 100 == 6) %>%  
      select(permno,yyyymm,me,exchcd,shrcd)
    , by = c('permno','yyyymm')
  )


# FF93 is unclear about the shrcd screen here, but
# WRDS does it
nysebreaks = signaljune %>% 
  filter(exchcd==1, shrcd %in% c(10,11)) %>% 
  group_by(yyyymm) %>% 
  summarise(
    qsignal_l = quantile(signal,0.3, na.rm=T)
    , qsignal_h = quantile(signal,0.7, na.rm=T)
    , qme_mid = quantile(me,0.5, na.rm=T)
  )


# only exchcd in (1,2,3), shrcd in (10,11), ff93 p8-9
# we already exclude negative BE
port6 = signaljune %>% 
  filter(
    exchcd %in% c(1,2,3), shrcd %in% c(10,11)
  ) %>% 
  left_join(nysebreaks, by=c('yyyymm')) %>% 
  mutate(
    q_signal = case_when(
      signal <= qsignal_l ~ 'L'
      , signal <= qsignal_h ~ 'M'
      , signal > qsignal_h ~ 'H'
    )
    , q_me = case_when(
      me <= qme_mid ~ 'S'
      , me > qme_mid ~ 'B'
    )
    , port6 = paste0(q_me,q_signal)
  ) %>% 
  select(
    permno, yyyymm, port6
  )

# ==== FIND MONTHLY FACTOR RETURNS ====


# find VW returns for port6
port6ret = crsp %>% 
  # merge annual port6 onto returns
  filter(floor(yyyymm/100)>=1963) %>% 
  select(permno,yyyymm,ret,me) %>% 
  left_join(port6, by=c('permno','yyyymm')) %>% 
  # fill and lag 
  group_by(permno) %>% 
  arrange(permno,yyyymm) %>% 
  fill(port6) %>% 
  mutate(
    port6lag = lag(port6)
    , melag = lag(me)
  ) %>% 
  filter(!is.na(melag)) %>% 
  # find value-weighted returns by port6lag month
  group_by(port6lag, yyyymm) %>% 
  summarize(
    ret_vw = weighted.mean(ret,melag,na.rm=T)
  ) 
  
  
# equal weight extreme portfolios to make FF1993-style factor
ff93style = port6ret %>% 
  pivot_wider(
    names_from = port6lag, values_from = ret_vw
  ) %>% 
  mutate(
    factor_ret = 0.5*(SH+BH) - 0.5*(SL+BL)
  ) %>% 
  select(yyyymm, factor_ret)  


# ==== COMPARE TO KEN FRENCH ====

ffweb2 = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip'
download.file(ffweb2,'temp/deleteme.zip')
unzip('temp/deleteme.zip', exdir = 'temp')

# HML
french = read.csv('temp/F-F_Research_Data_Factors.csv', skip=3, nrows = 1141 - 3 - 1) %>% 
  as_tibble() %>% 
  mutate_all(funs(as.numeric)) %>% 
  transmute(
    yyyymm = X, HML_french = HML
  )


plotme = ff93style %>% 
  left_join(french, by = c('yyyymm')) %>%
  pivot_longer(
    cols = c('factor_ret','HML_french')
    , names_to = 'dataset'
    , values_to = 'ret'
  ) %>% 
  mutate(
    date = as.Date(as.character(yyyymm*100+28), '%Y%m%d')
    , dataset = if_else(dataset=='factor_ret','HML_openap',dataset)
  )


# plot close up returns
datebegin = as.Date('2010-01-01')
ggplot(
  plotme %>% 
    filter(date >= datebegin, date <= '2020-12-31') %>% 
    group_by(dataset) %>% arrange(date) %>% 
    mutate(
      ret = if_else(abs(date - datebegin) <= 31 , 0, ret)
      , cret = cumprod(1+ret/100) - 1
    )
  , aes(x=date, y=ret, group=dataset)) +
  geom_line(aes(linetype = dataset, color = dataset), size = 0.75) +
  ylab('Return (%)')+
  theme_minimal(base_size = 18) +
  theme(
    legend.title = element_blank()
    , legend.position = c(0.25, 0.25)    
    , legend.background = element_rect(fill='white')
  ) 



temp = 1
ggsave(filename = paste0("temp/openap_vs_french_hml.png")
       , width = 10*temp, height = 6*temp)
