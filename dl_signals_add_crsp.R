# Andrew 2020 04

# creates what you need to do machine learning style stuff
# downloads downloadable wide signals
# downloads crsp predictors
# merges together and outputs

# takes about 5 minutes

# creates:
#   temp/signed_predictors_all_wide.csv
#   temp/SignalDocumentation.xlsx



# ==== ENVIRONMENT ====
rm(list = ls())
tic = Sys.time()

library(tidyverse)
library(googledrive)
library(data.table)
library(fst)
library(getPass)
library(RPostgres)

  # root of April 2021 release on Gdrive
  # pathRelease = 'https://drive.google.com/drive/folders/1I6nMmo8k_zGCcp9tUvmMedKTAkb9734R'

  # root of March 2022 release on Gdrive
  # pathRelease = 'https://drive.google.com/drive/u/0/folders/1O18scg9iBTiBaDiQFhoGxdn4FdsbMqGo'

# root of August 2023 release on Gdrive
pathRelease = 'https://drive.google.com/drive/u/0/folders/1EP6oEabyZRamveGNyzYU0u6qJ-N43Qfq'

# output path
outpath = 'temp/'

# ## LOGIN TO WRDS
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
pathRelease %>% drive_ls()

dir.create(outpath)

# ==== DOWNLOAD AND PROCESS MONTHLY CRSP STUFF ====


# CRSP monthly
# Follows in part: https://wrds-www.wharton.upenn.edu/pages/support/research-wrds/macros/wrds-macro-crspmerge/
crspm = dbSendQuery(conn = wrds, statement =
                      "select a.permno, a.date, a.ret, a.shrout, a.prc, 
                     b.exchcd,
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
# GHZ cite Johnson and Zhao (2007), Shumway and Warther (1999)
# but the way HXZ does this might be a bit better
crspm2 = crspm %>%
  mutate(
    dlret = ifelse(
      is.na(dlret)
      & (dlstcd == 500 | (dlstcd >=520 & dlstcd <=584))
      & (exchcd == 1 | exchcd == 2)
      , -0.35
      , dlret
    )
    , dlret = ifelse(
      is.na(dlret)
      & (dlstcd == 500 | (dlstcd >=520 & dlstcd <=584))
      & (exchcd == 3)
      , -0.55
      , dlret
    )
    , dlret = ifelse(
      dlret < -1 & !is.na(dlret)
      , -1
      , dlret
    )
    , dlret = ifelse(
      is.na(dlret)
      , 0
      , dlret
    )
    , ret = (1+ret)*(1+dlret)-1
    , ret = ifelse(
      is.na(ret) & ( dlret != 0)
      , dlret
      , ret
    )
  )

# convert ret to pct, other formatting
crspm2 = crspm2 %>%
  mutate(
    ret = 100*ret
    , date = as.Date(date)
    , me = abs(prc) * shrout
    , yyyymm = year(date) * 100 + month(date)
  )


# NOTE THESE ARE SIGNED!
crspmsignal = crspm2 %>% 
  transmute(
    permno
    , yyyymm
    , STreversal = -1*if_else(is.na(ret), 0, ret)
    , Price = -1*log(abs(prc))
    , Size = -1*log(me)
  ) 

# ==== DOWNLOAD AND READ IN BIG ZIP ====
# 2 gig dl, can take a few minutes

# download
target_dribble = pathRelease %>% drive_ls() %>% 
    filter(name == 'Firm Level Characteristics') %>% drive_ls() %>% 
    filter(name == 'Full Sets') %>% drive_ls() %>% 
    filter(name == 'signed_predictors_dl_wide.zip') 
dl = drive_download(target_dribble, path = paste0(outpath,'deleteme.zip'), overwrite = T)

# unzip, read, clean up
unzip(paste0(outpath,'deleteme.zip'), exdir = gsub('/$', '', outpath))
wide_dl_raw = fread(paste0(outpath, 'signed_predictors_dl_wide.csv'))
file.remove(paste0(outpath, 'signed_predictors_dl_wide.csv'))

# ==== MERGE AND EXPORT CSV ====

signalwide = full_join(
  wide_dl_raw
  , crspmsignal
  , by = c('permno','yyyymm')
)


fwrite(
  signalwide
  , file = paste0(outpath, 'signed_predictors_all_wide.csv')
  , row.names = F
)

gc() # clean up ram

# ==== SUMMARIZE WHAT YOU GOT ====

# copied from CrossSection/Shipping/Code/3_check_storage.r

## broad overview ====
obs = signalwide %>%
  select(-permno) %>%
  group_by(yyyymm) %>%
  summarize_all(funs(sum(!is.na(.))))

widesum = obs %>% pivot_longer(
  -yyyymm
  , names_to = 'signalname'
  , values_to = 'obs'
) %>%
  filter(obs >= 1) %>%
  group_by(signalname) %>%
  summarize(
    date_begin = min(yyyymm)
    , date_end = max(yyyymm)
    , mean_firmobs_per_month = floor(mean(obs))
  ) %>% as.data.frame()


print(paste0(
  'In ', outpath, 'signal_predictors_all_wide.csv you have the following signals'
))

widesum %>% setDT()
widesum %>% print(topn = 10)

## overview of recent data ====

datelist = unique(signalwide$yyyymm) %>% sort(decreasing = T)
datelist = datelist[1:24]


recent_nobs = signalwide %>% 
  select(-permno) %>% 
  filter(yyyymm %in% datelist) %>% 
  group_by(yyyymm) %>% 
  summarize_all(funs(sum(!is.na(.)))) %>% 
  t()


print('Number of firms with data in recent months')
print(recent_nobs[1:10 , 1:12])
print('...')
print(recent_nobs[c(1, nrow(recent_nobs) - 0:10), 1:12])
